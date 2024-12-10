#' Call Language Model API
#'
#' This generic function sends prompts to various language model APIs and
#' retrieves responses. It supports multiple APIs, including Groq, OpenAI,
#' Claude (Anthropic), and Gemini.
#'
#' @param prompts A list of prompts to send to the API. The class of this object
#'   should match one of the supported APIs: "groq", "claude" (for the Anthropic
#'   API), "openai", "gemini", or "llamafile" (for local
#'   [llamafiles](https://github.com/mozilla-ocho/llamafile/)). It will
#'   typically be the result of a call to [prompt_list()].
#' @param model A string specifying the model to use. Get available models with
#'   [get_available_models()].
#' @param prompt_name An optional string specifying the type of prompt.
#' @param ... Additional arguments passed to specific `call_api` methods, such
#'   as `max_retries`, `temperature`, `max_tokens`, `json_mode`, `system`,
#'   `pause_cap`, `llamafile_path`, or `log`. See the documentation for the
#'   [openai_single_request()] and [llamafile_single_request()] for more.
#'
#' @return A tibble containing the API responses and usage information.
#' @details This function is implemented as a generic with methods for different
#'   APIs:
#' - call_api.groq
#' - call_api.claude
#' - call_api.openai
#' - call_api.gemini
#' - call_api.mistral
#' - call_api.llamafile
#'
#'   Each method handles API-specific details such as endpoint URLs,
#'   authentication, and response parsing.
#'
#' @seealso [prompt_list()] for creating prompts to use with this
#'   function.
#' @family generic
#' @family multiple requests
#' @export
call_api <- function(prompts, model, prompt_name, ...) {
  UseMethod("call_api", prompts)
}


#' @export
call_api.default <- function(prompts,
                             model,
                             prompt_name,
                             ...) {
  # Undefined vars
  id <- NULL

  # Extract default arguments from ...
  args <- list(...)
  json_mode <- args$json_mode %||% FALSE
  log <- args$log %||% TRUE

  # Validate log - must be a logical scalar
  checkmate::assert_logical(log, len = 1)

  # Validate json_mode - must be a logical scalar
  checkmate::assert_logical(json_mode, len = 1)

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  # Validate arguments, including those in ...
  validate_single_request(prompts, model, prompt_name, ...)

  # Resolve functions using the helper
  api <- class(prompts)[1]
  resolved_functions <- resolve_functions(api, prompt_name)

  single_request_fun <- resolved_functions$single_request_fun
  content_extraction_fun <- resolved_functions$content_extraction_fun

  # Ensure prompt IDs are defined
  ids <- names(prompts)
  if (is.null(ids)) {
    cli::cli_abort("Each prompt must have a unique name. Please name your prompts.")
  }

  # Initialize response collection
  responses <- tibble::tibble()

  # Generate a unique hash for the prompt set
  prompt_set <- digest::digest(ids)

  # Loop over prompts and process each
  for (i in seq_along(prompts)) {
    # Log the progress if enabled
    if (log) {
      log_message <- glue::glue(
        "{lubridate::now()}: Processing prompt {i} of {length(prompts)} ",
        "with model '{model}' ({class(prompts)[1]}) in set '{prompt_set}'"
      )
      message(log_message)
    }

    # Call the single request function
    response <- do.call(single_request_fun, list(
      prompt = prompts[[i]],
      model = model,
      content_extraction_fun = content_extraction_fun,
      ...
    ))

    # Add prompt ID to the response
    response$id <- ids[i]
    responses <- dplyr::bind_rows(responses, response)
  }

  # Add metadata to responses and ensure consistent structure
  responses <- responses |>
    dplyr::mutate(api = class(prompts)[1], model = model) |>
    dplyr::relocate(id, api, model, .before = dplyr::everything())

  return(responses)
}

#' @export
#' @export
call_api.groq <- function(prompts,
                          model,
                          prompt_name,
                          ...) {

  # Set defaults for model if missing
  if (missing(model)) {
    model <- get_default_model("groq")
  }

  # Call the generic NextMethod to handle the request
  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             ...)
}


#' @export
call_api.claude <- function(prompts, model, prompt_name, ...) {

  if (missing(model)) {
    model <- get_default_model(class(prompts)[1])
  }

  NextMethod(prompts,
             model = model,
             prompt_name,
             ...)
}

#' @export
call_api.mistral <- function(prompts, model, prompt_name, ...) {

  if (missing(model)) {
    model <- get_default_model("mistral")
  }

  NextMethod(prompts,
             model = model,
             prompt_name,
             ...)
}


#' @export
call_api.openai <- function(prompts, model, prompt_name, ...) {

  if (missing(model)) {
    model <- get_default_model("openai")
  }

  NextMethod(prompts,
             model = model,
             prompt_name,
             ...)
}

#' @export
call_api.gemini <- function(prompts, model, prompt_name, ...) {

  if (missing(model)) {
    model <- get_default_model("gemini")
  }

  NextMethod(prompts,
             model = model,
             prompt_name,
             ...)
}


#' @export
call_api.llamafile <- function(prompts, model, prompt_name, ...) {
  args <- list(...)
  llamafile_path <- args$llamafile_path
  json_mode <- args$json_mode %||% FALSE

  if(is_llamafile_running()) {
    running_model <- which_llamafile_running()
    if(!missing(model) && model != running_model) {
      cli::cli_abort(
        "Running llamafile model ({running_model}) is not the model demanded ({model}). ",
        "Kill the current instance of {running_model} with {.fn kill_llamafile()} before running {model}.")
    }
    model <- which_llamafile_running()
  } else {
    if(is.null(llamafile_path) && !missing(model)) {
      llamafile_path <- fs::dir_ls(recurse = TRUE, regexp = paste0(model, ".+llamafile"))
      if(length(llamafile_path == 0)) {
        cli::cli_abort("No llamafile models found in this directory or in its subdirectory.")
      }
      if(length(llamafile_path > 1)) {
        cli::cli_abort("Multiple llamafile models found in this directory matching {model}.")
      }
    }
    cli::cli_alert_info("Starting LLamafile at {llamafile_path}.")
    start_llamafile(llamafile_path)
    model <- which_llamafile_running()
  }

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  NextMethod(prompts,
             model = model,
             ...)
}


default_json_content_extraction <- function(json_string) {
  json_string |>
    purrr::map(default_json_content_cleaning) |>
    purrr::map(jsonlite::fromJSON) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_rbind()
}

default_json_content_cleaning <- function(json_string) {
  json_string |>
    stringr::str_remove("```$") |>
    stringr::str_remove("```json( )?")  |>
    stringr::str_replace(stringr::regex("\\}\n.+", dotall = TRUE), "}")|>
    stringr::str_remove("<\\|eot_id\\|>$")
}

retry_response <- function(base_url,
                           api_key,
                           response_format,
                           body,
                           max_retries,
                           pause_cap,
                           quiet) {
  res <- httr::RETRY(
    verb = "POST",
    url = base_url,
    config = httr::add_headers("Authorization" = paste("Bearer", api_key),
                               "content-type" = "application/json",
                               "response_format" = response_format),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  res

}
