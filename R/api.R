#' Call Language Model API
#'
#' This generic function sends prompts to various language model APIs and
#' retrieves responses. It supports multiple APIs, including Groq, OpenAI,
#' Claude (Anthropic), and Gemini.
#'
#' @param prompts A list of prompts to send to the API. The class of this object
#'   should match one of the supported APIs: "groq", "claude" (for the Anthropic
#'   API), "openai", "gemini", or "llamafile" (for local
#'   [llamafiles](https://github.com/mozilla-ocho/llamafile/)).
#' @param model A string specifying the model to use.
#' @param prompt_name An optional string specifying the type of prompt.
#' @param ... Additional arguments passed to specific `call_api` methods, such
#'   as `max_retries`, `temperature`, `max_tokens`, `json_mode`, `system`,
#'   `pause_cap`, `llamafile_path`, or `log`.
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
#' @seealso [build_prompts_from_files()] for creating prompts to use with this
#'   function.
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
  id <- api <- NULL

  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  json_mode <- args$json_mode %||% FALSE
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  single_request_fun <- args$single_request_fun
  response_validation_fun <- args$response_validation_fun
  content_extraction_fun <- args$content_extraction_fun
  llamafile_path <- args$llamafile_path
  log <- args$log %||% TRUE

  # Ensure prompt IDs are defined
  ids <- names(prompts)
  if (is.null(ids)) {
    stop("Each prompt must have a unique name. Please name your prompts.")
  }

  # Initialize response collection
  responses <- tibble::tibble()

  # Generate a unique hash for the prompt set
  prompt_set <- digest::digest(ids)

  # Start Llamafile if applicable and not already running
  if (!is.null(llamafile_path) && !is_llamafile_running()) {
    message("Starting LLamafile: ", llamafile_path)
    start_llamafile(llamafile_path)
  }

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
      n_candidates = n_candidates,
      max_retries = max_retries,
      temperature = temperature,
      max_tokens = max_tokens,
      json_mode = json_mode,
      system = system,
      response_validation_fun = response_validation_fun,
      content_extraction_fun = content_extraction_fun,
      pause_cap = pause_cap,
      quiet = !log
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
  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  json_mode <- args$json_mode %||% TRUE
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  log <- args$log %||% TRUE

  # Set defaults for model and prompt_name if missing
  if (missing(model)) {
    model <- get_default_model("groq")
  }

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  # Validate the arguments (optional, depending on your implementation)
  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         json_mode = json_mode,
                         system = system,
                         pause_cap = pause_cap,
                         log = log)

  # Resolve functions using the helper
  resolved_functions <- resolve_functions("groq", prompt_name)

  # Call the generic NextMethod to handle the request
  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             n_candidates = n_candidates,
             max_retries = max_retries,
             temperature = temperature,
             max_tokens = max_tokens,
             json_mode = json_mode,
             system = system,
             response_validation_fun = resolved_functions$response_validation_fun,
             content_extraction_fun = resolved_functions$content_extraction_fun,
             single_request_fun = resolved_functions$single_request_fun,
             pause_cap = pause_cap,
             log = log)
}


#' @export
call_api.claude <- function(prompts, model, prompt_name, ...) {
  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  json_mode <- args$json_mode %||% TRUE
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  log <- args$log %||% TRUE

  if (missing(model)) {
    model <- get_default_model(class(prompts)[1])
  }

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         json_mode = json_mode,
                         system = system,
                         pause_cap = pause_cap,
                         log = log)

  # Resolve functions using the helper
  resolved_functions <- resolve_functions("claude", prompt_name)

  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             n_candidates = n_candidates,
             max_retries = max_retries,
             temperature = temperature,
             max_tokens = max_tokens,
             json_mode = json_mode,
             system = system,
             response_validation_fun = resolved_functions$response_validation_fun,
             content_extraction_fun = resolved_functions$content_extraction_fun,
             single_request_fun = resolved_functions$single_request_fun,
             pause_cap = pause_cap,
             log = log)
}

#' @export
call_api.mistral <- function(prompts, model, prompt_name, ...) {
  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  top_p <- args$top_p %||% 1
  max_tokens <- args$max_tokens %||% 300
  stop <- args$stop
  random_seed <- args$random_seed
  presence_penalty <- args$presence_penalty %||% 0
  frequency_penalty <- args$frequency_penalty %||% 0
  safe_prompt <- args$safe_prompt %||% FALSE
  pause_cap <- args$pause_cap %||% 1200
  log <- args$log %||% TRUE

  if (missing(model)) {
    model <- get_default_model(class(prompts)[1])
  }

  if (missing(prompt_name)) {
    prompt_name <- if (args$json_mode %||% TRUE) "json" else "default"
  }

  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         json_mode = args$json_mode %||% TRUE,
                         system = args$system,
                         pause_cap = pause_cap,
                         log = log)

  # Resolve functions using the helper
  resolved_functions <- resolve_functions("mistral", prompt_name)

  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             n_candidates = n_candidates,
             max_retries = max_retries,
             temperature = temperature,
             top_p = top_p,
             max_tokens = max_tokens,
             stop = stop,
             random_seed = random_seed,
             presence_penalty = presence_penalty,
             frequency_penalty = frequency_penalty,
             safe_prompt = safe_prompt,
             json_mode = args$json_mode %||% TRUE,
             pause_cap = pause_cap,
             response_validation_fun = resolved_functions$response_validation_fun,
             content_extraction_fun = resolved_functions$content_extraction_fun,
             single_request_fun = resolved_functions$single_request_fun,
             log = log)
}


#' @export
call_api.openai <- function(prompts, model, prompt_name, ...) {
  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  json_mode <- args$json_mode %||% TRUE
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  log <- args$log %||% TRUE

  if (missing(model)) {
    model <- get_default_model(class(prompts)[1])
  }

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         json_mode = json_mode,
                         system = system,
                         pause_cap = pause_cap,
                         log = log)

  # Resolve functions using the helper
  resolved_functions <- resolve_functions("openai", prompt_name)

  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             n_candidates = n_candidates,
             max_retries = max_retries,
             temperature = temperature,
             max_tokens = max_tokens,
             json_mode = json_mode,
             system = system,
             response_validation_fun = resolved_functions$response_validation_fun,
             content_extraction_fun = resolved_functions$content_extraction_fun,
             single_request_fun = resolved_functions$single_request_fun,
             pause_cap = pause_cap,
             log = log)
}

#' @export
call_api.gemini <- function(prompts, model, prompt_name, ...) {
  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  json_mode <- args$json_mode %||% TRUE
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  log <- args$log %||% TRUE

  if (missing(model)) {
    model <- get_default_model(class(prompts)[1])
  }

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         json_mode = json_mode,
                         system = system,
                         pause_cap = pause_cap,
                         log = log)

  # Resolve functions using the helper
  resolved_functions <- resolve_functions("gemini", prompt_name)

  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             n_candidates = n_candidates,
             max_retries = max_retries,
             temperature = temperature,
             max_tokens = max_tokens,
             json_mode = json_mode,
             system = system,
             response_validation_fun = resolved_functions$response_validation_fun,
             content_extraction_fun = resolved_functions$content_extraction_fun,
             single_request_fun = resolved_functions$single_request_fun,
             pause_cap = pause_cap,
             log = log)
}


#' @export
call_api.llamafile <- function(prompts, model, prompt_name, ...) {
  # Extract additional arguments from ...
  args <- list(...)
  n_candidates <- args$n_candidates %||% 1
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  json_mode <- args$json_mode %||% TRUE
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  llamafile_path <- args$llamafile_path
  log <- args$log %||% TRUE

  if (missing(model)) {
    model <- "LLaMA_CPP"
  }

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         json_mode = json_mode,
                         system = system,
                         pause_cap = pause_cap,
                         log = log,
                         llamafile_path = llamafile_path)

  # Resolve functions using the helper
  resolved_functions <- resolve_functions("llamafile", prompt_name)

  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             n_candidates = n_candidates,
             max_retries = max_retries,
             temperature = temperature,
             max_tokens = max_tokens,
             json_mode = json_mode,
             system = system,
             llamafile_path = llamafile_path,
             response_validation_fun = resolved_functions$response_validation_fun,
             content_extraction_fun = resolved_functions$content_extraction_fun,
             single_request_fun = resolved_functions$single_request_fun,
             pause_cap = pause_cap,
             log = log)
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
