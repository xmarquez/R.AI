#' Call Language Model API
#'
#' This generic function takes a list of prompts and sends them sequentially to
#' various language model APIs, putting the responses in a tidy [tibble()]. The
#' prompts should typically be created via [format_chat()] so they have the
#' appropriate class. For processing a single prompt, rather than a list of
#' prompts, use [chat()]. For asynchronous batch processing of prompts in the
#' [Anthropic](https://docs.anthropic.com/en/api/creating-message-batches),
#' [OpenAI](https://platform.openai.com/docs/api-reference/batch), and
#' [Mistral](https://docs.mistral.ai/capabilities/batch/) APIs, use
#' [batch_job()].
#'
#' @param prompts A list of prompts to send to the API. The class of this object
#'   should match one of the supported APIs: "groq", "claude" (for the Anthropic
#'   API), "openai", "gemini", or "llamafile" (for local
#'   [llamafiles](https://github.com/mozilla-ocho/llamafile/)). It will
#'   typically be the result of a call to [format_chat()].
#' @param model A string specifying the model to use. Get available models with
#'   [get_available_models()].
#' @param prompt_name An optional string specifying the type of prompt.
#' @param ... Additional arguments passed to specific [chat()] methods, such as
#'   `max_retries`, `temperature`, `max_tokens`, `json_mode`, `system`,
#'   `pause_cap`, `llamafile_path`, or `log`. See the documentation for [chat()]
#'   for more.
#'
#' @return A [tibble()] containing the API responses and usage information.
#'
#' @seealso [format_chat()] for creating prompts to use with this function.
#' @family generic
#' @family multiple requests
#' @export
call_api <- function(prompts, model, prompt_name, ...) {
  UseMethod("call_api", prompts)
}

#' @rdname call_api
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
  quiet <- args$quiet %||% FALSE
  log <- args$log %||% !quiet

  # Validate log - must be a logical scalar
  checkmate::assert_logical(log, len = 1)

  # Validate json_mode - must be a logical scalar
  checkmate::assert_logical(json_mode, len = 1)

  if (missing(prompt_name)) {
    prompt_name <- if (json_mode) "json" else "default"
  }

  # Retrieve api
  api <- class(prompts)[1]

  if(missing(model)) {
    model <- get_default_model(api)
  }

  # Validate arguments, including those in ...
  # validate_single_request(prompts, model, prompt_name, ...)

  # Ensure prompt IDs are defined
  ids <- names(prompts)
  if (is.null(ids)) {
    ids <- seq_along(prompts)
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
      cli::cli_alert_info(log_message)
    }

    res <- chat(messages = prompts[[i]],
                model = model, ...)

    response <- get_usage(res, model)
    response$response <- list(get_content(res))

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


#' @rdname call_api
#' @export
call_api.llama_cpp <- function(prompts, model, prompt_name, ...) {

  if(!is_llamafile_running()) {
    cli::cli_abort(
        c("No llama.cpp model is running. Start the llama.cpp server or llamafile manually, and check that it is running by calling {.fun is_llamafile_running}"))
    }
  model <- which_llamafile_running()

  NextMethod(prompts,
             model = model,
             ...)
}

#' @rdname call_api
#' @export
call_api.ollama <- function(prompts, model, prompt_name, ...) {

  running_models <- list_running_models.ollama()

  if(length(running_models$models) == 0) {
    cli::cli_abort(
      "No ollama server is running. Make sure the server is running and you have provided an appropriate model name.")
  }
  model <- running_models$models[[1]]

  NextMethod(prompts,
             model = model,
             ...)
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
    quiet = quiet,
    terminate_on = c(400:499)
  )

  res

}
