#' Execute a Single Request to a Llamafile Local API
#'
#' This function sends a single prompt to a locally running Llamafile instance
#' using the OpenAI-compatible API endpoint. It allows users to customize parameters
#' such as model, number of candidates, and more. It also handles retries and extracts
#' the relevant response content.
#'
#' @param prompt A list of messages, each with a role (e.g., "user" or
#'   "assistant") and content, used as input for the model. Must be in the
#'   format `list(list(role = "user", content = "Hello world!"))`.
#' @param model The model to use for generating responses, e.g.,
#'   "LLaMA_CPP".
#' @param n_candidates The number of response candidates to generate. Defaults
#'   to 1.
#' @param max_retries The maximum number of retry attempts in case of request
#'   failures. Defaults to 10.
#' @param temperature A numeric value between 0 and 1 that controls the
#'   randomness of the response. Higher values make the output more random.
#'   Defaults to 0.2.
#' @param max_tokens The maximum number of tokens to include in the response.
#'   Defaults to 300.
#' @param json_mode A logical value indicating whether the response should be
#'   parsed as JSON. Defaults to `FALSE`.
#' @param system Optional system message providing instructions or context for
#'   the model.
#' @param response_validation_fun A function to validate the response received
#'   from the API. Defaults to `llamafile_default_response_validation()` if not
#'   provided.
#' @param content_extraction_fun A function to extract the desired content from
#'   the API response. If not provided, a default extraction function is used
#'   depending on the value of `json_mode`.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A tibble containing the usage statistics (tokens used) and the
#'   generated response(s).
#' @export
llamafile_single_request <- function(prompt,
                                     model = "LLaMA_CPP",
                                     n_candidates = 1,
                                     max_retries = 10,
                                     temperature = 0.2,
                                     max_tokens = 300,
                                     json_mode = FALSE,
                                     system,
                                     response_validation_fun,
                                     content_extraction_fun,
                                     pause_cap = 1200,
                                     quiet = FALSE) {

  body <- jsonlite::toJSON(
    list(
      messages = prompt,
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      n = n_candidates
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- retry_response(base_url = "http://localhost:8080/v1/chat/completions",
                        api_key = "no-key",
                        response_format = NULL,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  httr::stop_for_status(res)
  response <- httr::content(res)

  df <- llamafile_usage(response)
  if (missing(content_extraction_fun)) {
    if (!json_mode) {
      content_extraction_fun <- get("llamafile_default_content_extraction")
    } else {
      content_extraction_fun <- get("llamafile_json_content_extraction")
    }
  }

  content <- do.call(content_extraction_fun, list(response))

  df <- df |>
    dplyr::mutate(response = list(content))

  df
}

llamafile_default_response_validation <- function(response) {
  return(TRUE)
}

llamafile_json_response_validation <- function(response) {
  response |>
    llamafile_default_content_extraction() |>
    default_json_content_cleaning() |>
    jsonlite::validate()
}

llamafile_default_content_extraction <- function(response_content) {
  response_content$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

llamafile_json_content_extraction <- function(response_content) {
  response_content |>
    llamafile_default_content_extraction() |>
    default_json_content_extraction()
}

llamafile_usage <- function(response) {
  usage_stats <- dplyr::tibble(prompt_tokens = response$usage$prompt_tokens,
                               completion_tokens  = response$usage$completion_tokens,
                               total_tokens = response$usage$total_tokens) |>
    dplyr::mutate(model = response$model)

  usage_stats
}

#' Check if Llamafile is Running on localhost:8080
#'
#' This function checks if a Llamafile instance is currently running at
#' localhost:8080.
#'
#' @return TRUE if any Llamafile instance is running, otherwise FALSE.
#' @export
is_llamafile_running <- function() {
  res <- try(httr::GET("http://localhost:8080/v1/models"), silent = TRUE)
  if (inherits(res, "try-error") || httr::status_code(res) != 200) {
    return(FALSE)
  }
  return(TRUE)
}

#' Start Llamafile Instance
#'
#' This function attempts to start the Llamafile executable if it is not already running.
#'
#' @param llamafile_path The path to the Llamafile executable.
#' @export
start_llamafile <- function(llamafile_path) {
  checkmate::assert_file_exists(llamafile_path)
  if(is_llamafile_running()) {
    running_model <- which_llamafile_running()
    stop(glue::glue("{running_model} is already running on http://localhost:8080. Kill the current llamafile before starting a new one."))
  }
  if (.Platform$OS.type == "windows") {
    shell.exec(shQuote(fs::path_real(llamafile_path)))
  } else {
    system(sprintf("nohup %s > /dev/null 2>&1 &", shQuote(llamafile_path)), wait = FALSE, invisible = TRUE)
  }
}

#' Kill the Running Llamafile Process
#'
#' This function checks if a Llamafile instance is running and attempts to terminate it.
#'
#' @return TRUE if the Llamafile process was successfully terminated, FALSE otherwise.
#' @export
kill_llamafile <- function() {
  if(!is_llamafile_running()) {
    warning("No llamafile instance appears to be running.")
    return(FALSE)
  }

  running_model <- which_llamafile_running()

  if (.Platform$OS.type == "windows") {
    # Terminate process on Windows
    system(glue::glue("taskkill /F /IM {running_model}.llamafile.exe"), show.output.on.console = FALSE)
  } else {
    # Terminate process on Unix-like systems
    system(glue::glue("pkill -f {running_model}.llamafile"), wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  # Re-check if Llamafile is still running
  if (!is_llamafile_running()) {
    message("Llamafile instance successfully terminated.")
    return(TRUE)
  } else {
    warning("Failed to terminate the Llamafile instance.")
    return(FALSE)
  }
}

#' Determines which llamfile model is running
#'
#' Determines which llamafile model is running, if any.
#'
#' @return The name of the runnning llamafile model, or `NA` if no llamafile is
#'   running.
#' @export
which_llamafile_running <- function() {
  if(is_llamafile_running()) {
    res <- httr::GET("http://localhost:8080/v1/models")
    running_model <- httr::content(res)$data[[1]]$id |>
      stringr::str_remove(".gguf")
    return(running_model)
  } else {
    warning("No llamafile instance running.")
    return(NA_character_)
  }
}

llamafile_embedding <- function(text,
                                model = "LLaMA_CPP",
                                max_retries = 2,
                                pause_cap = 1200,
                                quiet = FALSE) {

  body <- jsonlite::toJSON(
    list(
      messages = list(content = text),
      model = model
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- retry_response(base_url = "http://localhost:8080/embedding",
                        api_key = "no-key",
                        response_format = NULL,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  httr::stop_for_status(res)
  response <- httr::content(res)

  embedding <- response |> purrr::flatten() |>
    unlist()
}
