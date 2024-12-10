#' Send a Single Request to the Anthropic API
#'
#' This function sends a single prompt to the [Anthropic
#' API](https://docs.anthropic.com/en/api/messages) for messages.It also handles
#' retries and extracts the relevant response content.
#'
#' @inheritParams openai_single_request
#' @param system A character string providing additional context or instructions
#'   for the system. If not provided, the function will attempt to extract it
#'   from the attribute `system` in `prompt` (if it exists); otherwise it will
#'   default to `NULL`.
#'
#' @details See the Anthropic API documentation at
#'   [https://docs.anthropic.com/en/api/messages](https://docs.anthropic.com/en/api/messages)
#'   for more.
#'
#' @inherit openai_single_request return
#'
#' @family claude
#' @family single message
#' @export
claude_single_request <- function(prompt,
                                  model,
                                  max_retries = 10,
                                  temperature = 0.2,
                                  max_tokens = 300,
                                  json_mode = FALSE,
                                  system = NULL,
                                  content_extraction_fun,
                                  pause_cap = 1200,
                                  quiet = FALSE,
                                  stop_sequences = NULL) {
  if(is.null(system)) {
    system <- attr(prompt, "system")
  }
  if(!is.null(stop_sequences)) {
    checkmate::assert_character(stop_sequences)
  }

  body <- jsonlite::toJSON(
    list(
      system = system,
      messages = prompt,
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      stop_sequences = stop_sequences
    ) |>
      purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.anthropic.com/v1/messages",
    config = httr::add_headers("x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
                               "anthropic-version" = "2023-06-01",
                               "content-type" = "application/json"),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  httr::stop_for_status(res)
  response <- httr::content(res)

  df <- claude_usage(response)
  if (missing(content_extraction_fun)) {
    if(!json_mode) {
      content_extraction_fun <- get("claude_default_content_extraction")
    } else {
      content_extraction_fun <- get("claude_json_content_extraction")
    }
  }

  content <- do.call(content_extraction_fun, list(response))
  df <- df |>
    dplyr::mutate(response = list(content))

  df
}

claude_default_content_extraction <- function(response_content) {
  response_content$content[[1]]$text
}

claude_json_content_extraction <- function(response_content) {
  response_content |>
    claude_default_content_extraction() |>
    default_json_content_extraction()
}

claude_usage <- function(response) {
  response$usage |>
    dplyr::as_tibble() |>
    dplyr::mutate(model = response$model)
}

