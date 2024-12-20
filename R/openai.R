#' Send a Single Request to the OpenAI API
#'
#' This function sends a single prompt to the [OpenAI
#' API](https://platform.openai.com/docs/api-reference/chat) for processing. It
#' also handles retries and extracts the relevant response content.
#'
#' @param prompt A list containing the prompt message(s). Typically this is the
#'   output of [prompt()].
#' @param model A string specifying the model to use. Get available models with
#'   [list_models()] or [get_available_models()].
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
#' @param content_extraction_fun A function to extract the desired content from
#'   the API response. If not provided, a default extraction function is used
#'   depending on the value of `json_mode`.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A [tibble()] with a `response` column and usage information.
#'
#' @details See the Openai API documentation at
#'   [https://platform.openai.com/docs/api-reference/chat](https://platform.openai.com/docs/api-reference/chat)
#'   for more.
#'
#' @family openai
#' @family single message
#' @export
openai_single_request <- function(prompt,
                                  model,
                                  n_candidates = 1,
                                  max_retries = 10,
                                  temperature = 0.2,
                                  max_tokens = 300,
                                  json_mode = FALSE,
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

  res <- retry_response(base_url = "https://api.openai.com/v1/chat/completions",
                        api_key = Sys.getenv("OPENAI_API_KEY"),
                        response_format = NULL,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)

  df <- openai_usage(response)
  if (missing(content_extraction_fun)) {
    if(!json_mode) {
      content_extraction_fun <- get("openai_default_content_extraction")
    } else {
      content_extraction_fun <- get("openai_json_content_extraction")
    }
  }

  content <- do.call(content_extraction_fun, list(response))

  df <- df |>
    dplyr::mutate(response = list(content))

  df
}

openai_default_content_extraction <- function(response_content) {
  response_content$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()

}

openai_json_content_extraction <- function(response_content) {
  response_content |>
    openai_default_content_extraction() |>
    default_json_content_extraction()
}

openai_usage <- function(response) {
  usage_stats <- dplyr::tibble(prompt_tokens = response$usage$prompt_tokens,
                               completion_tokens  = response$usage$completion_tokens,
                               total_tokens = response$usage$total_tokens) |>
    dplyr::mutate(model = response$model)

  usage_stats
}

openai_embedding <- function(texts, model, api_key, quiet = FALSE) {
  url <- "https://api.openai.com/v1/embeddings"

  # Prepare the request payload
  payload <- list(
    model = model,
    input = as.list(texts)
  )

  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  if (!quiet) message("Sending request to OpenAI embedding API...")

  # Use httr::RETRY for robust retry logic
  response <- httr::RETRY(
    verb = "POST",
    url = url,
    headers,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "json",
    times = 3, # Number of retries
    quiet = quiet,
    terminate_on = c(400, 401, 403, 404) # Terminate on client errors
  )

  # Handle response
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
    }
  result <- httr::content(response)

  # Extract embeddings into a tibble
  return(
    dplyr::tibble(
      id = seq_along(texts),
      text_set = digest::digest(texts),
      embedding = purrr::map(result$data, "embedding")
    )
  )
}
