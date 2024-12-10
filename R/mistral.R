#' Send a Single Request to the Mistral API
#'
#' This function sends a single prompt to the Mistral API and retrieves the
#' response. It also handles retries and extracts the relevant response content.
#'
#' @inheritParams openai_single_request
#' @param stop A character vector of stop sequences. The model will stop
#'   generating further tokens when any of the specified sequences is
#'   encountered. Defaults to `NULL`.
#' @param random_seed An integer for setting a seed for reproducibility.
#'   Defaults to `NULL` (no fixed seed).
#' @param presence_penalty A numeric value that penalizes the presence of new
#'   tokens. Values range from -2.0 to 2.0, with higher values encouraging the
#'   generation of novel tokens. Defaults to 0.
#' @param frequency_penalty A numeric value that penalizes the frequency of
#'   tokens that have already been generated. Values range from -2.0 to 2.0.
#'   Defaults to 0.
#' @param safe_prompt A logical value indicating whether to validate the prompt
#'   for safety before sending it to the API. Defaults to `FALSE`.
#'
#' @inherit openai_single_request return
#'
#' @details See the Mistral documentation at
#'   [https://docs.mistral.ai/capabilities/completion/](https://docs.mistral.ai/capabilities/completion/)
#'   for more.
#'
#' @family mistral
#' @family single message
#' @export
mistral_single_request <- function(prompt,
                                   model,
                                   n_candidates = 1,
                                   max_retries = 10,
                                   temperature = 0.2,
                                   top_p = 1,
                                   max_tokens = 300,
                                   stop = NULL,
                                   random_seed = NULL,
                                   presence_penalty = 0,
                                   frequency_penalty = 0,
                                   safe_prompt = FALSE,
                                   content_extraction_fun,
                                   pause_cap = 1200,
                                   quiet = FALSE) {

  body <- jsonlite::toJSON(
    list(
      model = model,
      temperature = temperature,
      top_p = top_p,
      max_tokens = max_tokens,
      stop = stop,
      random_seed = random_seed,
      messages = prompt,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      n = n_candidates,
      safe_prompt = safe_prompt
    ) |>
      purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- retry_response(base_url = "https://api.mistral.ai/v1/chat/completions",
                        api_key = Sys.getenv("MISTRAL_API_KEY"),
                        response_format = NULL,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  httr::stop_for_status(res)
  response <- httr::content(res)

  df <- mistral_usage(response)
  if (missing(content_extraction_fun)) {
    content_extraction_fun <- get("mistral_default_content_extraction")
  }

  content <- do.call(content_extraction_fun, list(response))

  df <- df |>
    dplyr::mutate(response = list(content))

  df
}

mistral_usage <- function(response) {
  usage_stats <- dplyr::tibble(prompt_tokens = response$usage$prompt_tokens,
                               completion_tokens  = response$usage$completion_tokens,
                               total_tokens = response$usage$total_tokens) |>
    dplyr::mutate(model = response$model)

  usage_stats
}

mistral_default_content_extraction <- function(response_content) {
  response_content$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

mistral_json_content_extraction <- function(response_content) {
  response_content |>
    mistral_default_content_extraction() |>
    default_json_content_extraction()
}

mistral_embedding <- function(texts, model, api_key, quiet = FALSE, ...) {
  url <- "https://api.mistral.ai/v1/embeddings"

  # Prepare the request payload
  payload <- list(
    model = model,
    input = as.list(texts)
  )

  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  if (!quiet) message("Sending request to Mistral embedding API...")

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
  httr::stop_for_status(response)
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
