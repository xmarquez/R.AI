#' Send a Single Request to the Mistral API
#'
#' This function sends a single prompt to the Mistral API and retrieves the response.
#' It supports various parameters for fine-tuning the behavior of Mistral's models,
#' which include premier, free, and legacy options. Mistral's API provides state-of-the-art
#' capabilities for text generation, coding, embeddings, and moderation.
#'
#' @param prompt A list containing the prompt message(s). Each message should
#'   be a named list with `role` (e.g., "system", "user") and `content`.
#' @param model A string specifying the model to use. Available models include:
#'
#'   - **Premier Models**:
#'     - `"mistral-large-latest"`: Top-tier reasoning model for high-complexity tasks.
#'     - `"pixtral-large-latest"`: Frontier-class multimodal model.
#'     - `"ministral-3b-latest"`: Best edge model with high performance/price ratio.
#'     - `"ministral-8b-latest"`: Powerful edge model with excellent efficiency.
#'     - `"mistral-small-latest"`: Enterprise-grade small model for cost-sensitive applications.
#'     - `"codestral-latest"`: Language model specialized for coding.
#'     - `"mistral-moderation-latest"`: Moderation model for harmful content detection.
#'
#'   - **Free Models**:
#'     - `"pixtral-12b-2409"`: Multimodal model with image and text understanding.
#'     - `"open-mistral-nemo"`: Multilingual open-source model.
#'     - `"open-codestral-mamba"`: Open-source coding model.
#'
#'   - **Legacy Models** (deprecated as of November 2024, retired by March 2025):
#'     - `"open-mistral-7b"`: The first dense model released in September 2023.
#'       Legacy date: 2024-11-25. Retirement date: 2025-03-30.
#'       Recommended alternative: `"ministral-8b-latest"`.
#'     - `"open-mixtral-8x7b"`: The first sparse mixture-of-experts model (December 2023).
#'       Legacy date: 2024-11-25. Retirement date: 2025-03-30.
#'       Recommended alternative: `"mistral-small-latest"`.
#'     - `"open-mixtral-8x22b"`: Advanced sparse model (April 2024).
#'       Legacy date: 2024-11-25. Retirement date: 2025-03-30.
#'       Recommended alternative: `"mistral-small-latest"`.
#'     - `"mistral-medium-2312"`: Model for intermediate reasoning tasks (December 2023).
#'       Legacy date: 2024-11-25. Retirement date: 2025-03-30.
#'       Recommended alternative: `"mistral-small-latest"`.
#'     - `"mistral-large-2402"`: An earlier version of the reasoning model (February 2024).
#'       Legacy date: 2024-11-25. Retirement date: 2025-03-30.
#'       Recommended alternative: `"mistral-large-latest"`.
#'
#' @param n_candidates An integer specifying the number of response candidates
#'   to generate per prompt. Defaults to 1.
#' @param max_retries Maximum number of retry attempts in case of request failure.
#'   Defaults to 10.
#' @param temperature A numeric value controlling randomness in the model's output.
#'   Higher values (e.g., 1) produce more diverse responses, while lower values
#'   (e.g., 0.2) make responses more focused. Defaults to 0.2.
#' @param top_p A numeric value for nucleus sampling, restricting responses to
#'   the smallest subset of tokens with cumulative probability `top_p`. Defaults to 1.
#' @param max_tokens The maximum number of tokens to generate in the response.
#'   Defaults to 300.
#' @param stream A logical value indicating whether to stream responses as they
#'   are generated. Defaults to `FALSE`.
#' @param stop A character vector of stop sequences. The model will stop generating
#'   further tokens when any of the specified sequences is encountered. Defaults to `NULL`.
#' @param random_seed An integer for setting a seed for reproducibility.
#'   Defaults to `NULL` (no fixed seed).
#' @param presence_penalty A numeric value that penalizes the presence of new tokens.
#'   Values range from -2.0 to 2.0, with higher values encouraging the generation
#'   of novel tokens. Defaults to 0.
#' @param frequency_penalty A numeric value that penalizes the frequency of tokens
#'   that have already been generated. Values range from -2.0 to 2.0. Defaults to 0.
#' @param safe_prompt A logical value indicating whether to validate the prompt
#'   for safety before sending it to the API. Defaults to `FALSE`.
#' @param response_validation_fun A function to validate the API's response.
#'   If not provided, the default validation function will be used.
#' @param content_extraction_fun A function to extract content from the API's
#'   response. If not provided, the default extraction function
#'   (`mistral_default_content_extraction`) will be used.
#' @param pause_cap The maximum duration (in seconds) for pauses between retries
#'   when the API is rate-limited. Defaults to 1200 seconds.
#' @param quiet A logical value. If `TRUE`, suppresses retry messages. Defaults to `FALSE`.
#'
#' @return A tibble containing the following columns:
#'   - `response`: The extracted response content.
#'   - `usage`: Usage details from the API (e.g., tokens used, API cost).
#'
#' @details
#' This function supports Mistral's premier, free, and legacy models. Legacy
#' models are scheduled for deprecation as of November 2024 and retirement by
#' March 2025. Users are encouraged to transition to recommended alternatives
#' listed above to avoid disruptions.
#'
#' The function constructs a JSON payload from the provided arguments, sends it to the
#' Mistral API using `POST`, and processes the response. It supports retries for failed
#' requests and allows customization of response validation and extraction.
#'
#' @export
mistral_single_request <- function(prompt,
                                   model,
                                   n_candidates = 1,
                                   max_retries = 10,
                                   temperature = 0.2,
                                   top_p = 1,
                                   max_tokens = 300,
                                   stream = FALSE,
                                   stop = NULL,
                                   random_seed = NULL,
                                   presence_penalty = 0,
                                   frequency_penalty = 0,
                                   safe_prompt = FALSE,
                                   response_validation_fun,
                                   content_extraction_fun,
                                   pause_cap = 1200,
                                   quiet = FALSE) {

  body <- jsonlite::toJSON(
    list(
      model = model,
      temperature = temperature,
      top_p = top_p,
      max_tokens = max_tokens,
      stream = stream,
      stop = stop,
      random_seed = random_seed,
      messages = prompt,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      n = n_candidates,
      safe_prompt = safe_prompt
    ),
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

