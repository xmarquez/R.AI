#' Send a Single Request to the Gemini API
#'
#' This function sends a single request to the Gemini API for content
#' generation. It handles retries, validates the response, and extracts the
#' desired content, returning the results as a tibble.
#'
#' @param prompt A list of message objects or structured prompts, depending on
#'   the Gemini API's requirements. Typically, you will generate this using
#'   [prompt()].
#' @param model A character string specifying the Gemini model to use for
#'   content generation.
#' @param max_retries An integer specifying the maximum number of retry attempts
#'   in case of request failures. Defaults to 10.
#' @param temperature A numeric value between 0 and 1 controlling the randomness
#'   of the model's output (higher values produce more random responses).
#'   Defaults to 0.2.
#' @param max_tokens An integer specifying the maximum number of tokens in the
#'   generated output.
#' @param system An optional string to provide system-level instructions or
#'   context for the model. If not provided, the function will attempt to
#'   extract it from the attribute `system` in `prompt` (if it exists);
#'   otherwise it will default to `NULL`.
#' @param content_extraction_fun A function for extracting content from the API
#'   response. This function should accept the API response as input and return
#'   the extracted content.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200 seconds.
#' @param quiet A logical value indicating whether to suppress log messages
#'   during retries. Defaults to `FALSE`.
#'
#' @return A tibble containing:
#'   - **Usage statistics**: Extracted using the internal `gemini_usage` function.
#'   - **Generated response content**: Extracted using the provided `content_extraction_fun`.
#'
#' @details This function constructs a POST request with the given `prompt` and
#'   model configuration, retries in case of transient errors, validates the API
#'   response, and extracts the desired content. The `content_extraction_fun`
#'   function is used to parse the generated content, and the results are
#'   returned in a structured tibble.
#'
#' @examples
#' \dontrun{
#' # Define a content extraction function
#' extract_content <- function(response) {
#'   response$candidates[[1]]$content$parts[[1]]$text
#' }
#'
#' # Send a single request to the Gemini API
#' response_df <- gemini_single_request(
#'   prompt = prompt("What are the benefits of regular exercise?", api = "gemini"),
#'   model = "gemini-1.5-flash-latest",
#'   max_retries = 3,
#'   temperature = 0.7,
#'   max_tokens = 150,
#'   system = NULL,
#'   content_extraction_fun = extract_content,
#'   pause_cap = 60,
#'   quiet = FALSE
#' )
#'
#' print(response_df)
#' }
#'
#' @export
gemini_single_request <- function(prompt,
                                  model,
                                  max_retries = 10,
                                  temperature = 0.2,
                                  max_tokens = 300,
                                  system = NULL,
                                  content_extraction_fun,
                                  pause_cap = 1200,
                                  quiet = FALSE) {

  model_query <- paste0(model, ":generateContent")

  if(is.null(system)) {
    if(!is.null(attr(prompt, "system"))) {
      system <- list(parts = attr(prompt, "system"))
    }
  }

  body <- jsonlite::toJSON(
    list(
      system_instruction = system,
      contents = prompt,
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_tokens
      )
    ) |> purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )


  res <- httr::RETRY(
    verb = "POST",
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    config = httr::add_headers("content-type" = "application/json"),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  httr::stop_for_status(res)
  response <- httr::content(res)

  df <- gemini_usage(response, model)
  content <- do.call(content_extraction_fun, list(response))
  df <- df |>
    dplyr::mutate(response = list(content))

  df
}

gemini_default_response_validation <- function(response) {
  return(TRUE)
}

gemini_json_response_validation <- function(response) {
  response |>
    gemini_default_content_extraction() |>
    default_json_content_cleaning() |>
    jsonlite::validate()
}

gemini_default_content_extraction <- function(response_content) {
  response_content$candidates[[1]]$content$parts[[1]]$text
}

gemini_json_content_extraction <- function(response_content) {
  response_content |>
    gemini_default_content_extraction() |>
    default_json_content_extraction()
}

gemini_usage <- function(response, model) {
  if(is.null(response$usageMetadata$candidatesTokenCount)) {
    response$usageMetadata$candidatesTokenCount <- 0
  }
  response$usageMetadata |>
    dplyr::as_tibble() |>
    dplyr::mutate(model = model) |>
    dplyr::rename(input_tokens = "promptTokenCount",
                  output_tokens = "candidatesTokenCount",
                  total_tokens = "totalTokenCount") |>
    dplyr::relocate("model")
}
