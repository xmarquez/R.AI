gemini_single_request <- function(prompt,
                                  model,
                                  n_candidates,
                                  max_retries,
                                  temperature,
                                  max_tokens,
                                  json_mode,
                                  system,
                                  response_validation_fun,
                                  content_extraction_fun,
                                  pause_cap = 1200,
                                  quiet = FALSE) {

  model_query <- paste0(model, ":generateContent")

  body <- jsonlite::toJSON(
    list(
      contents = prompt,
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_tokens
      )
    ),
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
    dplyr::rename(input_tokens = .data$promptTokenCount,
                  output_tokens = .data$candidatesTokenCount,
                  total_tokens = .data$totalTokenCount) |>
    dplyr::relocate(.data$model)
}
