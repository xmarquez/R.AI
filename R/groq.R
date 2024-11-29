groq_single_request <- function(prompt,
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

  if(json_mode) {
    response_format = jsonlite::toJSON(list(type = "json_object"),
                                       auto_unbox = TRUE)
  } else {
    response_format = NULL
  }

  body <- jsonlite::toJSON(
    list(
      messages = prompt,
      model = model,
      max_tokens = max_tokens,
      temperature = temperature
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- retry_response(base_url = "https://api.groq.com/openai/v1/chat/completions",
                        api_key = Sys.getenv("GROQ_API_KEY"),
                        response_format = response_format,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  httr::stop_for_status(res)
  response <- httr::content(res)

  df <- groq_usage(response)
  content <- do.call(content_extraction_fun, list(response))
  df <- df |>
    dplyr::mutate(response = list(content))

  df

}

groq_default_response_validation <- function(response) {
  return(TRUE)
}

groq_default_content_extraction <- function(response_content) {
  response_content$choices[[1]]$message$content
}

groq_json_response_validation <- function(response) {
  httr::content(response) |>
    groq_default_content_extraction() |>
    default_json_content_cleaning() |>
    jsonlite::validate()
}

groq_json_content_extraction <- function(response_content) {
  response_content |>
    groq_default_content_extraction() |>
    default_json_content_extraction()
}

groq_usage <- function(response) {
  response$usage |>
    dplyr::as_tibble() |>
    dplyr::mutate(model = response$model) |>
    dplyr::rename(input_tokens = "prompt_tokens",
                  output_tokens = "completion_tokens")
}
