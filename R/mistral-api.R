#' @rdname chat
#' @inheritParams chat.openai_list
#' @exportS3Method
chat.mistral_list <- function(messages,
                              model = get_default_model("mistral"),
                              n = 1,
                              max_retries = 3,
                              temperature = 0.2,
                              top_p = 1,
                              max_tokens = 300,
                              json_mode = FALSE,
                              stop = NULL,
                              random_seed = NULL,
                              presence_penalty = 0,
                              frequency_penalty = 0,
                              quiet = FALSE) {

  if (json_mode) {
    response_format <- list(type = "json_object")
  } else {
    response_format <- NULL
  }

  body <- jsonlite::toJSON(
    list(
      model = model,
      temperature = temperature,
      top_p = top_p,
      max_tokens = max_tokens,
      stop = stop,
      random_seed = random_seed,
      messages = messages,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      n = n
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
                        pause_cap = 1200,
                        quiet = quiet)

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)

  structure(response, class = c("mistral_chat", class(response)))
}

#' @exportS3Method
get_content.mistral_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

#' @exportS3Method
get_message.mistral_chat <- function(response) {
  res <- response$choices[[1]]$message |>
    purrr::compact() |>
    list() |>
    structure(class = c("mistral_list", "list"))

  res
}

#' @exportS3Method
embed.mistral_character <- function(content,
                                        model = "mistral-embed",
                                        quiet = FALSE) {
  checkmate::assert_character(content)

  url <- "https://api.mistral.ai/v1/embeddings"

  # Prepare the request payload
  payload <- list(
    model = model,
    input = as.list(texts)
  )

  headers <- httr::add_headers(
    Authorization = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")),
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
    terminate_on = c(400:499) # Terminate on client errors
  )

  # Handle response
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }
  response <- httr::content(response)

  total_tokens <- response$usage$total_tokens

  response <- response$data |>
    purrr::map(\(x) unlist(x$embedding))

  structure(response, class = c("embedding", class(response)),
            total_tokens = total_tokens, model = model)
}
