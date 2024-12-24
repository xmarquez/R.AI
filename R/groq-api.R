#'
#' @inheritParams chat.openai_list
#' @param json_mode (Optional) If `TRUE`, ensures `response_format` is set to
#'   JSON mode. Defaults to `FALSE`.
#'
#' @seealso <https://console.groq.com/docs/api-reference#chat-create>
#' @rdname chat
#' @exportS3Method
chat.groq_list <- function(messages,
                           model = get_default_model("groq"),
                           frequency_penalty = NULL,
                           max_tokens = NULL,
                           n = 1,
                           presence_penalty = NULL,
                           response_format = NULL,
                           seed = NULL,
                           stop = NULL,
                           temperature = 0.2,
                           tools = NULL,
                           tool_choice = NULL,
                           parallel_tool_calls = NULL,
                           user = NULL,
                           json_mode = FALSE,
                           max_retries = 3,
                           quiet = FALSE) {

  # Ensure response_format is set to JSON if json_mode is enabled
  if (isTRUE(json_mode)) {
    response_format <- list(type = "json_object")
  }

  # Prepare the request body, including only non-NULL parameters
  args_list <- list(
    messages = messages,
    model = model,
    frequency_penalty = frequency_penalty,
    max_tokens = max_tokens,
    n = n,
    presence_penalty = presence_penalty,
    response_format = response_format,
    seed = seed,
    stop = stop,
    temperature = temperature,
    tools = tools,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    user = user
  )

  args_list <- args_list[!vapply(args_list, is.null, logical(1))]

  body <- jsonlite::toJSON(args_list, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "https://api.groq.com/openai/v1/chat/completions",
    api_key = Sys.getenv("GROQ_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = max_retries,
    pause_cap = 1200,
    quiet = quiet
  )

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}.")
  }

  response <- httr::content(res)

  structure(response, class = c("groq_chat", class(response)))
}

#' @exportS3Method
get_content.groq_chat <- function(response) {
  response$choices[[1]]$message$content
}

#' @exportS3Method
get_message.groq_chat <- function(response) {
  res <- response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message")) |>
    purrr::map(\(x) structure(x, class = c("groq_list", class(x))))
  res
}
