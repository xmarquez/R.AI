#'
#' @inheritParams chat.openai_list
#' @seealso <https://inference-docs.cerebras.ai/api-reference/>
#' @rdname chat
#' @exportS3Method
chat.cerebras_list <- function(messages,
                               model = get_default_model("cerebras"),
                               max_tokens = NULL,
                               json_mode = FALSE,
                               seed = NULL,
                               stop = NULL,
                               temperature = 0.2,
                               top_p = NULL,
                               tools = NULL,
                               tool_choice = NULL,
                               user = NULL,
                               quiet = FALSE) {

  # Ensure response_format is set to JSON if specified
  if (json_mode) {
    response_format <- list(type = "json_object")
  } else {
    response_format <- NULL
  }

  # Prepare the request body, including only non-NULL parameters
  args_list <- list(
    messages = messages,
    model = model,
    max_completion_tokens = max_tokens,
    response_format = response_format,
    seed = seed,
    stop = stop,
    temperature = temperature,
    top_p = top_p,
    tools = tools,
    tool_choice = tool_choice,
    user = user
  )

  args_list <- args_list[!vapply(args_list, is.null, logical(1))]

  body <- jsonlite::toJSON(args_list, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "https://api.cerebras.ai/v1/chat/completions",
    api_key = Sys.getenv("CEREBRAS_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = 3,
    pause_cap = 1200,
    quiet = quiet
  )

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }

  response <- httr::content(res)

  structure(response, class = c("cerebras_chat", class(response)))
}

#' @exportS3Method
get_content.cerebras_chat <- function(response) {
  response$choices[[1]]$message$content
}

#' @exportS3Method
get_message.cerebras_chat <- function(response) {
  res <- response$choices[[1]] |>
    purrr::pluck("message") |>
    list() |>
    structure(class = c("cerebras_list", "list"))

  res
}
