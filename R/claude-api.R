#' @param system Character or `NULL`. Optional system-level prompt providing
#'   context and instructions (e.g., specifying a goal or role). If `NULL`, the
#'   function will try to find it in the `"system"` attribute, if any, of
#'   `messages`.
#' @param stop_sequences Character vector or `NULL`. Custom text sequences that
#'   will cause the model to stop generating. Defaults to `NULL`.
#'
#' @seealso <https://docs.anthropic.com/en/api/messages>
#'
#' @examples
#' \dontrun{
#'   response <- chat.claude_list(
#'     messages = list(
#'       list(role = "user", content = "What is the capital of France?")
#'     ),
#'     model = "claude-3-haiku-20240307"
#'   )
#'   print(response)
#' }
#'
#' @rdname chat
#'
#' @exportS3Method
chat.claude_list <- function(messages,
                             model = get_default_model("claude"),
                             max_retries = 3,
                             temperature = 0.2,
                             max_tokens = 300,
                             system = NULL,
                             stop_sequences = NULL,
                             top_k = NULL,
                             top_p = NULL,
                             tool_choice = NULL,
                             tools = NULL,
                             quiet = FALSE) {
  # Validate parameters
  if (is.null(system)) {
    if (!is.null(attr(messages, "system"))) {
      system <- attr(messages, "system")
    }
  }

  # Construct request body
  body <- jsonlite::toJSON(
    list(
      system = system,
      messages = messages,
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      stop_sequences = stop_sequences,
      top_k = top_k,
      top_p = top_p,
      tool_choice = tool_choice,
      tools = tools
    ) |> purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Send request with retries
  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.anthropic.com/v1/messages",
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = 1200,
    quiet = quiet,
    terminate_on = c(400:499)
  )

  # Handle errors
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }

  # Parse response
  response <- httr::content(res)

  structure(response, class = c("claude_chat", class(response)))
}

#' @exportS3Method
get_content.claude_chat <- function(response) {
  response$content[[1]]$text
}

#' @exportS3Method
get_message.claude_chat <- function(response) {
  response$content |>
    structure(class = c("claude_list", "list"))
}

#' @exportS3Method
get_usage.claude_chat <- function(response, ...) {
  response$usage |>
    dplyr::as_tibble() |>
    dplyr::mutate(total_tokens = input_tokens + output_tokens,
                  input_tokens_details = list(dplyr::tibble(cache_creation_input_tokens, cache_read_input_tokens)),
                  model = response$model) |>
    dplyr::select(-cache_creation_input_tokens, -cache_read_input_tokens)
}

