#' Count Tokens for a Given Message Using Anthropic API
#'
#' This function calls the Anthropic Token Count API to determine the number of
#' tokens in a given message. The API is useful for understanding token usage in
#' models before sending the actual message for processing.
#'
#' @param messages A list of prompts created by [build_prompts_from_files()].
#' @param model A string specifying the model to be used (e.g.,
#'   "claude-3-5-sonnet-20241022").
#' @param system (Optional) A string specifying a system prompt to provide
#'   context and instructions to the model. Tokens in the system prompt are
#'   counted as well.
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return An integer representing the total number of input tokens across the
#'   provided list of messages and system prompt.
#' @export
#'
#' @examples
#' messages <- list(
#'   list(
#'     list(
#'       role = "user", content = "Hello, world"
#'       )
#'     )
#'    )
#' class(messages) <- c("claude", class(messages))
#' model <- "claude-3-5-sonnet-20241022"
#' tokens <- claude_token_count(messages, model)
#' print(tokens)
claude_token_count <- function(messages, model, system = NULL, max_retries = 3, pause_cap = 1200, quiet = FALSE) {

  checkmate::assert_class(messages, "claude")
  # Construct the JSON body for the request
  body <- jsonlite::toJSON(
    list(
      messages = purrr::flatten(messages) |> unname(),
      model = model
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Make the API call using httr::RETRY to handle retries
  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.anthropic.com/v1/messages/count_tokens",
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "token-counting-2024-11-01",
      "content-type" = "application/json",
      "system" = system
    ),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check if the response contains an error
  httr::stop_for_status(res)
  response <- httr::content(res)

  # Return the number of input tokens
  response$input_tokens
}
