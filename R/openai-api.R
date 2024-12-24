#'
#' @param store **Openai only**. Logical or `NULL`. Whether or not to store the output of this
#'   chat completion request for use in OpenAI's model distillation or evals
#'   products. Defaults to `NULL` (meaning don't send the parameter unless
#'   needed).
#' @param metadata **Openai only**. A named list or `NULL`, containing developer-defined tags and
#'   values used for filtering completions in the dashboard.
#' @param frequency_penalty Numeric or `NULL`. A value between -2.0 and 2.0 that
#'   penalizes new tokens based on their existing frequency in the text so far,
#'   reducing the likelihood of repeating the same line verbatim. Defaults to
#'   `NULL` (no penalty sent).
#' @param logit_bias A named list or `NULL`, mapping tokens (by their token ID)
#'   to a numeric bias value from -100 to 100. Adjusts the likelihood of
#'   specified tokens appearing in the completion. Defaults to `NULL` (no bias).
#' @param logprobs Logical or `NULL`, defaults to `NULL`. Whether to return log
#'   probabilities of the output tokens. If `TRUE`, returns log probabilities of
#'   each output token in the message content.
#' @param top_logprobs Integer or `NULL`, defaults to `NULL`. Number of top
#'   tokens to return with log probabilities at each token position. Must be
#'   used with `logprobs = TRUE`.
#' @param max_tokens Integer or `NULL`. The maximum number of tokens to
#'   generate. For Openai, this is deprecated in favor of
#'   `max_completion_tokens`. Not compatible with o1 series models.
#' @param max_completion_tokens **Openai only**. Integer or `NULL`, defaults to
#'   `NULL`. The upper bound for the number of tokens generated for the
#'   completion, including visible and reasoning tokens.
#' @param n Integer or `NULL`, defaults to 1. How many chat completion choices
#'   to generate for each input message. This is only implemented in the Openai
#'   and Mistral APIs for now.
#' @param modalities Character vector or `NULL`, specifying desired output
#'   modalities. Typically `c("text")` or for certain models `c("text",
#'   "audio")`.
#' @param prediction **Openai only**. List or `NULL`. Configuration for a predicted output,
#'   improving response times if large parts of the response are already known.
#' @param audio **Openai only**. List or `NULL`. Parameters for audio output. Required when audio
#'   output is requested.
#' @param response_format List or `NULL`. An object specifying the format the
#'   model must output. For example, `list(type = "json_object")` ensures the
#'   model produces JSON.
#' @param seed Integer or `NULL`, defaults to `NULL`. Attempts deterministic
#'   sampling if specified. Determinism not guaranteed.
#' @param service_tier **Openai only**. Character or `NULL`, defaults to `NULL`.
#'   Specifies the latency tier to use. Options: "auto", "default".
#' @param stop Character vector, single string, or `NULL`, defaults to `NULL`.
#'   Up to 4 sequences at which the API will stop generating further tokens.
#' @param parallel_tool_calls Logical, defaults to `NULL`. Whether to enable
#'   parallel tool calls.
#' @param user String or `NULL`, defaults to `NULL`. A unique identifier
#'   representing the end-user.
#' @param json_mode Logical, defaults to `FALSE`. If `TRUE`, sets
#'   `response_format` to produce JSON output. Make sure to provide proper
#'   system/user instructions to the model to produce valid JSON.
#'
#' @return An object of class `"openai_chat"` containing the response from the
#'   OpenAI API.
#'
#' @seealso <https://platform.openai.com/docs/api-reference/chat/create>
#' @rdname chat
#'
#' @exportS3Method
chat.openai_list <- function(messages,
                             model = get_default_model("openai"),
                             store = NULL,
                             metadata = NULL,
                             frequency_penalty = NULL,
                             logit_bias = NULL,
                             logprobs = NULL,
                             top_logprobs = NULL,
                             max_tokens = NULL,
                             max_completion_tokens = NULL,
                             n = 1,
                             modalities = NULL,
                             prediction = NULL,
                             audio = NULL,
                             presence_penalty = NULL,
                             response_format = NULL,
                             seed = NULL,
                             service_tier = NULL,
                             stop = NULL,
                             temperature = 0.2,
                             top_p = NULL,
                             tools = NULL,
                             tool_choice = NULL,
                             parallel_tool_calls = NULL,
                             user = NULL,
                             json_mode = FALSE,
                             max_retries = 3,
                             quiet = FALSE) {

  # If json_mode is TRUE, ensure response_format is set to JSON mode
  if (json_mode) {
    response_format <- list(type = "json_object")
  }

  # Prepare the request body, only including parameters that are not NULL
  args_list <- list(
    messages = messages,
    model = model,
    store = store,
    metadata = metadata,
    frequency_penalty = frequency_penalty,
    logit_bias = logit_bias,
    logprobs = logprobs,
    top_logprobs = top_logprobs,
    max_tokens = max_tokens,
    max_completion_tokens = max_completion_tokens,
    n = n,
    modalities = modalities,
    prediction = prediction,
    audio = audio,
    presence_penalty = presence_penalty,
    response_format = response_format,
    seed = seed,
    service_tier = service_tier,
    stop = stop,
    temperature = temperature,
    top_p = top_p,
    tools = tools,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    user = user
  )

  # Remove NULL entries
  args_list <- args_list[!vapply(args_list, is.null, logical(1))]

  body <- jsonlite::toJSON(args_list, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "https://api.openai.com/v1/chat/completions",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = max_retries,
    pause_cap = 1200,
    quiet = quiet
  )

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }

  response <- httr::content(res)

  structure(response, class = c("openai_chat", class(response)))
}

#' @exportS3Method
get_content.openai_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

#' @exportS3Method
get_message.openai_chat <- function(response) {
  res <- response$choices[[1]] |>
    purrr::pluck("message") |>
    list() |>
    structure(class = c("openai_list", "list"))

  res
}

#' @exportS3Method
tool_calls.openai_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "tool_calls"))
}

#' @exportS3Method
embed.openai_character <- function(content,
                                       model = "text-embedding-3-large",
                                       dimensions = NULL,
                                       quiet = FALSE) {
  checkmate::assert_character(content)

  url <- "https://api.openai.com/v1/embeddings"

  # Prepare the request payload
  payload <- list(
    model = model,
    input = as.list(content),
    dimensions = dimensions
  ) |> purrr::compact()

  headers <- httr::add_headers(
    Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  )

  if (!quiet) message("Sending request to OpenAI embedding API...")

  res <- retry_response(
    base_url = url,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    response_format = NULL,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    max_retries = 3,
    pause_cap = 1200,
    quiet = quiet
  )

  # Handle response
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)

  total_tokens <- response$usage$total_tokens

  response <- response$data |>
    purrr::map(\(x) unlist(x$embedding))

  structure(response, class = c("embedding", class(response)),
            total_tokens = total_tokens, model = model)
}


