#' Retrieve Usage Information from a Chat or Completion Response
#'
#' A generic function to extract usage details (e.g., token counts) from response
#' objects returned by various language model (LLM) backends. Calling `get_usage()`
#' will return a standardized data frame (commonly a tibble) summarizing usage metrics
#' such as prompt tokens, completion tokens, total tokens, etc.
#'
#' @param response A response object returned by a chat or completion call.
#'   This object should contain usage metadata (e.g., `response$usage`,
#'   `response$usageMetadata`, or related fields), depending on the backend.
#'
#' @return A data frame containing usage statistics. The exact
#'   columns depend on the backend, but commonly include:
#'   \itemize{
#'     \item `input_tokens` (prompt tokens),
#'     \item `output_tokens` (completion tokens),
#'     \item `total_tokens`,
#'     \item `model`,
#'     \item other fields as provided by the specific backend.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## Claude example:
#' response_claude <- chat.claude_list(messages, model = "claude-v1")
#' usage_info <- get_usage(response_claude)
#' print(usage_info)
#'
#' ## Gemini example:
#' response_gemini <- chat.gemini_list(messages, model = "gemini-v1")
#' usage_info <- get_usage(response_gemini)
#' print(usage_info)
#' }
#'
#' @export
get_usage <- function(response) {
  UseMethod("get_usage")
}

#---------------------------
# S3 Methods (Inherit docs)
#---------------------------

#' @rdname get_usage
#' @exportS3Method get_usage default
get_usage.default <- function(response) {
  usage <- response$usage

  # Extract main columns
  prompt_tokens <- usage$prompt_tokens %||% NULL
  completion_tokens <- usage$completion_tokens %||% NULL
  total_tokens <- usage$total_tokens %||% NULL

  # Extract details columns (ensure they are lists or NULL)
  prompt_tokens_details <- (usage$prompt_tokens_details %||% NULL) |> dplyr::as_tibble()
  completion_tokens_details <- (usage$completion_tokens_details %||% NULL) |> dplyr::as_tibble()

  if (nrow(prompt_tokens_details) == 0) {
    prompt_tokens_details <- NULL
  }
  if (nrow(completion_tokens_details) == 0) {
    completion_tokens_details <- NULL
  }

  tibble::tibble(
    input_tokens = prompt_tokens,
    output_tokens = completion_tokens,
    total_tokens = total_tokens,
    input_tokens_details = list(prompt_tokens_details),
    output_tokens_details = list(completion_tokens_details),
    model = response$model
  )
}

#' @rdname get_usage
#' @exportS3Method get_usage claude_chat
get_usage.claude_chat <- function(response) {
  input_tokens <- output_tokens <- cache_creation_input_tokens <- cache_read_input_tokens <- NULL
  response$usage |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      total_tokens = input_tokens + output_tokens,
      input_tokens_details = list(
        dplyr::tibble(cache_creation_input_tokens, cache_read_input_tokens)
      ),
      model = response$model
    ) |>
    dplyr::select(-cache_creation_input_tokens, -cache_read_input_tokens)
}

#' @rdname get_usage
#' @exportS3Method get_usage gemini_chat
get_usage.gemini_chat <- function(response) {
  if (is.null(response$usageMetadata$candidatesTokenCount)) {
    response$usageMetadata$candidatesTokenCount <- 0
  }

  response$usageMetadata |>
    dplyr::as_tibble() |>
    dplyr::rename(
      input_tokens = "promptTokenCount",
      output_tokens = "candidatesTokenCount",
      total_tokens = "totalTokenCount"
    ) |>
    dplyr::mutate(model = response$modelVersion)
}

#' @rdname get_usage
#' @exportS3Method get_usage llama_cpp_completion
get_usage.llama_cpp_completion <- function(response) {
  model <- tibble::tibble(model = response$model)
  tokens <- tibble::tibble(
    tokens_cached = response$tokens_cached,
    tokens_evaluated = response$tokens_evaluated,
    tokens_predicted = response$tokens_predicted,
    truncated = response$truncated
  )
  dplyr::bind_cols(tokens, model)
}

#' @rdname get_usage
#' @exportS3Method get_usage ollama_chat
get_usage.ollama_chat <- function(response) {
  input_tokens <- output_tokens <- model <- NULL
  dplyr::tibble(
    input_tokens = response$prompt_eval_count,
    output_tokens = response$eval_count
  ) |>
    dplyr::mutate(total_tokens = input_tokens + output_tokens,
                  model = response$model)
}

#' @rdname get_usage
#' @exportS3Method get_usage ollama_completion
get_usage.ollama_completion <- function(response) {
  response$response <- NULL
  response$context <- NULL
  dplyr::as_tibble(response)
}

#' @rdname get_usage
#' @exportS3Method get_usage cohere_chat
get_usage.cohere_chat <- function(response) {
  input_tokens <- output_tokens <- model <- NULL
  dplyr::tibble(
    input_tokens = response$usage$tokens$input_tokens,
    billed_input_tokens = response$usage$billed_units$input_tokens,
    output_tokens = response$usage$tokens$output_tokens,
    billed_output_tokens = response$usage$billed_units$output_tokens
  ) |>
    dplyr::mutate(total_tokens = input_tokens + output_tokens,
                  model = "unknown")
}

#' @rdname get_usage
#' @exportS3Method get_usage deepseek_chat
get_usage.deepseek_chat <- function(response) {
  usage <- response$usage

  if (is.null(usage)) {
    # If there's no usage info, return an NA-filled row with the model at least
    return(
      tibble::tibble(
        input_tokens              = NA_integer_,
        output_tokens             = NA_integer_,
        total_tokens              = NA_integer_,
        prompt_cache_hit_tokens   = NA_integer_,
        prompt_cache_miss_tokens  = NA_integer_,
        model                     = response$model %||% NA_character_
      )
    )
  }

  tibble::tibble(
    # These fields come from DeepSeek's usage object
    input_tokens             = usage$prompt_tokens            %||% 0,
    output_tokens            = usage$completion_tokens         %||% 0,
    total_tokens             = usage$total_tokens              %||% 0,
    prompt_cache_hit_tokens  = usage$prompt_cache_hit_tokens   %||% 0,
    prompt_cache_miss_tokens = usage$prompt_cache_miss_tokens  %||% 0,
    model                    = response$model %||% NA_character_
  )
}
