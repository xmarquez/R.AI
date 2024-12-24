#' Functions to interact with the llama.cpp server API
#'
#' See
#' [https://github.com/ggerganov/llama.cpp/tree/master/examples/server](https://github.com/ggerganov/llama.cpp/tree/master/examples/server)
#' for more details.
#'
#' @param ... Additional arguments passed to the HTTP request.
#' @rdname llama-cpp-api
#' @export
health.llama_cpp <- function(...) {
  res <- httr::GET(
    url = "http://localhost:8080/health"
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname llama-cpp-api
#' @export
props.llama_cpp <- function(...) {
  res <- httr::GET(
    url = "http://localhost:8080/props",
    ...
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname llama-cpp-api
#' @param prompt Text input for generation. Can be a vector of characters.
#' @param n_predict Maximum tokens to generate. Use -1 for unlimited.
#' @param temperature Sampling temperature.
#' @param top_k Top-k sampling.
#' @param top_p Top-p sampling.
#' @param stream Enable streaming mode.
#' @exportS3Method
completion.llama_cpp_character <- function(prompt,
                                           n_predict = -1,
                                           temperature = 0.8,
                                           top_k = 40,
                                           top_p = 0.95,
                                           stream = FALSE, ...) {
  checkmate::assert_string(prompt)
  body <- list(
    prompt = prompt,
    n_predict = n_predict,
    temperature = temperature,
    top_k = top_k,
    top_p = top_p,
    stream = stream,
    ...
  ) |> purrr::compact()

  res <- httr::RETRY(
    verb = "POST",
    url = "http://localhost:8080/completion",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = 5,
    pause_min = 1,
    pause_cap = 10
  )
  handle_errors(res)
  res <- httr::content(res)
  structure(res, class = c("llama_cpp_completion",
                           class(res)))
}

#' @rdname llama-cpp-api
#' @param content Text to tokenize.
#' @param add_special Include special tokens (default FALSE).
#' @param with_pieces Return token pieces (default FALSE).
#' @exportS3Method
tokenize.llama_cpp_character <- function(content, add_special = FALSE, with_pieces = FALSE, ...) {
  checkmate::assert_character(content)
  body <- list(
    content = content,
    add_special = add_special,
    with_pieces = with_pieces,
    ...
  ) |> purrr::compact()

  res <- httr::POST(
    url = "http://localhost:8080/tokenize",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  handle_errors(res)
  res <- httr::content(res)
  structure(res$tokens, class = c("llama_cpp_tokenlist",
                                  class(res)))
}

#' @rdname llama-cpp-api
#' @param content Text to generate embeddings for.
#' @exportS3Method
embed.llama_cpp_character <- function(content, ...) {

  checkmate::assert_character(content, min.len = 1)

  body <- list(
    content = content,
    ...
  ) |> purrr::compact()

  res <- httr::RETRY(
    verb = "POST",
    url = "http://localhost:8080/embedding",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = 5,
    pause_min = 1,
    pause_cap = 10
  )
  handle_errors(res)
  res <- httr::content(res)

  response <- res$results |>
    purrr::map(\(x) unlist(x$embedding))

  structure(response, class = c("embedding", class(response)))
}

#' @rdname chat
#' @seealso <https://github.com/ggerganov/llama.cpp/tree/master/examples/server#post-v1chatcompletions-openai-compatible-chat-completions-api>
#' @exportS3Method
chat.llama_cpp_list <- function(messages,
                                temperature = 0.8,
                                stop = NULL,
                                ...) {
  body <- list(
    messages = messages,
    temperature = temperature,
    stop = stop,
    ...
  ) |> purrr::compact()

  res <- httr::RETRY(
    verb = "POST",
    url = "http://localhost:8080/v1/chat/completions",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = 5,
    pause_min = 1,
    pause_cap = 10
  )
  handle_errors(res)

  res <- httr::content(res)

  structure(res, class = c("llama_cpp_chat",
                           class(res)))
}

#' @rdname llama-cpp-api
#' @param tokens A vector of token IDs to detokenize into text.
#' @exportS3Method
detokenize.llama_cpp_tokenlist <- function(tokens, ...) {
  body <- list(tokens = tokens)

  res <- httr::POST(
    url = "http://localhost:8080/detokenize",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)

  res <- httr::content(res)
  structure(res, class = c("llama_cpp_character",
                           class(res)))
}

#' @exportS3Method
get_content.llama_cpp_completion <- function(response) {
  return(response$content)
}

#' @exportS3Method
get_content.llama_cpp_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

#' @exportS3Method
get_message.llama_cpp_chat <- function(response) {
  res <- response$choices[[1]] |>
    purrr::pluck("message") |>
    list() |>
    structure(class = c("llama_cpp_list", "list"))

  res
}

#' @exportS3Method
get_usage.llama_cpp_completion <- function(response) {
  model <- tibble::tibble(model = response$model)
  settings <- response$generation_settings |>
    purrr::compact() |>
    tibble::as_tibble() |>
    dplyr::select(-samplers) |>
    dplyr::distinct()
  stop <- tibble::tibble(stop = response$stop, stopped_eos = response$stopped_eos,
                         stopped_limit = response$stopped_limit,
                         stopped_word = response$stopped_word,
                         stopping_word = response$stopping_word)
  tokens <- tibble::tibble(tokens_cached = response$tokens_cached,
                           tokens_evaluated = response$tokens_evaluated,
                           tokens_predicted = response$tokens_predicted,
                           truncated = response$truncated)
  timings <- tibble::as_tibble(response$timings)
  dplyr::bind_cols(settings, stop, timings, tokens)
}
