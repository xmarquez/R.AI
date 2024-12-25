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

