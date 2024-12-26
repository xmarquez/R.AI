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



