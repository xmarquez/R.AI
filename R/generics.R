
#' @export
tool_calls <- function(response) {
  UseMethod("tool_calls")
}

#' @export
tokenize <- function(content, ...) {
  UseMethod("tokenize")
}

#' @export
detokenize <- function(tokens, ...) {
  UseMethod("detokenize")
}

#' @exportS3Method
as.matrix.embedding <- function(x, ...) {
  ncol <- length(x[[1]])
  nrow <- length(x)

  res <- matrix(unlist(x), nrow = nrow, ncol = ncol)
  res
}
