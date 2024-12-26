
#' @exportS3Method
as.matrix.embedding <- function(x, ...) {
  ncol <- length(x[[1]])
  nrow <- length(x)

  res <- matrix(unlist(x), nrow = nrow, ncol = ncol)
  res
}
