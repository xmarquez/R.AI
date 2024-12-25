
#' @exportS3Method
tool_calls.openai_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "tool_calls"))
}


