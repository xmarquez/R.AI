#' List or Retrieve Run Steps in OpenAI
#'
#' These functions interact with the OpenAI [Run Steps API](https://platform.openai.com/docs/api-reference/runs/steps). See the documentation there for more details.
#'
#' @param thread_id Character. Required. The ID of the thread the run and run steps belong to.
#' @param run_id Character. Required. The ID of the run the run steps belong to.
#' @param step_id Character. Required for retrieving a specific run step. The ID of the run step to retrieve.
#' @param limit Integer. Optional. Maximum number of run steps to retrieve. Defaults to 20.
#' @param order Character. Optional. Sort order by creation timestamp. One of "asc" or "desc" (default: "desc").
#' @param after Character. Optional. Cursor for pagination after a specific run step ID.
#' @param before Character. Optional. Cursor for pagination before a specific run step ID.
#' @param include List or NULL. Optional. A list of additional fields to include in the response.
#' @return For `openai_list_run_steps`, a list containing run step objects.
#'
#' For `openai_get_run_step`, the details of a specific run step, including:
#'   * `id`: string. The identifier of the run step.
#'   * `status`: string. Status of the run step (`"in_progress"`, `"cancelled"`, `"failed"`, `"completed"`, or `"expired"`).
#'   * `step_details`: object. Details of the run step (e.g., message creation or tool calls).
#'   * `usage`: object. Token usage statistics for the run step.
#' @rdname openai-run-steps
#' @family run
#' @family openai
#' @family assistants
#' @export
openai_list_run_steps <- function(thread_id, run_id, limit = 20, order = "desc", after = NULL, before = NULL, include = NULL) {
  query_params <- list(
    limit = limit,
    order = order,
    after = after,
    before = before,
    `include[]` = list(include)
  )
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/steps"),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    query = query_params
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-run-steps
#' @export
openai_get_run_step <- function(thread_id, run_id, step_id, include = NULL) {
  query_params <- list(include = include)
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/steps/", step_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    query = query_params
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}
