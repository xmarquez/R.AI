#' Manage Threads in OpenAI
#'
#' These functions interact with the OpenAI [Threads
#' API](https://platform.openai.com/docs/api-reference/threads). See the
#' documentation there for more details.
#'
#' @param messages List. Optional. A list of messages to start the thread with.
#'   Each message should include properties such as `role` and `content`. These
#'   can be generated with [prompt_list()].
#' @param tool_resources List or `NULL`. Optional. A set of resources made
#'   available to the assistant's tools in this thread.
#' @param metadata List. Optional. A set of 16 key-value pairs to attach to the
#'   thread. Keys can be a maximum of 64 characters long, and values can be a
#'   maximum of 512 characters long.
#' @param thread_id Character. Required for thread retrieval, modification, or
#'   deletion. The ID of the thread to interact with.
#' @return Depending on the function used:
#'   * `openai_create_thread`: A list containing the created thread object.
#'   * `openai_get_thread`: The details of the specified thread.
#'   * `openai_modify_thread`: The updated thread object with modified metadata or tool resources.
#'   * `openai_delete_thread`: The deletion status, including whether the thread was successfully deleted.
#' @rdname openai-threads
#' @family openai
#' @family threads
#' @export
openai_create_thread <- function(messages = NULL, tool_resources = NULL, metadata = NULL) {
  body <- list(
    messages = messages,
    tool_resources = tool_resources,
    metadata = metadata
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = "https://api.openai.com/v1/threads",
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-threads
#' @export
openai_get_thread <- function(thread_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    )
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-threads
#' @export
openai_modify_thread <- function(thread_id, tool_resources = NULL, metadata = NULL) {
  body <- list(
    tool_resources = tool_resources,
    metadata = metadata
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/threads/", thread_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-threads
#' @export
openai_delete_thread <- function(thread_id) {
  response <- httr::DELETE(
    url = paste0("https://api.openai.com/v1/threads/", thread_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    )
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}
