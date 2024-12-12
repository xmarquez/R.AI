#' Create, list, update, retrieve, or delete Messages in a Thread in OpenAI
#'
#' These functions interact with the OpenAI [Messages
#' API](https://platform.openai.com/docs/api-reference/messages). See the
#' documentation there for more.
#'
#' @param thread_id Character. Required. The ID of the thread the message
#'   belongs to.
#' @param message_id Character. Optional. The ID of the message to retrieve,
#'   modify, or delete.
#' @param role Character. Required for message creation. The role of the entity
#'   creating the message. Allowed values are `"user"` or `"assistant"`.
#' @param content Character or List. Required for message creation. The content
#'   of the message. Can be a string or an array of text and/or images.
#' @param attachments List or `NULL`. Optional. A list of files attached to the
#'   message and the tools they should be added to.
#' @param metadata List. Optional. A set of key-value pairs to attach to the
#'   message or modify its existing metadata. Keys can be a maximum of 64
#'   characters long, and values can be a maximum of 512 characters long.
#' @param limit Integer. Optional. A limit on the number of objects to return.
#'   Defaults to 20, with a maximum of 100.
#' @param order Character. Optional. Sort order by the `created_at` timestamp.
#'   Defaults to `"desc"` for descending order. Use `"asc"` for ascending order.
#' @param after Character. Optional. A cursor for pagination to fetch results
#'   after this ID.
#' @param before Character. Optional. A cursor for pagination to fetch results
#'   before this ID.
#' @param run_id Character. Optional. Filter messages by the run ID that
#'   generated them.
#' @return For `openai_create_message`, a list containing the created message
#'   object with details such as:
#'   * `id`: string. The identifier of the message.
#'   * `object`: string. The object type, always `"thread.message"`.
#'   * `created_at`: integer. The Unix timestamp for when the message was created.
#'   * `thread_id`: string. The ID of the thread the message belongs to.
#'   * `role`: string. The entity that produced the message.
#'   * `content`: array. The content of the message.
#'   * `attachments`: array or `NULL`. Files attached to the message.
#'   * `metadata`: map. Key-value pairs attached to the message.
#'
#'   For `openai_list_messages`, a list of message objects in the thread, each
#'   with details such as:
#'   * `id`: string. The identifier of the message.
#'   * `content`: array. The content of the message.
#'   * `created_at`: integer. The Unix timestamp for when the message was created.
#'
#'   For `openai_get_message`, the details of the retrieved message object.
#'
#'   For `openai_modify_message`, the details of the modified message object.
#'
#'   For `openai_delete_message`, a list with the deletion status, including:
#'   * `id`: string. The identifier of the deleted message.
#'   * `object`: string. The object type, `"thread.message.deleted"`.
#'   * `deleted`: logical. Whether the message was successfully deleted.
#' @rdname openai-messages
#' @family openai
#' @family threads
#' @family messages
#' @export
openai_create_message <- function(thread_id, role, content, attachments = NULL, metadata = NULL) {
  body <- list(
    role = role,
    content = content,
    attachments = attachments,
    metadata = metadata
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/messages"),
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

#' @rdname openai-messages
#' @export
openai_list_messages <- function(thread_id, limit = 20, order = "desc", after = NULL, before = NULL, run_id = NULL) {
  query_params <- list(
    limit = limit,
    order = order,
    after = after,
    before = before,
    run_id = run_id
  )
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/messages"),
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

#' @rdname openai-messages
#' @export
openai_get_message <- function(thread_id, message_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id),
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

#' @rdname openai-messages
#' @export
openai_modify_message <- function(thread_id, message_id, metadata = NULL) {
  body <- list(metadata = metadata)
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id),
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

#' @rdname openai-messages
#' @export
openai_delete_message <- function(thread_id, message_id) {
  response <- httr::DELETE(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id),
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
