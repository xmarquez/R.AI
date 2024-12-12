#' Create, list, update, retrieve, or delete a Vector Store in OpenAI
#'
#' These functions interact with the OpenAI [vector store
#' API](https://platform.openai.com/docs/api-reference/vector-stores). See the
#' documentation there for more.
#'
#' @param vector_store_id Character. ID of the vector store to retrieve, update,
#'   or delete.
#' @param file_ids Character vector. Optional. A list of File IDs that the
#'   vector store should use. Useful for tools like `file_search` that can
#'   access files.
#' @param name Character. Optional. The name of the vector store.
#' @param expires_after List. Optional. The expiration policy for the vector
#'   store. Specify expiration details as a list object with `anchor` and `days`
#'   fields:
#'   * `anchor`: Timestamp after which the expiration policy applies. Supported anchors: `last_active_at`.
#'   * `days`: The number of days after the anchor time that the vector store will expire.
#' @param chunking_strategy List. Optional. The chunking strategy used to chunk
#'   the file(s). If not set, the `auto` strategy will be used. Only applicable
#'   if `file_ids` is non-empty.
#' @param metadata List. Optional. A set of up to 16 key-value pairs that can be
#'   attached to an object. Keys can be a maximum of 64 characters long, and
#'   values can be a maximum of 512 characters long.
#' @param limit Integer. Optional. A limit on the number of vector stores to
#'   return. The limit can range between 1 and 100, with a default value of 20.
#' @param order Character. Optional. Sort order by the `created_at` timestamp of
#'   the objects. Defaults to `"desc"` for descending order. Use `"asc"` for
#'   ascending order.
#' @param after Character. Optional. A cursor for use in pagination. The `after`
#'   parameter is an object ID that defines your place in the list. For example,
#'   if a list request returns 100 objects ending with `obj_foo`, a subsequent
#'   call can include `after=obj_foo` to fetch the next page of the list.
#' @param before Character. Optional. A cursor for use in pagination. The
#'   `before` parameter is an object ID that defines your place in the list. For
#'   example, if a list request returns 100 objects starting with `obj_foo`, a
#'   subsequent call can include `before=obj_foo` to fetch the previous page of
#'   the list.
#' @return For `openai_create_vector_store`, `openai_create_vector_store`, and
#'   `openai_get_vector_store`, a list containing the created vector store
#'   object, which includes the following fields:
#'   * `id`: string. The identifier, which can be referenced in API endpoints.
#'   * `object`: string. The object type, which is always `"vector_store"`.
#'   * `created_at`: integer. The Unix timestamp (in seconds) for when the vector store was created.
#'   * `name`: string. The name of the vector store.
#'   * `usage_bytes`: integer. The total number of bytes used by the files in the vector store.
#'   * `file_counts`: object. An object showing the counts of files in the vector store.
#'   * `status`: string. The status of the vector store, which can be either `"expired"`, `"in_progress"`, or `"completed"`. A status of `"completed"` indicates that the vector store is ready for use.
#'   * `expires_after`: object. The expiration policy for the vector store.
#'   * `expires_at`: integer or `NULL`. The Unix timestamp (in seconds) for when the vector store will expire.
#'   * `last_active_at`: integer or `NULL`. The Unix timestamp (in seconds) for when the vector store was last active.
#'   * `metadata`: map. A set of 16 key-value pairs that can be attached to an object. Keys can be a maximum of 64 characters long, and values can be a maximum of 512 characters long.
#'
#'   For `openai_list_vector_stores`, a list of vector store objects.
#'
#'   For `openai_delete_vector_store`, a list with the deletion status.
#' @rdname openai-vector-stores
#' @family openai
#' @family vector stores
#' @export
openai_create_vector_store <- function(file_ids = NULL,
                                       name = NULL,
                                       expires_after = NULL,
                                       chunking_strategy = NULL,
                                       metadata = NULL) {
  body <- list(
    file_ids = file_ids,
    name = name,
    expires_after = expires_after,
    chunking_strategy = chunking_strategy,
    metadata = metadata
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = "https://api.openai.com/v1/vector_stores",
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


#' @rdname openai-vector-stores
#' @export
openai_list_vector_stores <- function(limit = 20, order = "desc", after = NULL, before = NULL) {
  query_params <- list(limit = limit, order = order, after = after, before = before)
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = "https://api.openai.com/v1/vector_stores",
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `OpenAI-Beta` = "assistants=v2"
    ),
    query = query_params
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-vector-stores
#' @export
openai_get_vector_store <- function(vector_store_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `OpenAI-Beta` = "assistants=v2"
    )
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}


#' @rdname openai-vector-stores
#' @export
openai_update_vector_store <- function(vector_store_id, name = NULL, expires_after = NULL, metadata = NULL) {
  body <- list(
    name = name,
    expires_after = expires_after,
    metadata = metadata
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id),
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

#' @rdname openai-vector-stores
#' @export
openai_delete_vector_store <- function(vector_store_id) {
  response <- httr::DELETE(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `OpenAI-Beta` = "assistants=v2"
    )
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}
