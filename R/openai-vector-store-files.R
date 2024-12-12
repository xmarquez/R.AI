#' Create, list, update, retrieve, or delete a Vector Store File in OpenAI
#'
#' These functions interact with the OpenAI [Vector Store Files
#' API](https://platform.openai.com/docs/api-reference/vector-stores-files). See
#' the documentation there for more.
#'
#' @param vector_store_id Character. Required. The ID of the vector store.
#'   Create a vector store with [openai_create_vector_store()] and find the id
#'   of an already created vector store with [openai_list_vector_stores()].
#' @param file_id Character. Required. The ID of the file that the vector store
#'   should use or retrieve. Upload a file with [openai_upload_file()], and get
#'   the id of already uploaded files with [openai_list_files()].
#' @param chunking_strategy List. Optional. The chunking strategy used to chunk
#'   the file(s). If not set, the `auto` strategy will be used.
#' @param limit Integer. Optional. A limit on the number of objects to return.
#'   Limit can range between 1 and 100, with a default of 20.
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
#' @param filter Character. Optional. Filter by file status. One of
#'   `"in_progress"`, `"completed"`, `"failed"`, or `"cancelled"`.
#'
#' @details `openai_delete_vector_store_file` removes the file from the vector
#'   store but does not delete the file itself. To delete the file completely,
#'   use [openai_delete_file()].
#'
#' @return For `openai_create_vector_store_file` and
#'   `openai_get_vector_store_file`, a list containing the created vector store
#'   file object, which includes:
#'   * `id`: string. The identifier of the file in the vector store.
#'   * `object`: string. The object type, which is always `"vector_store.file"`.
#'   * `created_at`: integer. The Unix timestamp (in seconds) for when the vector store file was created.
#'   * `usage_bytes`: integer. The total number of bytes used by the file.
#'   * `vector_store_id`: string. The ID of the vector store the file is attached to.
#'   * `status`: string. The status of the vector store file, which can be either `"in_progress"`, `"completed"`, or `"error"`.
#'   * `last_error`: string or `NULL`. Any error message associated with the file creation, or `NULL` if no error occurred.
#'
#'   For `openai_list_vector_store_files`, a list containing vector store file
#'   objects.
#'
#'   For `openai_delete_vector_store_file`, a list containing the deletion
#'   status.
#' @rdname openai-vector-store-files
#' @family openai
#' @family vector stores
#' @family files
#' @export
openai_create_vector_store_file <- function(vector_store_id,
                                            file_id,
                                            chunking_strategy = NULL) {
  body <- list(
    file_id = file_id,
    chunking_strategy = chunking_strategy
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/files"),
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


#' @rdname openai-vector-store-files
#' @export
openai_list_vector_store_files <- function(vector_store_id,
                                           limit = 20,
                                           order = "desc",
                                           after = NULL,
                                           before = NULL,
                                           filter = NULL) {
  query_params <- list(
    limit = limit,
    order = order,
    after = after,
    before = before,
    filter = filter
  )
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/files"),
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

#' @rdname openai-vector-store-files
#' @export
openai_get_vector_store_file <- function(vector_store_id, file_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/files/", file_id),
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

#' @rdname openai-vector-store-files
#' @export
openai_delete_vector_store_file <- function(vector_store_id, file_id) {
  response <- httr::DELETE(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/files/", file_id),
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
