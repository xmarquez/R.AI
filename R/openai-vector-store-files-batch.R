#' Manage Vector Store File Batches in OpenAI
#'
#' These functions interact with the OpenAI [Vector Store File Batches
#' API](https://platform.openai.com/docs/api-reference/vector-stores-file-batches).
#' See the documentation there for more details.
#'
#' @param vector_store_id Character. Required. The ID of the vector store.
#' @param batch_id Character. Optional. The ID of the file batch.
#' @param file_ids Character vector. Required for file batch creation. A list of
#'   File IDs to include in the file batch.
#' @param chunking_strategy List. Optional. The chunking strategy used for the
#'   files in the batch. Defaults to "auto".
#' @param limit Integer. Optional. Maximum number of files to return. Defaults
#'   to 20.
#' @param order Character. Optional. Sort order for results. One of "asc" or
#'   "desc" (default: "desc").
#' @param after Character. Optional. Cursor for pagination after a specific file
#'   ID.
#' @param before Character. Optional. Cursor for pagination before a specific
#'   file ID.
#' @param filter Character. Optional. Filter files by status (e.g.,
#'   "in_progress", "completed").
#' @return A list containing details of the file batch or its files, depending
#'   on the function used. See each function for specific return fields.
#'
#' @rdname openai-vector-store-files-batch
#' @family openai
#' @family vector stores
#' @family files
#' @export
openai_create_vector_store_file_batch <- function(vector_store_id, file_ids, chunking_strategy = NULL) {
  body <- list(
    file_ids = file_ids,
    chunking_strategy = chunking_strategy
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/file_batches"),
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

#' @rdname openai-vector-store-files-batch
#' @export
openai_get_vector_store_file_batch <- function(vector_store_id, batch_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/file_batches/", batch_id),
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

#' @rdname openai-vector-store-files-batch
#' @export
openai_cancel_vector_store_file_batch <- function(vector_store_id, batch_id) {
  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/file_batches/", batch_id, "/cancel"),
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

#' @rdname openai-vector-store-files-batch
#' @export
openai_list_vector_store_files_in_batch <- function(vector_store_id, batch_id, limit = 20, order = "desc", after = NULL, before = NULL, filter = NULL) {
  query_params <- list(
    limit = limit,
    order = order,
    after = after,
    before = before,
    filter = filter
  )
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/vector_stores/", vector_store_id, "/file_batches/", batch_id, "/files"),
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
