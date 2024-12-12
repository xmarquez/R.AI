#' Upload, List, Retrieve, Delete, or Download Files in OpenAI
#'
#' These functions interact with the OpenAI [Files
#' API](https://platform.openai.com/docs/api-reference/files). See the
#' documentation there for more.
#'
#' @param file_path Character. The path to the file to upload.
#' @param purpose Character. The purpose of the file. Must be one of "batch",
#'   "fine-tune", "assistants", or "vision". Default is "assistants".
#' @param file_id Character. The ID of the file to retrieve, delete, or
#'   download.
#' @param destination Character. The local path to save the downloaded file.
#' @return For `openai_upload_file`, a list containing the details of the
#'   uploaded file with fields:
#'   * `id`: string. The file identifier.
#'   * `bytes`: integer. Size of the file in bytes.
#'   * `created_at`: integer. Unix timestamp for file creation.
#'   * `filename`: string. Name of the file.
#'   * `object`: string. Object type, always "file".
#'   * `purpose`: string. Intended purpose of the file.
#'   * `status`: Deprecated. Current status of the file.
#'   * `status_details`: Deprecated. Details on validation failures.
#'
#'   For `openai_list_files`, a list of uploaded file objects.
#'
#'   For `openai_file_details`, the details of a specific file.
#'
#'   For `openai_delete_file`, a list containing the deletion status.
#'
#'   For `openai_download_file`, NULL. Saves the file to the specified location.
#' @details
#'   * Individual files can be up to 512 MB. The total size of files uploaded by one organization can be up to 100 GB.
#'   * The Assistants API supports files up to 2 million tokens and specific file types. See the [Assistants Tools guide](https://platform.openai.com/docs/assistants/tools) for details.
#'   * The Fine-tuning API only supports .jsonl files. See [chat](https://platform.openai.com/docs/api-reference/fine-tuning/chat-input) or [completions](https://platform.openai.com/docs/api-reference/fine-tuning/completions-input) formats.
#'   * The Batch API supports .jsonl files up to 200 MB in size. See the [required format](https://platform.openai.com/docs/api-reference/batch/request-input).
#' @rdname openai-files
#' @family openai
#' @family files
#' @export
openai_upload_file <- function(file_path, purpose = "assistants") {

  checkmate::assert_choice(purpose, c("assistants", "fine-tune", "vision", "batch"))
  checkmate::assert_file_exists(file_path)

  response <- httr::POST(
    url = "https://api.openai.com/v1/files",
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "multipart/form-data"
    ),
    body = list(
      file = httr::upload_file(file_path),
      purpose = purpose
    )
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-files
#' @export
openai_list_files <- function() {
  response <- httr::GET(
    url = "https://api.openai.com/v1/files",
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")))
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-files
#' @export
openai_file_details <- function(file_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/files/", file_id),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")))
  )


  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-files
#' @export
openai_delete_file <- function(file_id) {
  response <- httr::DELETE(
    url = paste0("https://api.openai.com/v1/files/", file_id),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")))
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-files
#' @export
openai_download_file <- function(file_id, destination) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/files/", file_id, "/content"),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    httr::write_disk(destination, overwrite = TRUE)
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  invisible(NULL)
}
