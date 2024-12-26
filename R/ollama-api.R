#' @title Ollama API Functions
#' @description Functions to interact with the Ollama API for managing models.
#'   These functions allow you to show information, pull, push, create, delete,
#'   and copy models, as well as list running models.
#' @name ollama-api
#' @section Model Format: Model names follow a `model:tag` format, where `model`
#'   can include an optional namespace. Examples: `orca-mini:3b-q4_1`,
#'   `llama3:70b`. The tag defaults to `latest` if not provided.
#' @details For full details on each endpoint, refer to the Ollama API
#'   documentation at <https://github.com/ollama/ollama/blob/main/docs/api.md>.
#' @export
#' @param model A string specifying the model name.
#' @param verbose Logical; if `TRUE`, returns verbose response fields. Default
#'   is `FALSE`.
#' @param insecure Logical; if `TRUE`, allows insecure connections. Default is
#'   `FALSE`.
#' @param stream Logical; if `TRUE`, enables streaming responses. Default is
#'   `FALSE`.
#' @param modelfile Character string or NULL; content of the Modelfile for model
#'   creation.
#' @param path Character string or NULL; file path for the Modelfile.
#' @param quantize Character string or NULL; quantization type for the model.
#' @param source A string specifying the source model for copy operations.
#' @param destination A string specifying the destination model for copy
#'   operations.
#' @param ... Other parameters passed to [httr::POST()] or [httr::GET()].
#' @return The parsed content of the API response.
#' @examples
#' \dontrun{
#' # Show model information
#' res <- ollama_show_model("llama3.2")
#'
#' # Create a new model
#' res <- ollama_create_model("new-model", modelfile = "FROM llama3")
#'
#' # List running models
#' res <- ollama_list_running_models()
#' }
#' @rdname ollama-api
#' @export
ollama_show_model <- function(model, verbose = FALSE, ...) {
  body <- list(
    model = model,
    verbose = verbose
  )
  res <- httr::POST(
    url = "http://localhost:11434/api/show",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname ollama-api
#' @export
ollama_pull_model <- function(model, insecure = FALSE, stream = FALSE, ...) {
  body <- list(
    model = model,
    insecure = insecure,
    stream = stream
  )
  res <- httr::POST(
    url = "http://localhost:11434/api/pull",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname ollama-api
#' @export
ollama_push_model <- function(model, insecure = FALSE, stream = FALSE, ...) {
  body <- list(
    model = model,
    insecure = insecure,
    stream = stream
  )
  res <- httr::POST(
    url = "http://localhost:11434/api/push",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  res
}

#' @rdname ollama-api
#' @export
ollama_create_model <- function(model, modelfile = NULL, path = NULL, quantize = NULL, stream = TRUE, ...) {
  body <- list(
    model = model,
    modelfile = modelfile,
    path = path,
    quantize = quantize,
    stream = stream
  ) |> purrr::compact()
  res <- httr::POST(
    url = "http://localhost:11434/api/create",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname ollama-api
#' @export
ollama_delete_model <- function(model, ...) {
  body <- list(
    model = model
  )
  res <- httr::DELETE(
    url = "http://localhost:11434/api/delete",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname ollama-api
#' @export
ollama_copy_model <- function(source, destination, ...) {
  body <- list(
    source = source,
    destination = destination
  )
  res <- httr::POST(
    url = "http://localhost:11434/api/copy",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  httr::content(res)
}

#' @rdname ollama-api
#' @export
ollama_list_running_models <- function(...) {
  res <- httr::GET(url = "http://localhost:11434/api/ps", ...)
  handle_errors(res)
  httr::content(res)
}

# Utility function for error handling
handle_errors <- function(res) {
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)}: {httr::content(res)}")
  }
}
