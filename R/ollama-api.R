#' @title Ollama API
#' @description Functions to interact with the Ollama server API.
#' @details For details about the API endpoints, visit
#'   https://github.com/ollama/ollama/blob/main/docs/api.md.
#' @param model The model to use (required for applicable endpoints).
#' @param prompt The input prompt (required for completion endpoints).
#' @param messages A list of messages for chat completion (required for chat
#'   endpoints).
#' @param suffix Text to append after the model response.
#' @param images A list of base64-encoded images for multimodal models.
#' @param system A system message to customize the behavior of the model.
#' @param template A prompt template to use.
#' @param stream Logical; whether to stream responses (default: FALSE).
#' @param raw Logical; whether to disable formatting (default: FALSE).
#' @param keep_alive Duration to keep the model in memory after the request.
#' @param verbose Logical; whether to return verbose information (default:
#'   FALSE).
#' @param tools Tools for the model to use in chat completions.
#' @param truncate Logical; whether to truncate inputs that exceed the model's
#'   context length.
#' @param input The input text or list of texts for embeddings.
#' @param insecure Logical; whether to allow insecure connections (for pull/push
#'   endpoints).
#' @param modelfile The content of the Modelfile for model creation.
#' @param path The path to the Modelfile for model creation.
#' @param quantize Quantization type for model creation.
#' @param source The source model for copying.
#' @param destination The destination model for copying.
#' @param ... Additional arguments passed to the HTTP request.
#' @return A list with fields described in the API documentation.
#' @rdname ollama-api
#' @export
show_model_ollama <- function(model, verbose = FALSE, ...) {
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
pull_model_ollama <- function(model, insecure = FALSE, stream = FALSE, ...) {
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
push_model_ollama <- function(model, insecure = FALSE, stream = FALSE, ...) {
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
create.ollama <- function(model, modelfile = NULL, path = NULL, quantize = NULL, stream = TRUE, ...) {
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
delete.ollama <- function(model, ...) {
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
copy.ollama <- function(source, destination, ...) {
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
list_running_models.ollama <- function(...) {
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
