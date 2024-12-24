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
#' @exportS3Method
completion.ollama_character <- function(prompt, model,
                                        suffix = NULL, images = NULL,
                                        options = NULL, system = NULL,
                                        template = NULL,
                                        stream = FALSE, raw = FALSE,
                                        keep_alive = "5m", ...) {
  body <- list(
    model = model,
    prompt = prompt,
    suffix = suffix,
    images = images,
    options = options,
    system = system,
    template = template,
    stream = stream,
    raw = raw,
    keep_alive = keep_alive
  ) |> purrr::compact()

  res <- httr::POST(
    url = "http://localhost:11434/api/generate",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  res <- httr::content(res)
  structure(res, class = c("ollama_completion",
                           class(res)))
}

#' @exportS3Method
get_content.ollama_completion <- function(response) {
  response$response
}

#' @exportS3Method
get_usage.ollama_completion <- function(response) {
  response$response <- NULL
  response$context <- NULL
  dplyr::as_tibble(response)
}


#' @inheritParams chat.openai_list
#' @param options **Ollama only.** Model parameters such as `temperature`, in a
#'   named list. These are documented
#'   [here](https://github.com/ollama/ollama/blob/main/docs/modelfile.md#valid-parameters-and-values).
#' @param keep_alive **Ollama only.** Duration to keep the model in memory after
#'   the request. Defaults to 5m.
#' @rdname chat
#' @seealso
#'   <https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion>
#' @exportS3Method
chat.ollama_list <- function(messages,
                             model,
                             tools = NULL,
                             format = NULL,
                             options = NULL,
                             stream = FALSE,
                             keep_alive = "5m", ...) {
  body <- list(
    model = model,
    messages = messages,
    tools = tools,
    format = format,
    options = options,
    stream = stream,
    keep_alive = keep_alive
  ) |> purrr::compact()

  res <- httr::POST(
    url = "http://localhost:11434/api/chat",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  response <- httr::content(res)
  structure(response, class = c("ollama_chat", class(response)))
}

#' @exportS3Method
get_content.ollama_chat <- function(response) {
  response$message$content
}

#' @exportS3Method
get_message.ollama_chat <- function(response) {
  res <- response$message |>
    list() |>
    structure(class = c("ollama_list", "list"))

  res
}

#' @exportS3Method
get_usage.ollama_chat <- function(response) {
  dplyr::tibble(input_tokens = response$prompt_eval_count,
                output_tokens = response$eval_count) |>
    dplyr::mutate(total_tokens = input_tokens + output_tokens)
}

#' @rdname ollama-api
#' @export
list_models.ollama <- function(...) {
  res <- httr::GET(url = "http://localhost:11434/api/tags", ...)
  handle_errors(res)
  httr::content(res)
}

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
#' @exportS3Method
embed.ollama_character <- function(content, model, truncate = TRUE, options = NULL, keep_alive = "5m", ...) {
  body <- list(
    model = model,
    input = content,
    truncate = truncate,
    options = options,
    keep_alive = keep_alive
  ) |> purrr::compact()

  res <- httr::POST(
    url = "http://localhost:11434/api/embed",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  response <- httr::content(res)

  total_tokens <- response$prompt_eval_count

  response <- response$embeddings |>
    purrr::map(\(x) unlist(x))

  structure(response, class = c("embedding", class(response)), total_tokens = total_tokens,
            model = model)
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
