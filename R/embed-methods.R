#' Generate Embeddings from Text Content
#'
#' A generic function to generate vector embeddings for given text content using
#' various backends (OpenAI, Mistral, Gemini, Cohere, Voyage, Llama.CPP, Ollama, etc.).
#' You specify which backend by dispatching on the **class** of `content` or via
#' a helper function that sets the class. Each backend can accept additional
#' parameters (via `...`) depending on its API.
#'
#' @param content A character vector of one or more text items to be embedded.
#' @param model A character string specifying the model to be used. Defaults vary
#'   by backend (e.g., `"text-embedding-3-large"` for OpenAI).
#' @param quiet Logical, defaults to `FALSE`. If `TRUE`, suppresses most messages
#'   (like batch progress).
#' @param max_retries Integer, defaults to 3. Maximum number of retries on
#'   transient failures.
#' @param ... Additional arguments passed to the backend-specific method.
#'   Examples include:
#'   \itemize{
#'     \item \code{dimensions} (OpenAI)
#'     \item \code{input_type}, \code{embedding_types} (Cohere)
#'     \item \code{truncate}, \code{options} (Ollama)
#'     \item \code{batch_size} or \code{encoding_format} (Voyage)
#'   }
#'
#' @return A list of numeric vectors (one embedding per input text). The returned
#'   object has class \code{c("embedding", "list")} plus any additional classes.
#'   Some methods also attach attributes like `total_tokens` or `model`.
#'
#' @section Environment Variables:
#' Most remote backends require an API key, e.g.:
#' \itemize{
#'   \item `OPENAI_API_KEY` for OpenAI
#'   \item `MISTRAL_API_KEY` for Mistral
#'   \item `GEMINI_API_KEY` for Gemini
#'   \item `COHERE_API_KEY` for Cohere
#'   \item `VOYAGE_API_KEY` for Voyage
#' }
#'
#' @examples
#' \dontrun{
#' # Example: OpenAI embeddings
#' txt <- c("Hello world!", "Some more text")
#' emb_oa <- embed(txt, model = "text-embedding-3-large")
#' str(emb_oa)
#'
#' # Example: Mistral embeddings
#' txt_mistral <- c("Bonjour le monde")
#' emb_mi <- embed(txt_mistral, model = "mistral-embed")
#' str(emb_mi)
#'
#' # If calling Ollama (local Llama-based model):
#' emb_ol <- embed(txt, model = "my-ollama-model", truncate = TRUE)
#' str(emb_ol)
#' }
#'
#' @export
embed <- function(content,
                  model = NULL,
                  quiet = FALSE,
                  max_retries = 3,
                  ...) {
  UseMethod("embed")
}

#-------------------------------------------------------------------------------
# OPENAI
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed openai_character
embed.openai_character <- function(content,
                                   model = "text-embedding-3-large",
                                   quiet = FALSE,
                                   max_retries = 3,
                                   ...) {
  # Extract any backend-specific args from `...`
  dots <- list(...)
  dimensions <- dots$dimensions %||% NULL

  checkmate::assert_character(content)

  url <- "https://api.openai.com/v1/embeddings"

  payload <- list(
    model = model,
    input = as.list(content),
    dimensions = dimensions
  ) |> purrr::compact()

  headers <- httr::add_headers(
    Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  )

  if (!quiet) message("Sending request to OpenAI embedding API...")

  res <- retry_response(
    base_url = url,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    response_format = NULL,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    max_retries = max_retries,
    pause_cap = 1200,
    quiet = quiet
  )

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)

  total_tokens <- response$usage$total_tokens

  # Flatten the embeddings
  embeddings <- response$data |>
    purrr::map(\(x) unlist(x$embedding))

  structure(embeddings, class = c("embedding", class(embeddings)),
            total_tokens = total_tokens, model = model)
}

#-------------------------------------------------------------------------------
# MISTRAL
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed mistral_character
embed.mistral_character <- function(content,
                                    model = "mistral-embed",
                                    quiet = FALSE,
                                    max_retries = 3,
                                    ...) {
  checkmate::assert_character(content)

  url <- "https://api.mistral.ai/v1/embeddings"
  payload <- list(model = model, input = as.list(content))

  headers <- httr::add_headers(
    Authorization = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")),
    `Content-Type` = "application/json"
  )

  if (!quiet) message("Sending request to Mistral embedding API...")

  response <- httr::RETRY(
    verb = "POST",
    url = url,
    headers,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "json",
    times = max_retries,
    quiet = quiet,
    terminate_on = c(400:499)
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }
  response <- httr::content(response)

  total_tokens <- response$usage$total_tokens
  embeddings <- response$data |>
    purrr::map(\(x) unlist(x$embedding))

  structure(embeddings, class = c("embedding", class(embeddings)),
            total_tokens = total_tokens, model = model)
}

#-------------------------------------------------------------------------------
# GEMINI
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed gemini_character
embed.gemini_character <- function(content,
                                   model = "text-embedding-004",
                                   quiet = FALSE,
                                   max_retries = 10,
                                   ...) {
  checkmate::assert_character(content)

  url <- glue::glue(
    "https://generativelanguage.googleapis.com/v1beta/models/{model}:batchEmbedContents"
  )

  payload <- list(
    requests = purrr::map(as.list(content), ~list(
      model = paste0("models/", model),
      content = list(parts = list(list(text = .x)))
    ))
  )

  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)

  if (!quiet) message("Sending batch request to Gemini embedding API...")

  res <- httr::RETRY(
    verb = "POST",
    url = url,
    config = httr::add_headers("content-type" = "application/json"),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = 1200,
    quiet = quiet,
    terminate_on = c(400:499)
  )

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)

  if (!is.null(response$embeddings)) {
    embeddings <- purrr::map(response$embeddings, ~unlist(.x$values))
  } else {
    cli::cli_abort("Failed to retrieve embeddings: {response$error$message}")
  }

  structure(embeddings, class = c("embedding", class(embeddings)), model = model)
}

#-------------------------------------------------------------------------------
# COHERE
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed cohere_character
embed.cohere_character <- function(content,
                                   model = "embed-english-v3.0",
                                   quiet = FALSE,
                                   max_retries = 3,
                                   ...) {
  # Extract additional args from `...`
  dots <- list(...)
  input_type <- dots$input_type %||% "search_document"
  embedding_types <- dots$embedding_types %||% "float"

  api_key <- Sys.getenv("COHERE_API_KEY")
  if (api_key == "") {
    stop("Cohere API key not set in environment variables.")
  }

  checkmate::assert_choice(input_type,
                           c("search_document", "search_query", "classification",
                             "clustering", "image"))
  checkmate::assert_subset(embedding_types,
                           c("float", "int8", "uint8", "binary", "ubinary"))

  checkmate::assert_character(content, min.len = 1)

  # Batch the content if needed
  batch_size <- 96
  text_batches <- split(content, ceiling(seq_along(content) / batch_size))

  all_embeddings <- list()
  total_tokens <- 0

  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json",
    `X-Client-Name` = "cohere_r_package"
  )

  for (batch_idx in seq_along(text_batches)) {
    batch <- text_batches[[batch_idx]]

    if (!quiet) {
      message("Processing batch ", batch_idx, " of ", length(text_batches), "...")
    }

    payload <- list(
      model = jsonlite::unbox(model),
      input_type = jsonlite::unbox(input_type),
      embedding_types = embedding_types,
      texts = batch
    )

    response <- httr::RETRY(
      verb = "POST",
      url = "https://api.cohere.com/v2/embed",
      headers,
      body = jsonlite::toJSON(payload),
      encode = "json",
      times = max_retries,
      quiet = quiet,
      terminate_on = c(400:499)
    )

    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
    }
    result <- httr::content(response)

    embeddings <- purrr::map(result$embeddings[[embedding_types[[1]]]], ~.x)
    total_tokens <- total_tokens + result$meta$billed_units$input_tokens

    all_embeddings <- c(all_embeddings, embeddings)
  }

  structure(all_embeddings, class = c("embedding", class(all_embeddings)),
            total_tokens = total_tokens, model = model)
}

#-------------------------------------------------------------------------------
# VOYAGE
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed voyage_character
embed.voyage_character <- function(content,
                                   model = "voyage-3-lite",
                                   quiet = FALSE,
                                   max_retries = 3,
                                   ...) {
  # Extract additional args
  dots <- list(...)
  input_type <- dots$input_type %||% NULL
  truncation <- dots$truncation %||% TRUE
  output_dimension <- dots$output_dimension %||% NULL
  output_dtype <- dots$output_dtype %||% "float"
  encoding_format <- dots$encoding_format %||% NULL

  api_key <- Sys.getenv("VOYAGE_API_KEY")
  if (api_key == "") {
    stop("Voyage API key is not set in environment variables.")
  }

  checkmate::assert_character(content, min.len = 1)

  # Batch processing
  batch_size <- 128
  text_batches <- split(content, ceiling(seq_along(content) / batch_size))

  all_embeddings <- list()
  total_tokens <- 0

  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  for (batch_idx in seq_along(text_batches)) {
    batch <- text_batches[[batch_idx]]
    if (!quiet) {
      message("Processing batch ", batch_idx, " of ", length(text_batches), "...")
    }

    payload <- list(
      input = list(batch),
      model = model,
      input_type = input_type,
      truncation = truncation,
      output_dimension = output_dimension,
      output_dtype = output_dtype,
      encoding_format = encoding_format
    ) |> purrr::flatten()

    res <- httr::RETRY(
      verb = "POST",
      url = "https://api.voyageai.com/v1/embeddings",
      headers,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      encode = "json",
      times = max_retries,
      quiet = quiet,
      terminate_on = c(400, 401, 403, 404)
    )

    if (httr::http_error(res)) {
      cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
    }
    result <- httr::content(res)

    # Each item in result$data is an embedding
    if (is.null(encoding_format)) {
      embeddings <- purrr::map(result$data, \(x) unlist(x$embedding))
    } else if (encoding_format == "base64") {
      embeddings <- purrr::map(result$data, \(x) {
        base64enc::base64decode(x$embedding) |> unlist()
      })
    } else {
      stop("Unknown encoding format: ", encoding_format)
    }

    total_tokens <- total_tokens + result$usage$total_tokens
    all_embeddings <- c(all_embeddings, embeddings)
  }

  structure(all_embeddings, class = c("embedding", class(all_embeddings)),
            total_tokens = total_tokens, model = model)
}

#-------------------------------------------------------------------------------
# Llama.CPP
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed llama_cpp_character
embed.llama_cpp_character <- function(content,
                                      model = NULL,
                                      quiet = FALSE,
                                      max_retries = 3,
                                      ...) {
  checkmate::assert_character(content, min.len = 1)

  dots <- list(...)
  body <- list(
    content = content,
    model = model
  ) |>
    purrr::compact() |>
    c(dots) # add any extra fields from `dots`

  # We'll do up to `max_retries` with httr::RETRY if desired
  res <- httr::RETRY(
    verb = "POST",
    url = "http://localhost:8080/embedding",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = max_retries,
    pause_min = 1,
    pause_cap = 10,
    quiet = quiet,
    terminate_on = c(400:499)
  )
  handle_errors(res)
  response <- httr::content(res)$results
  embeddings <- purrr::map(response, \(x) unlist(x$embedding))

  structure(embeddings, class = c("embedding", class(embeddings)), model = model)
}

#-------------------------------------------------------------------------------
# OLLAMA
#-------------------------------------------------------------------------------
#' @rdname embed
#' @exportS3Method embed ollama_character
embed.ollama_character <- function(content,
                                   model,
                                   quiet = FALSE,
                                   max_retries = 3,
                                   ...) {
  dots <- list(...)
  truncate <- dots$truncate %||% TRUE
  options  <- dots$options %||% NULL
  keep_alive <- dots$keep_alive %||% "5m"

  body <- list(
    model = model,
    input = content,
    truncate = truncate,
    options = options,
    keep_alive = keep_alive
  ) |> purrr::compact()

  res <- httr::RETRY(
    verb = "POST",
    url = "http://localhost:11434/api/embed",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = max_retries,
    quiet = quiet,
    terminate_on = c(400:499)
  )
  handle_errors(res)
  response <- httr::content(res)

  total_tokens <- response$prompt_eval_count

  embeddings <- response$embeddings |>
    purrr::map(\(x) unlist(x))

  structure(embeddings, class = c("embedding", class(embeddings)),
            total_tokens = total_tokens, model = model)
}
