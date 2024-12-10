#' Call Embedding API
#'
#' A generic function to retrieve text embeddings by sending requests to various
#' APIs or a local LlamaFile instance.
#'
#' @param texts `character` A character vector containing the input texts to
#'   process. Must not be empty.
#' @param model `character` A string specifying the embedding model to use. The
#'   model must be supported by the chosen API. Use `get_available_models(mode =
#'   "embedding")` to identify the relevant models.
#' @param api `character` Specifies the API or method for embedding generation.
#'   Supported values are:
#'   * `"mistral"` - Dispatches to the [Mistral embedding API](https://docs.mistral.ai/api/#tag/agents/operation/agents_completion_v1_agents_completions_post).
#'   * `"openai"` - Dispatches to the [OpenAI embedding API](https://platform.openai.com/docs/api-reference/embeddings).
#'   * `"cohere"` - Dispatches to the [Cohere embedding API](https://docs.cohere.com/reference/embed).
#'   * `"voyage"` - Dispatches to the [Voyage embedding API](https://docs.voyageai.com/reference/embeddings-api).
#'   * `"llamafile"` - Dispatches to a local LlamaFile instance.
#' @param ... Additional arguments passed to API-specific implementations. See
#'   details for supported options.
#'
#' @return A `tibble` containing the embeddings and metadata, with the following
#'   columns:
#' * `id`: Identifier for each input text.
#' * `text_set`: Hash representing the batch of texts processed together.
#' * `embedding`: A list-column containing the embedding vector for each input text.
#'
#' @details The function dispatches requests to API-specific methods based on
#'   the value of `api`.
#'
#'   Additional arguments for specific APIs include:
#' * **LlamaFile**:
#'   * `llamafile_path`: Path to the LlamaFile model.
#'   * `quiet`: Logical, suppresses logging (default: `FALSE`).
#' * **Cohere**:
#'   * `input_type`: Type of input for embeddings (e.g., `"search_document"`).
#'   * `embedding_types`: List of desired embedding types (e.g., `"float"`).
#'   * `quiet`: Logical, suppresses logging (default: `FALSE`).
#' * **OpenAI**:
#'   * `quiet`: Logical, suppresses logging (default: `FALSE`).
#' * **Mistral**:
#'   * `quiet`: Logical, suppresses logging (default: `FALSE`).
#' * **Voyage**:
#'   * `input_type`: Specifies whether the input is `"query"`, `"document"`, or `NULL` (default: `"document"`).
#'   * `truncation`: Logical, whether to truncate input texts (default: `TRUE`).
#'   * `output_dimension`: Integer, specifies the output embedding dimension (optional).
#'   * `output_dtype`: Data type for embeddings (e.g., `"float"`, `"int8"`, etc., default: `"float"`).
#'   * `encoding_format`: Format for encoding embeddings (e.g., `"base64"` or `NULL` for raw lists).
#'   * `quiet`: Logical, suppresses logging (default: `FALSE`).
#'
#' @note Note that your API keys must be stored in an environment variable so
#'   they are accessible via [Sys.getenv()].
#'
#' @seealso [get_available_models()] for retrieving available models for
#'   specific APIs.
#'
#' @family embeddings
#' @export
call_embedding_api <- function(texts, model, api, ...) {
  class(texts) <- c(api, class(texts))
  UseMethod("call_embedding_api")
}


#' @export
call_embedding_api.llamafile <- function(texts, model, ...) {

  checkmate::assert_character(texts, min.len = 1)

  # Extract additional arguments from ...
  args <- list(...)
  llamafile_path <- args$llamafile_path %||% fs::dir_ls(recurse = TRUE, regexp = "mxbai-embed.+llamafile")
  quiet <- args$quiet %||% FALSE

  if(is_llamafile_running()) {
    running_model <- which_llamafile_running()
    if(!missing(model) && model != running_model) {
      stop("Running llamafile model is not the correct model. Kill current instance.")
    }
    model <- which_llamafile_running()
  } else {
    message("Starting LLamafile: ", llamafile_path)
    start_llamafile(llamafile_path)
    model <- which_llamafile_running()
  }

  # Ensure prompt IDs are defined
  ids <- seq_along(texts)

  # Initialize response collection
  responses <- tibble::tibble()

  # Generate a unique hash for the set
  text_set <- digest::digest(texts)

  # Loop over prompts and process each
  for (i in seq_along(texts)) {
    # Log the progress if enabled
    if (!quiet) {
      log_message <- glue::glue(
        "{lubridate::now()}: Embedding text {i} of {length(texts)} ",
        "with model '{model}' in set '{text_set}'"
      )
      message(log_message)
    }

    # Call the single request function
    response <- llamafile_embedding(texts[i], model = model)

    # Add prompt ID to the response
    response_df <- tibble::tibble(id = ids[i], text_set = text_set, embedding = list(response))
    responses <- dplyr::bind_rows(responses, response_df)
  }

  return(responses)

}

#' @export
call_embedding_api.openai <- function(texts, model, api, ...) {
  id <- NULL
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OpenAI API key is not set in environment variables.")

  # Extract additional arguments from ...
  args <- list(...)
  quiet <- args$quiet %||% FALSE

  # Validate inputs
  checkmate::assert_character(texts)
  checkmate::assert_string(model)

  # Fetch list of available OpenAI models
  available_models <- list_models("openai") |>
    dplyr::filter(stringr::str_detect(id, "embed")) |>
    dplyr::pull(id)

  # Check if the provided model is in the list
  if (!model %in% available_models) {
    stop("The specified model '", model, "' is not an available embedding model. ",
         "Available models: ", paste(available_models, collapse = ", "))
  }

  # Call the embedding function for OpenAI
  openai_embedding(texts = texts, model = model, api_key = api_key, quiet = quiet)
}

#' @export
call_embedding_api.mistral <- function(texts, model, api, ...) {
  id <- NULL
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if (api_key == "") stop("Mistral API key is not set in environment variables.")

  # Validate inputs
  checkmate::assert_character(texts)
  checkmate::assert_string(model)

  # Fetch list of available Mistral models
  available_models <- list_models("mistral") |>
    dplyr::filter(stringr::str_detect(id, "embed")) |>
    dplyr::pull(id)

  # Check if the provided model is in the list
  if (!model %in% available_models) {
    stop("The specified model '", model, "' is not an available embedding model. ",
         "Available models: ", paste(available_models, collapse = ", "))
  }

  # Call the embedding function for Mistral
  mistral_embedding(texts = texts, model = model, api_key = api_key, ...)
}

#' @export
call_embedding_api.cohere <- function(texts, model, api, ...) {
  api_key <- Sys.getenv("COHERE_API_KEY")
  if (api_key == "") stop("Cohere API key is not set in environment variables.")

  args <- list(...)
  input_type <- args$input_type %||% "search_document"
  embedding_types <- args$embedding_types %||% c("float")
  quiet <- args$quiet %||% FALSE

  # Validate inputs
  checkmate::assert_character(texts, min.len = 1)
  checkmate::assert_string(model)
  checkmate::assert_choice(input_type, c("search_document", "search_query", "classification", "clustering", "image"))
  checkmate::assert_subset(embedding_types, c("float", "int8", "uint8", "binary", "ubinary"))

  # Fetch available models and validate the model
  available_models <- c(
    "embed-english-v3.0", "embed-multilingual-v3.0",
    "embed-english-light-v3.0", "embed-multilingual-light-v3.0",
    "embed-english-v2.0", "embed-english-light-v2.0",
    "embed-multilingual-v2.0"
  )

  if (!model %in% available_models) {
    stop("The specified model '", model, "' is not an available embedding model. ",
         "Available models: ", paste(available_models, collapse = ", "))
  }

  # Call the embedding function for Cohere
  cohere_embedding(texts = texts, model = model, api_key = api_key,
                   input_type = input_type, embedding_types = embedding_types,
                   quiet = quiet)
}

#' @export
call_embedding_api.voyage <- function(texts, model, api, ...) {
  # Validate API key
  api_key <- Sys.getenv("VOYAGE_API_KEY")
  if (api_key == "") stop("Voyage API key is not set in environment variables.")

  # Extract additional arguments
  args <- list(...)
  input_type <- args$input_type %||% "document"
  truncation <- args$truncation %||% TRUE
  output_dimension <- args$output_dimension %||% NULL
  output_dtype <- args$output_dtype %||% "float"
  quiet <- args$quiet %||% FALSE
  encoding_format <- args$encoding_format %||% NULL

  # Validate inputs
  checkmate::assert_character(texts, min.len = 1)
  checkmate::assert_string(model)
  checkmate::assert_choice(input_type, c(NULL, "query", "document"), null.ok = TRUE)
  checkmate::assert_flag(truncation)
  checkmate::assert_int(output_dimension, null.ok = TRUE)
  checkmate::assert_choice(output_dtype, c("float", "int8", "uint8", "binary", "ubinary"))
  checkmate::assert_choice(encoding_format, c(NULL, "base64"), null.ok = TRUE)

  # Validate model
  available_models <- c(
    "voyage-3", "voyage-3-lite", "voyage-code-3",
    "voyage-finance-2", "voyage-multilingual-2",
    "voyage-law-2", "voyage-code-2"
  )
  if (!model %in% available_models) {
    stop("The specified model '", model, "' is not a valid Voyage embedding model. ",
         "Available models: ", paste(available_models, collapse = ", "))
  }

  # Call the Voyage embedding function
  voyage_embedding(
    texts = texts,
    model = model,
    api_key = api_key,
    input_type = input_type,
    truncation = truncation,
    output_dimension = output_dimension,
    output_dtype = output_dtype,
    quiet = quiet
  )
}


