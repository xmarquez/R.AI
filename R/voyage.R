voyage_embedding <- function(
    texts, model, api_key,
    input_type = NULL, truncation = TRUE,
    output_dimension = NULL, output_dtype = "float",
    encoding_format = NULL, quiet = FALSE
) {


  # Batch processing: Maximum batch size is 128 texts
  batch_size <- 128
  text_batches <- split(texts, ceiling(seq_along(texts) / batch_size))

  all_responses <- dplyr::tibble()
  for (batch_idx in seq_along(text_batches)) {
    batch <- text_batches[[batch_idx]]

    if (!quiet) {
      message("Processing batch ", batch_idx, " of ", length(text_batches), "...")
    }

    # Prepare request payload
    payload <- list(
      input = list(batch),
      model = model,
      input_type = input_type,
      truncation = truncation,
      output_dimension = output_dimension,
      output_dtype = output_dtype,
      encoding_format = encoding_format
      ) |>
      purrr::flatten()

    # Construct headers
    headers <- httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )

    # Send request using httr::RETRY for robustness
    response <- httr::RETRY(
      verb = "POST",
      url = "https://api.voyageai.com/v1/embeddings",
      headers,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      encode = "json",
      times = 3, # Retry up to 3 times on transient errors
      quiet = quiet,
      terminate_on = c(400, 401, 403, 404) # Terminate on critical client errors
    )

    # Handle response
    httr::stop_for_status(response)
    result <- httr::content(response)

    # Parse embeddings based on encoding format
    if (is.null(encoding_format)) {
      embeddings <- purrr::map(result$data, \(x) unlist(x$embedding))
    } else if (encoding_format == "base64") {
      embeddings <- purrr::map(result$data, \(x) unlist(base64enc::base64decode(x$embedding)))
    } else {
      stop("Unknown encoding format: ", encoding_format)
    }

    # Organize responses into a tibble
    batch_responses <- dplyr::tibble(
      id = seq_along(batch),
      text_set = digest::digest(batch),
      embedding = embeddings
    )

    all_responses <- dplyr::bind_rows(all_responses, batch_responses)
  }

  return(all_responses)
}

#' Voyage AI Reranker
#'
#' Sends a query and a list of documents to the Voyage AI reranking API and
#' retrieves the reranking results.
#'
#' @param query `character` A string containing the query. The query must not be
#'   empty.
#' @param documents `character` A character vector containing the documents to
#'   be reranked. Must contain at least one document and no more than 1000.
#' @param model `character` The name of the reranking model to use. Supported
#'   options are:
#'   * `"rerank-2"` - Allows up to 4000 query tokens, 16000 tokens per document, and 300,000 total tokens.
#'   * `"rerank-2-lite"` - Allows up to 2000 query tokens, 8000 tokens per document, and 300,000 total tokens.
#'   * `"rerank-1"` - Allows up to 2000 query tokens, 8000 tokens per document, and 300,000 total tokens.
#'   * `"rerank-lite-1"` - Allows up to 1000 query tokens, 4000 tokens per document, and 300,000 total tokens.
#' @param top_k `integer`, optional The number of most relevant documents to
#'   return. If not specified, reranking results for all documents will be
#'   returned.
#' @param return_documents `logical`, optional Whether to include the documents
#'   in the response. Defaults to `FALSE`.
#'   * If `FALSE`, the API returns a list of indices and relevance scores.
#'   * If `TRUE`, the API also includes the documents in the response.
#' @param truncation `logical`, optional Whether to truncate the input to
#'   satisfy the token limits. Defaults to `TRUE`.
#'   * If `TRUE`, the query and documents will be truncated as needed to meet the limits.
#'   * If `FALSE`, an error will be raised if the token limits are exceeded.
#' @param quiet `logical`, optional If `TRUE`, suppresses logging messages.
#'   Defaults to `FALSE`.
#'
#' @return A `tibble` containing the reranking results with the following
#'   columns:
#'   * `index`: The index of the document in the input list.
#'   * `relevance_score`: The relevance score of the document with respect to the query.
#'   * `document`: (optional) The document string, included if `return_documents = TRUE`.
#'
#' @details This function communicates with the [Voyage AI reranking
#'   API](https://docs.voyageai.com/reference/reranker-api) to prioritize
#'   documents based on their relevance to the input query. See
#'   [https://docs.voyageai.com/reference/reranker-api](https://docs.voyageai.com/reference/reranker-api)
#'   for more.
#'
#'   Your API key must be set in the `VOYAGE_API_KEY` environment variable.
#'
#' @seealso [call_embedding_api()] for embedding queries and documents.
#'
#' @family embedding
#' @family reranking
#' @family voyage
#' @export
voyage_reranker <- function(query, documents, model, top_k = NULL,
                            return_documents = FALSE, truncation = TRUE,
                            quiet = FALSE) {

  # Validate API key
  api_key <- Sys.getenv("VOYAGE_API_KEY")
  if (api_key == "") stop("Voyage API key is not set in environment variables.")

  # Validate inputs
  checkmate::assert_string(query, min.chars = 1)
  checkmate::assert_character(documents, min.len = 1, max.len = 1000)
  checkmate::assert_string(model)
  checkmate::assert_int(top_k, lower = 1, null.ok = TRUE)
  checkmate::assert_flag(return_documents)
  checkmate::assert_flag(truncation)

  # Define model-specific token limits
  model_token_limits <- list(
    "rerank-2" = list(query_tokens = 4000, max_tokens_per_doc = 16000, total_tokens = 3e5),
    "rerank-2-lite" = list(query_tokens = 2000, max_tokens_per_doc = 8000, total_tokens = 3e5),
    "rerank-1" = list(query_tokens = 2000, max_tokens_per_doc = 8000, total_tokens = 3e5),
    "rerank-lite-1" = list(query_tokens = 1000, max_tokens_per_doc = 4000, total_tokens = 3e5)
  )
  token_limits <- model_token_limits[[model]]
  if (is.null(token_limits)) {
    stop("The specified model '", model, "' is not recognized. Supported models: ",
         paste(names(model_token_limits), collapse = ", "))
  }

  # Placeholder: Implement token counting for the query and documents
  # query_tokens <- count_tokens(query)
  # doc_tokens <- purrr::map_int(documents, count_tokens)
  # For now, assume the tokens are within limits.
  query_tokens <- NA  # Replace with actual token counting logic.
  doc_tokens <- NA

  # Validate token constraints
  if (!is.na(query_tokens) && query_tokens > token_limits$query_tokens) {
    stop("The query exceeds the token limit of ", token_limits$query_tokens, " for model '", model, "'.")
  }
  if (!is.na(doc_tokens) && any(doc_tokens > token_limits$max_tokens_per_doc)) {
    stop("One or more documents exceed the token limit of ", token_limits$max_tokens_per_doc, " for model '", model, "'.")
  }
  # Placeholder for combined token checks
  # if (!is.na(query_tokens) && !is.na(doc_tokens) && <combined checks>) { stop(... logic ...) }

  # Prepare the request payload
  payload <- list(
    query = query,
    documents = list(documents),
    model = model,
    top_k = top_k,
    return_documents = return_documents,
    truncation = truncation
    )  |>
    purrr::flatten()

  # Construct headers
  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  if (!quiet) {
    message("Sending rerank request to Voyage AI for model '", model, "'...")
  }

  # Send the request using httr::RETRY for robust handling
  response <- httr::RETRY(
    verb = "POST",
    url = "https://api.voyageai.com/v1/rerank",
    headers,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "json",
    times = 3, # Retry up to 3 times on transient errors
    quiet = quiet,
    terminate_on = c(400, 401, 403, 404) # Terminate on critical client errors
  )

  # Handle response
  httr::stop_for_status(response)
  result <- httr::content(response)

  # Parse reranking results
  rankings <- dplyr::tibble(
    index = purrr::map_int(result$data, ~.x$index),
    relevance_score = purrr::map_dbl(result$data, ~.x$relevance_score)
  )
  if (return_documents) {
    rankings <- rankings |>
      dplyr::mutate(document = purrr::map_chr(result$data, ~.x$document))
  }

  # Return the parsed rankings
  return(rankings)
}

