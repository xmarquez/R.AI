#' @exportS3Method
embed.cohere_character <- function(content, model = "embed-english-v3.0",
                                   input_type = "search_document",
                                   embedding_types = "float", quiet = FALSE) {
  api_key <- Sys.getenv("COHERE_API_KEY")

  # Validate API key
  if(api_key == "") {
    stop("Cohere API key not set in environment variables.")
  }

  checkmate::assert_choice(input_type, c("search_document", "search_query", "classification", "clustering", "image"))
  checkmate::assert_subset(embedding_types, c("float", "int8", "uint8", "binary", "ubinary"))

  batch_size <- 96
  text_batches <- split(content, ceiling(seq_along(content) / batch_size))

  all_embeddings <- list()
  total_tokens <- 0
  for (batch_idx in seq_along(text_batches)) {
    batch <- text_batches[[batch_idx]]

    # Log progress
    if (!quiet) {
      message("Processing batch ", batch_idx, " of ", length(text_batches), "...")
    }

    # Construct the payload
    payload <- list(
      model = jsonlite::unbox(model),
      input_type = jsonlite::unbox(input_type),
      embedding_types = embedding_types,
      texts = batch
    )

    # Set headers
    headers <- httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json",
      `X-Client-Name` = "cohere_r_package"
    )

    # Retry logic for API request
    response <- httr::RETRY(
      verb = "POST",
      url = "https://api.cohere.com/v2/embed",
      headers,
      body = jsonlite::toJSON(payload),
      encode = "json",
      times = 3,
      quiet = quiet,
      terminate_on = c(400:499)
    )

    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
      }
    result <- httr::content(response)

    # Parse embeddings dynamically based on embedding types
    embeddings <- purrr::map(result$embeddings[[embedding_types[[1]]]], ~.x)

    # Combine results
    total_tokens <- total_tokens + result$meta$billed_units$input_tokens
    all_embeddings <- c(all_embeddings, embeddings)
  }

  structure(all_embeddings, class = c("embedding", class(all_embeddings)),
            total_tokens = total_tokens, model = model)
}
