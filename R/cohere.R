cohere_embedding <- function(texts, model, api_key, input_type, embedding_types, quiet) {

  batch_size <- 96
  text_batches <- split(texts, ceiling(seq_along(texts) / batch_size))

  all_responses <- dplyr::tibble()
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
      terminate_on = c(400, 401, 403, 404)
    )

    # Stop for non-2xx responses
    httr::stop_for_status(response)
    result <- httr::content(response)

    # Parse embeddings dynamically based on embedding types
    embeddings <- purrr::map(result$embeddings[[embedding_types[[1]]]], ~.x)

    # Organize batch responses
    batch_responses <- dplyr::tibble(
      id = seq_along(batch),
      text_set = digest::digest(batch),
      embedding = embeddings
    )

    # Combine results
    all_responses <- dplyr::bind_rows(all_responses, batch_responses)
  }

  return(all_responses)
}
