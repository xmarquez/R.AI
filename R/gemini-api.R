#' @rdname chat
#' @param ttl **Gemini only**. "Time to live" for cached messages. Defaults to
#'   300s.
#' @param use_cache_name **Gemini Only**. If provided, the API will attempt to
#'   use cached contents using the specific cache name. The cache name is
#'   returned in the response if the messages have a cache attribute created by
#'   [format_chat()]; it is not user-created. Typically, you will call [chat()],
#'   extract the cache name, and call [chat()] again with a new query and the
#'   paramter `use_cache_name` set to the `cache_name` field of the first
#'   response.
#' @exportS3Method
chat.gemini_list <- function(messages,
                             model = get_default_model("gemini"),
                             use_cache_name = NULL,
                             max_retries = 3,
                             temperature = 0.2,
                             max_tokens = 300,
                             json_mode = FALSE,
                             system = NULL,
                             ttl = "300s",
                             quiet = FALSE) {

  model_query <- paste0(model, ":generateContent")

  if (json_mode) {
    response_mime_type <- "application/json"
  } else {
    response_mime_type <- NULL
  }


  if (is.null(system)) {
    if (!is.null(attr(messages, "system"))) {
      system <- list(parts = list(text = attr(messages, "system")))
    }
  }

  # Handle cache
  cache <- attr(messages, "cache")
  cached_content <- NULL
  if(!is.null(use_cache_name)) {
    cache$name <- use_cache_name
  }

  if (!is.null(cache)) {
    if (!is.null(cache$name)) {
      # Try to retrieve existing cache
      cached_content <- retrieve_cache(cache$name, quiet = quiet)
    }
    if (is.null(cached_content) && !is.null(cache$content)) {
      # Create cache if it doesn't exist
      cached_content <- create_cache(
        cache_name = cache$name,
        content = cache$content,
        model = model,
        ttl = ttl,
        quiet = quiet
      )
    }
  }

  # Build request body
  body <- jsonlite::toJSON(
    list(
      system_instruction = system,
      contents = messages,
      cachedContent = cached_content,
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_tokens,
        response_mime_type = response_mime_type
      ) |> purrr::compact()
    ) |> purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  if (!quiet) message("Sending request to Gemini API...")

  res <- httr::RETRY(
    verb = "POST",
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
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

  # Attach the known cache name
  if(!is.null(cached_content)) {
    response$cache_name <- cached_content
  }

  structure(response, class = c("gemini_chat", class(response)))
}

# Helper function to create a cache
create_cache <- function(cache_name, content, model, ttl, quiet = FALSE) {
  url <- "https://generativelanguage.googleapis.com/v1beta/cachedContents"

  payload <- list(
    model = paste0("models/", model),
    contents = content,
    ttl = ttl,
    displayName = cache_name
  )

  if (!quiet) message("Creating cache...")

  res <- httr::POST(
    url = url,
    config = httr::add_headers("content-type" = "application/json"),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
  )

  if (httr::http_error(res)) {
    cli::cli_abort("Cache creation failed: {httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }

  cache_response <- httr::content(res)
  cache_response$name
}

retrieve_cache <- function(cache_name, quiet = FALSE) {
  # If the user did not pass "cachedContents/xxx" explicitly,
  # we prepend it ourselves:
  full_name <- cache_name
  if (!startsWith(cache_name, "cachedContents/")) {
    full_name <- paste0("cachedContents/", cache_name)
  }

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/", full_name)

  if (!quiet) message("Retrieving cache... from ", url)

  res <- httr::GET(
    url = url,
    query = list(key = Sys.getenv("GEMINI_API_KEY"))
  )

  if (httr::http_error(res)) {
    if (!quiet) message("Cache retrieval failed: {httr::http_status(res)$message}")
    return(NULL)
  }

  cache_response <- httr::content(res)
  cache_response$name
}

#' @exportS3Method
get_usage.gemini_chat <- function(response, model) {
  if(is.null(response$usageMetadata$candidatesTokenCount)) {
    response$usageMetadata$candidatesTokenCount <- 0
  }
  response$usageMetadata |>
    dplyr::as_tibble() |>
    dplyr::rename(input_tokens = "promptTokenCount",
                  output_tokens = "candidatesTokenCount",
                  total_tokens = "totalTokenCount") |>
    dplyr::mutate(model = response$modelVersion)
}

#' @exportS3Method
get_content.gemini_chat <- function(response) {
  response$candidates[[1]]$content$parts[[1]]$text
}

#' @exportS3Method
get_message.gemini_chat <- function(response) {
  response$candidates[[1]]$content |>
    structure(class = c("gemini_list", "list"))
}

#' @exportS3Method
embed.gemini_character <- function(content,
                                   model = "text-embedding-004",
                                   quiet = FALSE,
                                   max_retries = 10) {
  checkmate::assert_character(content)

  url <- glue::glue("https://generativelanguage.googleapis.com/v1beta/models/{model}:batchEmbedContents")

  # Prepare the request payload
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

  # Handle response
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

