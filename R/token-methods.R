#' Tokenize, Detokenize, and Count Tokens
#'
#' These functions provide three generics for token manipulation:
#'
#' 1. **\code{tokenize(content, ...)}** Splits text (a character vector) into
#' tokens (e.g. integer IDs).
#'
#' 2. **\code{detokenize(tokens, ...)}** Converts integer token IDs back into
#' text.
#'
#' 3. **\code{token_count(content, ...)}** Returns the number of tokens used by
#' a given model or backend, typically by calling \code{tokenize()} internally.
#'
#' @param content A character vector of text to tokenize or count, or a list of
#'   messages created by [format_chat()] to count tokens for (Claude only).
#' @param tokens An integer or numeric vector of token IDs to detokenize.
#' @param model For backends like Cohere or Claude, a string specifying which
#'   model's tokenizer to use.
#' @param add_special (llama.cpp only) Boolean controlling how the text is
#'   split.
#' @param with_pieces (llama.cpp only) Boolean controlling whether to return
#'   token pieces.
#' @param quiet Suppress retry messages.
#' @param ... Additional arguments passed to each method, potentially including
#'   API keys, extra parameters, or other backend-specific settings.
#'
#' @return
#'
#' - \strong{tokenize()}: Returns a vector (or list) of token IDs, often with a custom class like
#'   `"llama_cpp_tokenlist"` or `"cohere_tokenlist"`.
#'
#' - \strong{detokenize()}: Returns a character string (or vector of strings) that reconstructs
#'   the original text from the token IDs. The object often has a custom class
#'   like `"llama_cpp_character"`.
#'
#' - \strong{token_count()}: Returns an \strong{integer} (or numeric) value indicating how many
#'   tokens are used in the input text.
#'
#' @examples
#' \dontrun{
#' # Example: llama.cpp usage. Requires a running llama.cpp instance.
#' # 1) Tokenize
#' txt <- "Hello world!"
#' class(txt) <- c("llama_cpp_character", "character")
#' tokens <- tokenize(txt, add_special=FALSE)
#'
#' # 2) Detokenize
#' class(tokens) <- c("llama_cpp_tokenlist", "integer")
#' detok <- detokenize(tokens)
#' cat(detok)
#'
#' # 3) Count tokens
#' # For llama.cpp, we do the same as tokenize but just return length(tokens).
#'
#' # Example: Cohere usage
#' txt_cohere <- "This is a test for Cohere tokenization."
#' class(txt_cohere) <- c("cohere_character", "character")
#'
#' # Tokenize with optional model
#' co_tokens <- tokenize(txt_cohere, model = "command-r-plus-08-2024")
#'
#' # Count tokens (calls tokenize internally)
#' n_toks <- token_count(txt_cohere, model = "command-r-plus-08-2024")
#' print(n_toks)
#'
#' # Detokenize (reconstruct text)
#' class(co_tokens) <- c("cohere_tokenlist", "integer")
#' retext <- detokenize(co_tokens, model = "command-r-plus-08-2024")
#' cat(retext)
#' }
#'
#' @name tokens
#' @rdname tokens
NULL

#---- GENERIC: tokenize ----

#' @rdname tokens
#' @export
tokenize <- function(content, ...) {
  UseMethod("tokenize")
}

#---- GENERIC: detokenize ----

#' @rdname tokens
#' @export
detokenize <- function(tokens, ...) {
  UseMethod("detokenize")
}

#---- GENERIC: token_count ----

#' @rdname tokens
#' @export
token_count <- function(content, ...) {
  UseMethod("token_count")
}

# ==================================================================
# llama.cpp METHODS
# ==================================================================

#' @rdname tokens
#' @exportS3Method tokenize llama_cpp_character
tokenize.llama_cpp_character <- function(content,
                                         add_special = FALSE,
                                         with_pieces = FALSE,
                                         ...) {
  checkmate::assert_character(content)
  body <- list(
    content = content,
    add_special = add_special,
    with_pieces = with_pieces,
    ...
  ) |> purrr::compact()

  res <- httr::POST(
    url = "http://localhost:8080/tokenize",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  handle_errors(res)
  res <- httr::content(res)

  structure(
    res$tokens,
    class = c("llama_cpp_tokenlist", class(res))
  )
}

#' @rdname tokens
#' @exportS3Method detokenize llama_cpp_tokenlist
detokenize.llama_cpp_tokenlist <- function(tokens, ...) {
  body <- list(tokens = tokens)
  res <- httr::POST(
    url = "http://localhost:8080/detokenize",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)

  content_obj <- httr::content(res)
  structure(
    content_obj,
    class = c("llama_cpp_character", class(content_obj))
  )
}

#' @rdname tokens
#' @exportS3Method token_count llama_cpp_character
token_count.llama_cpp_character <- function(content, ...) {
  # A typical approach: call tokenize, count length of returned token list
  tokens <- tokenize.llama_cpp_character(content, ...)
  length(tokens)
}

# ==================================================================
# Claude METHOD (token_count)
# ==================================================================
#' @rdname tokens
#' @exportS3Method token_count claude_list
token_count.claude_list <- function(content, model, quiet = FALSE, ...) {
  # content is presumably a list of messages. Already has class "claude_chat" or "claude"
  # The existing code references "messages" - let's rename 'content' -> 'messages' internally:
  messages <- content

  if(is.null(system)) {
    system <- attr(content, "system")
  }

  body <- jsonlite::toJSON(
    list(messages = messages,
         model = model),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.anthropic.com/v1/messages/count_tokens",
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "token-counting-2024-11-01",
      "content-type" = "application/json",
      "system" = system
    ),
    body = body,
    encode = "json",
    times = 3,
    pause_base = 1,
    quiet = quiet,
    terminate_on = c(400:499)
  )

  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)

  # Return the number of input tokens
  response$input_tokens
}

# ==================================================================
# Cohere METHODS
# ==================================================================

#' @rdname tokens
#' @exportS3Method tokenize cohere_character
tokenize.cohere_character <- function(content, model = NULL, ...) {
  # If model is not supplied, default to something like "command-r-plus-08-2024"
  if (is.null(model)) {
    model <- "command-r-plus-08-2024"
  }

  api_key <- Sys.getenv("COHERE_API_KEY")
  if (api_key == "") {
    stop("Cohere API key not set in environment variables (COHERE_API_KEY).")
  }

  body <- list(
    model = model,
    text = paste(content, collapse = "\n")  # Cohere wants a single string?
  )

  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  res <- httr::POST(
    url = "https://api.cohere.com/v1/tokenize",
    headers,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  handle_errors(res)
  parsed <- httr::content(res)

  # 'tokens' is a list of integers
  # 'token_strings' is a list of strings
  # 'meta' might have usage info
  structure(
    parsed$tokens,
    class = c("cohere_tokenlist", "integer"),
    token_strings = parsed$token_strings,
    meta = parsed$meta
  )
}

#' @rdname tokens
#' @exportS3Method detokenize cohere_tokenlist
detokenize.cohere_tokenlist <- function(tokens, model = NULL, ...) {
  if (is.null(model)) {
    model <- "command-r-plus-08-2024"
  }

  api_key <- Sys.getenv("COHERE_API_KEY")
  if (api_key == "") {
    stop("Cohere API key not set in environment variables (COHERE_API_KEY).")
  }

  body <- list(
    model = model,
    tokens = unlist(tokens)  # ensure it's just an integer vector
  )

  headers <- httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  res <- httr::POST(
    url = "https://api.cohere.com/v1/detokenize",
    headers,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  handle_errors(res)
  parsed <- httr::content(res)

  # 'text' is the reconstructed string
  # 'meta' might contain usage info
  structure(
    parsed$text,
    class = c("cohere_character", "character"),
    meta = parsed$meta
  )
}

#' @rdname tokens
#' @exportS3Method token_count cohere_character
token_count.cohere_character <- function(content, model = NULL, ...) {
  # Just call tokenize, return length
  tokens <- tokenize.cohere_character(content, model = model, ...)
  length(tokens)
}

# ---- GEMINI METHOD (token_count) ----

#' @rdname tokens
#' @exportS3Method token_count gemini_list
token_count.gemini_list <- function(content,
                                    model = "gemini-1.5-flash",
                                    ...) {

  # 1) Retrieve your Google API key
  google_api_key <- Sys.getenv("GEMINI_API_KEY")
  if (nzchar(google_api_key) == FALSE) {
    stop("No GEMINI_API_KEY found in environment variables.")
  }

  system <- attr(content, "system")

  body <- jsonlite::toJSON(
    list(
      model = model,
      system_instruction = system,
      contents = content
    ) |> purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # 3) POST to the Google Generative Language 'countTokens' endpoint
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model, ":countTokens?key=", google_api_key
  )

  res <- httr::POST(
    url = url,
    body = body,
    encode = "json",
    httr::content_type_json()
  )

  # 4) Check for errors
  if (httr::http_error(res)) {
    err <- httr::content(res)$error
    cli::cli_abort(
      "Gemini API error: ", err$message,
      " (code: ", err$code, ", status: ", err$status, ")"
    )
  }

  # 5) Parse response: { "totalTokens": int, "cachedContentTokenCount": int }
  out <- httr::content(res)
  total <- out$totalTokens
  cached <- out$cachedContentTokenCount

  # If you only want the total, just return `total`.
  # Otherwise, you can store cachedContentTokenCount as an attribute:
  structure(
    total,
    cached_tokens = cached
  )
}
