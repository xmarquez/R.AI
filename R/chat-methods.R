#' Create a Chat Completion
#'
#' A generic S3-based interface for creating chat completions with multiple
#' backends (OpenAI, Gemini, Claude, Cerebras, Groq, Llama.cpp, Ollama). Each
#' backend has its own method, but all methods share the same formal signature.
#'
#' @section Supported Backends:
#' - **OpenAI**: Dispatches to the [OpenAI Chat API](https://platform.openai.com/docs/api-reference/chat).
#' - **Gemini**: Dispatches to the [Gemini Chat API](https://ai.google.dev/api/generate-content).
#' - **Claude**: Dispatches to the [Anthropic Claude API](https://docs.anthropic.com/en/api/messages).
#' - **Cohere**: Dispatches to the [Cohere Chat API](https://docs.cohere.com/v2/reference/chat).
#' - **DeepSeek**: Dispatches to the [DeepSeek Chat API](https://api.deepseek.com/chat/completions).
#' - **Mistral**: Dispatches to the [Mistral Chat API](https://docs.mistral.ai/api/#tag/chat).
#' - **Groq**: Dispatches to the [Groq Chat API](https://console.groq.com/docs/api-reference#chat).
#' - **Cerebras**: Dispatches to the [Cerebras Chat API](https://inference-docs.cerebras.ai/api-reference/chat-completions).
#' - **Llama.cpp**: Dispatches to a local LlamaFile instance or an Llama.CPP server running a GGUF model.
#' - **Ollama**: Dispatches to a local model via the [Ollama server](https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion).
#'
#' @section Environment Variables:
#' Each non-local backend requires an API key. For example, OpenAI uses
#' `OPENAI_API_KEY`, Gemini uses `GEMINI_API_KEY`, Claude uses
#' `ANTHROPIC_API_KEY`, etc. You can set them via `Sys.setenv()` or in
#' your `.Renviron`.
#'
#' @param messages A list of user and system messages in the correct format for
#'   the intended backend. Often produced by helper functions like
#'   [format_chat()].
#' @param model A string specifying the ID of the model to use. See the docs or
#'   [list_models()] for available models. Each backend has different model
#'   naming conventions.
#' @param temperature Numeric (default `0.2`). Controls randomness during
#'   sampling. Values closer to `1` produce more random outputs; values closer
#'   to `0` produce more deterministic outputs.
#' @param max_tokens Integer or `NULL`. The maximum number of tokens to generate.
#'   Some backends (e.g., OpenAI) prefer a separate parameter called
#'   `max_completion_tokens`. **Note** that for newer OpenAI “o1” series models,
#'   this is not supported. For backward compatibility, you can still pass
#'   `max_tokens` to older OpenAI models, but it is effectively deprecated in
#'   favor of `max_completion_tokens`.
#' @param quiet Logical (default `FALSE`). If `TRUE`, suppress messages (including
#'   retry notifications).
#' @param ... Additional arguments passed to the backend-specific method.
#'
#' @section Common Additional Parameters:
#' The following are recognized by some or all backends. If a backend does not
#' support a parameter, it will be silently ignored.
#'
#' - **`presence_penalty`** (Numeric or `NULL`): Used by OpenAI (and some others)
#'   to penalize repeated content, promoting new topics.
#' - **`frequency_penalty`** (Numeric or `NULL`): Also primarily for OpenAI.
#' - **`top_p`** (Numeric or `NULL`): Probability mass threshold for nucleus
#'   sampling.
#' - **`top_k`** (Integer or `NULL`): Typically for Claude, limiting sampling to
#'   the top-k tokens.
#' - **`stop`** (Character vector/string or `NULL`): Custom sequences that cause
#'   the model to stop.
#' - **`stop_sequences`** (Character vector or `NULL`): Claude-specific name for
#'   stops.
#' - **`tools`, `tool_choice`** (List or `NULL`): Ad-hoc mechanism for
#'   instructing a model to call external functions. Variation in how each API
#'   implements these.
#' - **`parallel_tool_calls`** (Logical or `NULL`): For backends supporting
#'   concurrent tool invocation.
#' - **`json_mode`** (Logical, default `FALSE`): If `TRUE`, requests and
#'   responses favor JSON outputs. Supported by some backends (OpenAI, Groq,
#'   Cerebras, etc.).
#' - **`user`** (String or `NULL`): Some backends let you specify a user ID.
#' - **`max_retries`** (Integer, default 3): How many times to retry on error.
#' - **`system`** (Character or `NULL`): A system-level prompt used by some
#'   backends (e.g., Claude, Gemini) for extra context or instructions.
#'
#' @section Gemini-Specific Cache Parameters:
#' - **`ttl`** (Gemini only): The “time to live” for cached content, default
#'   `"300s"`.
#' - **`use_cache_name`** (Gemini only): If provided, tries to use existing
#'   cached messages in caches with this name.
#'
#' @examples
#' \dontrun{
#' # OpenAI example
#' response <- chat.openai_list(
#'   messages = list(list(role="user", content="Hello!")),
#'   model = "gpt-3.5-turbo",
#'   temperature = 0.3
#' )
#'
#' # Gemini example (with a system prompt):
#' response <- chat.gemini_list(
#'   messages = list(list(role="user", content="What's the weather?")),
#'   system = "You are a helpful weather assistant.",
#'   ttl = "300s"
#' )
#'
#' # Claude example (with stop sequences):
#' response <- chat.claude_list(
#'   messages = list(list(role = "user", content = "Write a short poem.")),
#'   stop_sequences = c("##")
#' )
#' }
#'
#' @return A response object from the chosen backend (S3 class typically named
#'   `{backend}_chat`).
#'
#' @export
chat <- function(messages, model, temperature, max_tokens, quiet, ...) {
  UseMethod("chat")
}

# --------------------------
# All the methods now simply inherit the documentation via @rdname chat:
# --------------------------

#' @rdname chat
#' @exportS3Method chat openai_list
chat.openai_list <- function(messages, model = get_default_model("openai"),
                             temperature = 0.2, max_tokens = NULL,
                             quiet = FALSE, ...) {
  # ... same code as before ...
  dots <- list(...)
  store <- dots$store %||% NULL
  metadata <- dots$metadata %||% NULL
  frequency_penalty <- dots$frequency_penalty %||% NULL
  logit_bias <- dots$logit_bias %||% NULL
  logprobs <- dots$logprobs %||% NULL
  top_logprobs <- dots$top_logprobs %||% NULL
  max_completion_tokens <- dots$max_completion_tokens %||% NULL
  n <- dots$n %||% 1
  modalities <- dots$modalities %||% NULL
  prediction <- dots$prediction %||% NULL
  audio <- dots$audio %||% NULL
  presence_penalty <- dots$presence_penalty %||% NULL
  response_format <- dots$response_format %||% NULL
  seed <- dots$seed %||% NULL
  service_tier <- dots$service_tier %||% NULL
  stop <- dots$stop %||% NULL
  temperature <- dots$temperature %||% 0.2
  top_p <- dots$top_p %||% NULL
  tools <- dots$tools %||% NULL
  tool_choice <- dots$tool_choice %||% NULL
  parallel_tool_calls <- dots$parallel_tool_calls %||% NULL
  user <- dots$user %||% NULL
  json_mode <- dots$json_mode %||% FALSE
  max_retries <- dots$max_retries %||% 3
  quiet <- dots$quiet %||% FALSE

  if (json_mode) {
    response_format <- list(type = "json_object")
  }

  args_list <- list(
    messages = messages,
    model = model,
    store = store,
    metadata = metadata,
    frequency_penalty = frequency_penalty,
    logit_bias = logit_bias,
    logprobs = logprobs,
    top_logprobs = top_logprobs,
    max_tokens = max_tokens,
    max_completion_tokens = max_completion_tokens,
    n = n,
    modalities = modalities,
    prediction = prediction,
    audio = audio,
    presence_penalty = presence_penalty,
    response_format = response_format,
    seed = seed,
    service_tier = service_tier,
    stop = stop,
    temperature = temperature,
    top_p = top_p,
    tools = tools,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    user = user
  )

  args_list <- args_list[!vapply(args_list, is.null, logical(1))]
  body <- jsonlite::toJSON(args_list, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "https://api.openai.com/v1/chat/completions",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = max_retries,
    pause_cap = 1200,
    quiet = quiet
  )
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)
  structure(response, class = c("openai_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat gemini_list
chat.gemini_list <- function(messages,
                             model = get_default_model("gemini"),
                             temperature = 0.2,
                             max_tokens = 300,
                             quiet = FALSE,
                             ...) {
  # ... same code as before ...
  dots <- list(...)
  use_cache_name <- dots$use_cache_name %||% NULL
  max_retries <- dots$max_retries %||% 3
  json_mode <- dots$json_mode %||% FALSE
  system <- dots$system %||% NULL
  ttl <- dots$ttl %||% "300s"

  model_query <- paste0(model, ":generateContent")
  if (json_mode) {
    response_mime_type <- "application/json"
  } else {
    response_mime_type <- NULL
  }

  if (is.null(system) && !is.null(attr(messages, "system"))) {
    system <- list(parts = list(text = attr(messages, "system")))
  }
  cache <- attr(messages, "cache")
  cached_content <- NULL
  if (!is.null(use_cache_name)) {
    cache$name <- use_cache_name
  }
  if (!is.null(cache)) {
    if (!is.null(cache$name)) {
      cached_content <- retrieve_cache(cache$name, quiet = quiet)
    }
    if (is.null(cached_content) && !is.null(cache$content)) {
      cached_content <- create_cache(
        cache_name = cache$name,
        content = cache$content,
        model = model,
        ttl = ttl,
        quiet = quiet
      )
    }
  }

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
  if (!is.null(cached_content)) {
    response$cache_name <- cached_content
  }
  structure(response, class = c("gemini_chat", class(response)))
}

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

#' @rdname chat
#' @exportS3Method chat claude_list
chat.claude_list <- function(messages,
                             model = get_default_model("claude"),
                             temperature = 0.2,
                             max_tokens = 300,
                             quiet = FALSE,
                             ...) {
  # ... same code ...
  dots <- list(...)
  max_retries <- dots$max_retries %||% 3
  system <- dots$system %||% NULL
  stop_sequences <- dots$stop_sequences %||% NULL
  top_k <- dots$top_k %||% NULL
  top_p <- dots$top_p %||% NULL
  tool_choice <- dots$tool_choice %||% NULL
  tools <- dots$tools %||% NULL

  if (is.null(system) && !is.null(attr(messages, "system"))) {
    system <- attr(messages, "system")
  }

  body <- jsonlite::toJSON(
    list(
      system = system,
      messages = messages,
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      stop_sequences = stop_sequences,
      top_k = top_k,
      top_p = top_p,
      tool_choice = tool_choice,
      tools = tools
    ) |> purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.anthropic.com/v1/messages",
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ),
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
  structure(response, class = c("claude_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat mistral_list
chat.mistral_list <- function(messages,
                              model = get_default_model("mistral"),
                              temperature = 0.2,
                              max_tokens = 300,
                              quiet = FALSE,
                              ...) {

  # Capture additional arguments from `...`
  dots <- list(...)

  # Extract Mistral-specific parameters
  n <- dots$n %||% 1
  max_retries <- dots$max_retries %||% 3
  top_p <- dots$top_p %||% 1
  json_mode <- dots$json_mode %||% FALSE
  stop <- dots$stop %||% NULL
  random_seed <- dots$random_seed %||% NULL
  presence_penalty <- dots$presence_penalty %||% 0
  frequency_penalty <- dots$frequency_penalty %||% 0

  # Set response format for JSON mode
  if (json_mode) {
    response_format <- list(type = "json_object")
  } else {
    response_format <- NULL
  }

  # Build the request body
  body <- jsonlite::toJSON(
    list(
      model = model,
      temperature = temperature,
      top_p = top_p,
      max_tokens = max_tokens,
      stop = stop,
      random_seed = random_seed,
      messages = messages,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      n = n,
      response_format = response_format
    ) |> purrr::compact(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Send the request with retries
  res <- retry_response(
    base_url = "https://api.mistral.ai/v1/chat/completions",
    api_key = Sys.getenv("MISTRAL_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = max_retries,
    pause_cap = 1200,
    quiet = quiet
  )

  # Handle errors
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }

  # Parse the response
  response <- httr::content(res)

  structure(response, class = c("mistral_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat cerebras_list
chat.cerebras_list <- function(messages,
                               model = get_default_model("cerebras"),
                               temperature = 0.2,
                               max_tokens = NULL,
                               quiet = FALSE,
                               ...) {
  dots <- list(...)
  json_mode <- dots$json_mode %||% FALSE
  seed <- dots$seed %||% NULL
  stop <- dots$stop %||% NULL
  top_p <- dots$top_p %||% NULL
  tools <- dots$tools %||% NULL
  tool_choice <- dots$tool_choice %||% NULL
  user <- dots$user %||% NULL

  if (json_mode) {
    response_format <- list(type = "json_object")
  } else {
    response_format <- NULL
  }

  args_list <- list(
    messages = messages,
    model = model,
    max_completion_tokens = max_tokens,
    response_format = response_format,
    seed = seed,
    stop = stop,
    temperature = temperature,
    top_p = top_p,
    tools = tools,
    tool_choice = tool_choice,
    user = user
  )
  args_list <- args_list[!vapply(args_list, is.null, logical(1))]
  body <- jsonlite::toJSON(args_list, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "https://api.cerebras.ai/v1/chat/completions",
    api_key = Sys.getenv("CEREBRAS_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = 3,
    pause_cap = 1200,
    quiet = quiet
  )
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)
  structure(response, class = c("cerebras_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat groq_list
chat.groq_list <- function(messages,
                           model = get_default_model("groq"),
                           temperature = 0.2,
                           max_tokens = NULL,
                           quiet = FALSE,
                           ...) {
  dots <- list(...)
  frequency_penalty <- dots$frequency_penalty %||% NULL
  n <- dots$n %||% 1
  presence_penalty <- dots$presence_penalty %||% NULL
  response_format <- dots$response_format %||% NULL
  seed <- dots$seed %||% NULL
  stop <- dots$stop %||% NULL
  tools <- dots$tools %||% NULL
  tool_choice <- dots$tool_choice %||% NULL
  parallel_tool_calls <- dots$parallel_tool_calls %||% NULL
  user <- dots$user %||% NULL
  json_mode <- dots$json_mode %||% FALSE
  max_retries <- dots$max_retries %||% 3

  if (isTRUE(json_mode)) {
    response_format <- list(type = "json_object")
  }

  args_list <- list(
    messages = messages,
    model = model,
    frequency_penalty = frequency_penalty,
    max_tokens = max_tokens,
    n = n,
    presence_penalty = presence_penalty,
    response_format = response_format,
    seed = seed,
    stop = stop,
    temperature = temperature,
    tools = tools,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    user = user
  )
  args_list <- args_list[!vapply(args_list, is.null, logical(1))]
  body <- jsonlite::toJSON(args_list, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "https://api.groq.com/openai/v1/chat/completions",
    api_key = Sys.getenv("GROQ_API_KEY"),
    response_format = NULL,
    body = body,
    max_retries = max_retries,
    pause_cap = 1200,
    quiet = quiet
  )
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}.")
  }
  response <- httr::content(res)
  structure(response, class = c("groq_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat llama_cpp_list
chat.llama_cpp_list <- function(messages,
                                model = NULL,
                                temperature = 0.8,
                                max_tokens = NULL,
                                quiet = FALSE,
                                ...) {
  dots <- list(...)
  stop <- dots$stop %||% NULL
  max_retries <- dots$max_retries %||% 5

  body <- list(
    messages = messages,
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    stop = stop
  ) |> purrr::compact()
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "http://localhost:8080/v1/chat/completions",
    api_key = "no-key",
    body = json_body,
    max_retries = max_retries,
    response_format = NULL,
    pause_cap = 10,
    quiet = quiet
  )
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)
  response$model <- which_llamafile_running()
  structure(response, class = c("llama_cpp_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat ollama_list
chat.ollama_list <- function(messages,
                             model,
                             temperature = 0.2,
                             max_tokens = 300,
                             quiet = FALSE,
                             ...) {
  dots <- list(...)
  tools <- dots$tools %||% NULL
  format <- dots$format %||% NULL
  options <- dots$options %||% list()
  stream <- dots$stream %||% FALSE
  keep_alive <- dots$keep_alive %||% "5m"
  max_retries <- dots$max_retries %||% 3

  options <- modifyList(options, list(temperature = temperature, max_tokens = max_tokens))
  body <- list(
    model = model,
    messages = messages,
    tools = tools,
    format = format,
    options = options,
    stream = stream,
    keep_alive = keep_alive
  ) |> purrr::compact()
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE)

  res <- retry_response(
    base_url = "http://localhost:11434/api/chat",
    api_key = "no-key",
    body = json_body,
    max_retries = max_retries,
    response_format = NULL,
    pause_cap = 1200,
    quiet = quiet
  )
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}. {httr::content(res)$error$message}")
  }
  response <- httr::content(res)
  structure(response, class = c("ollama_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat cohere_list
chat.cohere_list <- function(messages,
                             model = "command-r-plus-08-2024",
                             temperature = 0.3,
                             max_tokens = NULL,
                             quiet = FALSE,
                             ...) {
  # Capture additional user arguments in dots
  dots <- list(...)

  # Cohere default fields
  # For v2/chat, the "stream" field is required, defaulting to FALSE
  stream <- dots$stream %||% FALSE

  # Additional optional parameters from docs:
  # frequency_penalty, presence_penalty, tools, documents, stop_sequences, etc.
  frequency_penalty <- dots$frequency_penalty %||% 0.0
  presence_penalty  <- dots$presence_penalty  %||% 0.0
  k                 <- dots$k                 %||% 0
  p                 <- dots$p                 %||% 0.75
  logprobs          <- dots$logprobs          %||% FALSE
  strict_tools      <- dots$strict_tools      %||% NULL
  safety_mode       <- dots$safety_mode       %||% NULL
  stop_sequences    <- dots$stop_sequences    %||% NULL
  seed              <- dots$seed              %||% NULL

  # Tools & documents are optional lists or objects
  tools             <- dots$tools             %||% NULL
  documents         <- dots$documents         %||% NULL

  # response_format can force JSON output structure
  response_format   <- dots$response_format   %||% NULL

  # Build the request body per Cohere's v2/chat spec
  body_list <- list(
    # Required
    stream       = stream,
    model        = model,
    messages     = messages,  # Must be a list of {role, content} objects

    # Optional
    tools        = tools,
    documents    = documents,
    response_format = response_format,
    safety_mode  = safety_mode,
    max_tokens   = max_tokens,
    stop_sequences = stop_sequences,
    temperature  = temperature,
    seed         = seed,
    frequency_penalty = frequency_penalty,
    presence_penalty  = presence_penalty,
    k            = k,
    p            = p,
    logprobs     = logprobs,
    strict_tools = strict_tools
  )
  # Remove any NULL fields before JSON-encoding
  body_list <- purrr::compact(body_list)

  # Convert to JSON
  json_body <- jsonlite::toJSON(body_list, auto_unbox = TRUE, pretty = TRUE)

  # Retrieve API key
  cohere_api_key <- Sys.getenv("COHERE_API_KEY")
  if (!nzchar(cohere_api_key)) {
    stop("Cohere API key is not set in environment variable COHERE_API_KEY.")
  }

  # Decide how many times to retry on errors/timeouts (default = 3)
  max_retries <- dots$max_retries %||% 3

  # Make the request with some retry logic
  res <- httr::RETRY(
    verb = "POST",
    url  = "https://api.cohere.com/v2/chat",
    config = httr::add_headers(
      "Authorization" = paste("Bearer", cohere_api_key),
      "Content-Type"  = "application/json"
    ),
    body = json_body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = 1200,   # Don’t exceed 20 min
    quiet = quiet,
    terminate_on = c(400:499)
  )

  # Handle errors
  if (httr::http_error(res)) {
    err <- httr::content(res)
    cli::cli_abort("{httr::http_status(res)$message}. {err$message %||% err$error$message}")
  }

  # Parse and return the result
  response <- httr::content(res)

  # Wrap the response in an S3 class "cohere_chat"
  structure(response, class = c("cohere_chat", class(response)))
}

#' @rdname chat
#' @exportS3Method chat deepseek_list
chat.deepseek_list <- function(messages,
                               model = "deepseek-chat",
                               temperature = 1,
                               max_tokens = 2048,
                               quiet = FALSE,
                               ...) {
  # Capture additional user arguments in dots
  dots <- list(...)

  # Look up API key from environment
  deepseek_api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (!nzchar(deepseek_api_key)) {
    stop("DeepSeek API key is not set in environment variable DEEPSEEK_API_KEY.")
  }

  # Additional optional parameters from DeepSeek docs:
  frequency_penalty  <- dots$frequency_penalty  %||% 0
  presence_penalty   <- dots$presence_penalty   %||% 0
  top_p              <- dots$top_p              %||% 1
  response_format    <- dots$response_format    %||% list(type = "text")
  stop_sequences     <- dots$stop               %||% NULL       # can be a string or list
  stream             <- dots$stream             %||% FALSE
  stream_options     <- dots$stream_options     %||% NULL
  tools              <- dots$tools              %||% NULL
  tool_choice        <- dots$tool_choice        %||% "none"
  logprobs           <- dots$logprobs           %||% FALSE
  top_logprobs       <- dots$top_logprobs       %||% NULL
  json_mode          <- dots$json_mode          %||% FALSE

  if (isTRUE(json_mode)) {
    response_format <- list(type = "json_object")
  }

  # Build the request body
  body_list <- list(
    messages          = messages,        # required
    model             = model,           # required
    frequency_penalty = frequency_penalty,
    presence_penalty  = presence_penalty,
    max_tokens        = max_tokens,
    response_format   = response_format, # e.g. { "type": "json_object" }
    stop              = stop_sequences,  # can be null, a string, or an array
    stream            = stream,
    stream_options    = stream_options,
    temperature       = temperature,
    top_p             = top_p,
    tools             = tools,
    tool_choice       = tool_choice,
    logprobs          = logprobs,
    top_logprobs      = top_logprobs
  )
  # Remove any NULL fields
  body_list <- purrr::compact(body_list)

  # Convert to JSON
  json_body <- jsonlite::toJSON(body_list, auto_unbox = TRUE, pretty = TRUE)

  # Decide how many times to retry on errors/timeouts (default = 3)
  max_retries <- dots$max_retries %||% 3

  # Send request with retries
  res <- httr::RETRY(
    verb = "POST",
    url  = "https://api.deepseek.com/chat/completions",
    config = httr::add_headers(
      "Authorization" = paste("Bearer", deepseek_api_key),
      "Content-Type"  = "application/json",
      "Accept"        = "application/json"
    ),
    body = json_body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = 1200,   # e.g. up to 20 min
    quiet = quiet,
    terminate_on = c(400:499) # any 4xx we won't retry
  )

  # Check for errors
  if (httr::http_error(res)) {
    err <- httr::content(res)
    cli::cli_abort("{httr::http_status(res)$message}. {err$message %||% err$error$message}")
  }

  # Parse the response
  response <- httr::content(res)

  # Wrap the response in an S3 class "deepseek_chat"
  structure(response, class = c("deepseek_chat", class(response)))
}

