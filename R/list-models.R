#' List Available Models
#'
#' A generic function that fetches model information from various sources
#' (Mistral, OpenAI, Groq, Cerebras, Gemini, Claude, Ollama, or a local directory
#' for Llama files). You specify which backend you want by passing a string
#' (e.g., `"mistral"`, `"openai"`, `"gemini"`, `"claude"`, `"llamafile"`). Internally,
#' the class of the string is set so that `UseMethod("list_models")` can dispatch
#' to the corresponding S3 method.
#'
#' @param api `character`. Specifies the source to query. Must be one of:
#'   * `"mistral"`
#'   * `"openai"`
#'   * `"groq"`
#'   * `"cerebras"`
#'   * `"gemini"`
#'   * `"ollama"`
#'   * `"claude"`
#'   * `"llamafile"`
#'
#' @param local_dir `character`. Path to the local directory containing models
#'   (used only when `api = "llamafile"`). Defaults to `"models"`.
#'
#' @param page_size `integer`. Maximum number of models to return (applies to
#'   Gemini and Claude APIs). Defaults to 50.
#'
#' @return A tidy data frame (`tibble`) when querying remote APIs, or a character
#'   vector of filenames for `"llamafile"`. Ollama returns a tibble of local models
#'   if you choose `"ollama"`.
#'
#' @section Environment Variables:
#' For remote API queries, ensure the relevant environment variable is set:
#' \itemize{
#'   \item `MISTRAL_API_KEY` (for `"mistral"`)
#'   \item `OPENAI_API_KEY` (for `"openai"`)
#'   \item `GROQ_API_KEY` (for `"groq"`)
#'   \item `CEREBRAS_API_KEY` (for `"cerebras"`)
#'   \item `GEMINI_API_KEY` (for `"gemini"`)
#'   \item `ANTHROPIC_API_KEY` (for `"claude"`)
#' }
#'
#' @examples
#' \dontrun{
#' ## List OpenAI models:
#' list_models("openai")
#'
#' ## List Claude models:
#' list_models("claude", page_size = 20)
#'
#' ## List local llama files:
#' list_models("llamafile", local_dir = "my_local_models")
#' }
#'
#' @family model utilities
#' @export
list_models <- function(api, local_dir = "models", page_size = 50) {
  # Assign a class that matches the chosen API
  class(api) <- c(api, class(api))
  UseMethod("list_models", api)
}

# -------------------------------------------------------------------------
# Mistral method
# -------------------------------------------------------------------------

#' @rdname list_models
#' @exportS3Method list_models mistral
list_models.mistral <- function(api, local_dir = "models", page_size = 50) {
  id <- max_context_length <- completion_chat <- vision <- NULL
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if (api_key == "") {
    stop("Mistral API key is not set in environment variables (MISTRAL_API_KEY).")
  }

  url <- "https://api.mistral.ai/v1/models"
  headers <- c(Authorization = paste("Bearer", api_key))
  response <- httr::GET(url, httr::add_headers(.headers = headers))
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}")
  }

  models <- httr::content(response)$data
  # Flatten the results into a tibble
  models |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON() |>
    tidyr::unnest(id:max_context_length) |>
    tidyr::unnest(completion_chat:vision)
}

# -------------------------------------------------------------------------
# OpenAI method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models openai
list_models.openai <- function(api, local_dir = "models", page_size = 50) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("OpenAI API key is not set in environment variables (OPENAI_API_KEY).")
  }

  url <- "https://api.openai.com/v1/models"
  headers <- c(Authorization = paste("Bearer", api_key))
  response <- httr::GET(url, httr::add_headers(.headers = headers))
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}")
  }

  models <- httr::content(response)$data
  purrr::map(models, dplyr::as_tibble) |> purrr::list_rbind()
}

# -------------------------------------------------------------------------
# Groq method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models groq
list_models.groq <- function(api, local_dir = "models", page_size = 50) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") {
    stop("Groq API key is not set in environment variables (GROQ_API_KEY).")
  }

  url <- "https://api.groq.com/openai/v1/models"
  headers <- c(Authorization = paste("Bearer", api_key))
  response <- httr::GET(url, httr::add_headers(.headers = headers))
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}")
  }

  models <- httr::content(response)$data
  # Flatten and return as tibble
  purrr::map(models, purrr::compact) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_rbind()
}

# -------------------------------------------------------------------------
# Cerebras method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models cerebras
list_models.cerebras <- function(api, local_dir = "models", page_size = 50) {
  api_key <- Sys.getenv("CEREBRAS_API_KEY")
  if (api_key == "") {
    stop("Cerebras API key is not set in environment variables (CEREBRAS_API_KEY).")
  }

  url <- "https://api.cerebras.ai/v1/models"
  headers <- c(Authorization = paste("Bearer", api_key))
  response <- httr::GET(url, httr::add_headers(.headers = headers))
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}")
  }

  models <- httr::content(response)$data
  purrr::map(models, purrr::compact) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_rbind()
}

# -------------------------------------------------------------------------
# Gemini method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models gemini
list_models.gemini <- function(api, local_dir = "models", page_size = 50) {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (api_key == "") {
    stop("Gemini API key is not set in environment variables (GEMINI_API_KEY).")
  }

  base_url <- "https://generativelanguage.googleapis.com/v1beta/models"
  page_token <- NULL
  all_models <- list()

  repeat {
    query_params <- list(key = api_key, pageSize = page_size)
    if (!is.null(page_token)) query_params$pageToken <- page_token

    response <- httr::GET(base_url, query = query_params)
    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}")
    }
    content <- httr::content(response)
    if (!is.null(content$models)) {
      all_models <- c(all_models, content$models)
    }

    if (!is.null(content$nextPageToken)) {
      page_token <- content$nextPageToken
    } else {
      break
    }
  }

  # Flatten to a tibble
  purrr::map(
    all_models,
    ~ Filter(Negate(is.null), .x) |> dplyr::as_tibble()
  ) |>
    purrr::list_rbind()
}

# -------------------------------------------------------------------------
# Ollama method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models ollama
list_models.ollama <- function(api, local_dir = "models", page_size = 50) {
  # We assume there's a custom function or endpoint for Ollama
  # that fetches local models from an Ollama server.
  # Here we call a hypothetical helper function to do that:
  res <- httr::GET(url = "http://localhost:11434/api/tags")
  handle_errors(res)
  models <- httr::content(res)

  purrr::map(
    models$models,
    function(x) tibble::tibble(
      name = x$name,
      model = x$model,
      modified_at = x$modified_at,
      size = x$size,
      digest = x$digest,
      details = list(x$details)
    )
  ) |> purrr::list_rbind()
}

# -------------------------------------------------------------------------
# Claude method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models claude
list_models.claude <- function(api, local_dir = "models", page_size = 50) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") {
    stop("Claude API key is not set in environment variables (ANTHROPIC_API_KEY).")
  }

  url <- "https://api.anthropic.com/v1/models"
  headers <- c(
    `x-api-key` = api_key,
    `anthropic-version` = "2023-06-01"
  )
  query_params <- list(limit = page_size)

  do_request <- function(qp) {
    httr::GET(
      url,
      httr::add_headers(.headers = headers),
      query = qp
    )
  }

  response <- do_request(query_params)
  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}")
  }
  content <- httr::content(response)

  all_models <- content$data
  while (isTRUE(content$has_more)) {
    query_params$after_id <- content$last_id
    response <- do_request(query_params)
    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}")
    }
    content <- httr::content(response)
    all_models <- c(all_models, content$data)
  }

  purrr::map(all_models, dplyr::as_tibble) |> purrr::list_rbind()
}

# -------------------------------------------------------------------------
# "llamafile" method (local directory)
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models llamafile
list_models.llamafile <- function(api, local_dir = "models", page_size = 50) {
  if (!dir.exists(local_dir)) {
    stop("Local path does not exist: ", local_dir)
  }
  # Return all .llamafile or .gguf files
  fs::dir_ls(local_dir, regexp = "\\.llamafile$|\\.gguf$")
}
