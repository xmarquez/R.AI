#' List Available Models
#'
#' A generic function that fetches model information from various sources
#' (Mistral, OpenAI, Groq, Cerebras, Cohere, Gemini, Claude, Ollama, or a local
#' directory for Llama.cpp compatible files). You specify which backend you want
#' by passing a string (e.g., `"mistral"`, `"openai"`, `"gemini"`, `"claude"`,
#' `"llamafile"`). Internally, the class of the string is set so that
#' `UseMethod("list_models")` can dispatch to the corresponding S3 method.
#' `"all"` returns all models.
#'
#' @param api `character`. Specifies the source to query. Must be one of:
#'   * `"mistral"`
#'   * `"openai"`
#'   * `"groq"`
#'   * `"cerebras"`
#'   * `"cohere"`
#'   * `"deepseek"`
#'   * `"gemini"`
#'   * `"ollama"`
#'   * `"claude"`
#'   * `"llama_cpp"`
#'
#' @param local_dir `character`. Path to the local directory containing models
#'   (used only when `api = "llama_cpp"`). Defaults to `"models"`.
#' @param mode If you set `api = "all"`, `mode` can be any of `"chat"`,
#'   `"embed"`, `"completion"`, `"rerank"`
#'
#' @param ... Other parameters passed on to methods
#'
#' @return A tidy data frame (`tibble`) when querying remote APIs, or a
#'   character vector of filenames for `"llama_cpp"`. Ollama returns a tibble of
#'   local models if you choose `"ollama"`.
#'
#' @section Environment Variables: For remote API queries, ensure the relevant
#'   environment variable is set:
#' \itemize{
#'   \item `MISTRAL_API_KEY` (for `"mistral"`)
#'   \item `OPENAI_API_KEY` (for `"openai"`)
#'   \item `GROQ_API_KEY` (for `"groq"`)
#'   \item `CEREBRAS_API_KEY` (for `"cerebras"`)
#'   \item `GEMINI_API_KEY` (for `"gemini"`)
#'   \item `ANTHROPIC_API_KEY` (for `"claude"`)
#'   \item `COHERE_API_KEY` (for `"cohere"`)
#'   \item `DEEPSEEK_API_KEY` (for `"deepseek"`)
#' }
#'
#' @examples
#' \dontrun{
#' ## List OpenAI models:
#' list_models("openai")
#'
#' ## List Claude models:
#' list_models("claude")
#'
#' ## List local llama files:
#' list_models("llamafile", local_dir = "my_local_models")
#' }
#'
#' @family model utilities
#' @export
list_models <- function(api, ...) {
  # Assign a class that matches the chosen API
  class(api) <- c(api, class(api))
  UseMethod("list_models", api)
}

# -------------------------------------------------------------------------
# Mistral method
# -------------------------------------------------------------------------

#' @rdname list_models
#' @exportS3Method list_models mistral
list_models.mistral <- function(api, ...) {
  id <- max_context_length <- completion_chat <- vision <- created <- NULL
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
    tidyr::unnest(completion_chat:vision) |>
    dplyr::mutate(created = lubridate::as_datetime(created),
                  mode = dplyr::case_when(completion_chat ~ "chat",
                                          stringr::str_detect(id, "embed") ~ "embed"))
}

# -------------------------------------------------------------------------
# OpenAI method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models openai
list_models.openai <- function(api, ...) {
  created <- NULL
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
  purrr::map(models, dplyr::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::mutate(created = lubridate::as_datetime(created),
                  mode = dplyr::case_when(stringr::str_detect(id, "embed") ~ "embed",
                                          stringr::str_detect(id, "moderation") ~ "moderation",
                                          stringr::str_detect(id, "whisper|tts") ~ "speech",
                                          stringr::str_detect(id, "dall-e") ~ "image",
                                          stringr::str_detect(id, "gpt-[0-9]|o1") ~ "chat"))
}

# -------------------------------------------------------------------------
# Groq method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models groq
list_models.groq <- function(api, ...) {
  created <- context_window <- NULL
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
    purrr::list_rbind() |>
    dplyr::mutate(created = lubridate::as_datetime(created),
                  mode = dplyr::case_when(stringr::str_detect(id, "whisper") ~ "speech",
                                          stringr::str_detect(id, "guard") ~ "moderation",
                                          TRUE ~ "chat")) |>
    dplyr::rename(max_context_length = context_window)
}

# -------------------------------------------------------------------------
# Cerebras method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models cerebras
list_models.cerebras <- function(api, ...) {
  created <- NULL
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
    purrr::list_rbind() |>
    dplyr::mutate(created = lubridate::as_datetime(created),
                  mode = "chat")
}

# -------------------------------------------------------------------------
# Gemini method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models gemini
list_models.gemini <- function(api, ...) {
  page_size <- 50
  id <- NULL
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
    purrr::list_rbind() |>
    tidyr::unnest("supportedGenerationMethods") |>
    dplyr::rename(id = "name",
                  display_name = "displayName",
                  max_context_length = "inputTokenLimit",
                  max_output_length = "outputTokenLimit",
                  default_model_temperature  = "temperature",
                  max_temperature = "maxTemperature",
                  default_model_topP = "topP",
                  default_model_topK = "topK") |>
    dplyr::mutate(id = stringr::str_remove(id, "models/"),
                  mode = dplyr::case_when(stringr::str_detect(supportedGenerationMethods, "embed") ~ "embed",
                                          stringr::str_detect(supportedGenerationMethods, "count") ~ "token_count",
                                          supportedGenerationMethods %in% c("generateText", "generateContent", "generateMessage") ~ "chat" ))
}

# -------------------------------------------------------------------------
# Ollama method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models ollama
list_models.ollama <- function(api, ...) {

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
  ) |> purrr::list_rbind() |>
    dplyr::rename(id = "name") |>
    dplyr::mutate(mode = dplyr::case_when(stringr::str_detect(id, "embed") ~ list("embed"),
                                          TRUE ~ list(c("chat", "completion")))) |>
    tidyr::unnest(mode)
}

# -------------------------------------------------------------------------
# Claude method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models claude
list_models.claude <- function(api, ...) {
  page_size <- 50
  created_at <- NULL
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

  purrr::map(all_models, dplyr::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::mutate(created = lubridate::as_datetime(created_at)) |>
    dplyr::select(-created_at) |>
    dplyr::rename(object = "type") |>
    dplyr::relocate(dplyr::all_of("object"), .after = "id")
}

# -------------------------------------------------------------------------
# "llama_cpp" method (local directory)
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models llama_cpp
list_models.llama_cpp <- function(api, local_dir = "models", ...) {
  if (!dir.exists(local_dir)) {
    stop("Local path does not exist: ", local_dir)
  }
  model_files <- fs::dir_ls(local_dir, regexp = "\\.llamafile.*|\\.gguf$")

  dplyr::tibble(id = basename(model_files) |>
                  stringr::str_remove("\\.llamafile.*|\\.gguf$"),
                model_file = model_files,
                created = fs::file_info(model_files)$modification_time,
                mode = dplyr::case_when(stringr::str_detect(model_file, "embed") ~ list("embed"),
                                        TRUE ~ list(c("chat", "completion")))) |>
    tidyr::unnest("mode")
}


# -------------------------------------------------------------------------
# Cohere method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models cohere
list_models.cohere <- function(api, ...) {
  page_size <- 20
  page_token <-  NULL
  id <- NULL

  cohere_api_key <- Sys.getenv("COHERE_API_KEY")
  if (cohere_api_key == "") {
    stop("Cohere API key is not set in environment variables (COHERE_API_KEY).")
  }

  base_url <- "https://api.cohere.com/v1/models"
  all_models <- list()

  repeat {
    # Build query parameters, dropping NULL ones
    query_params <- list(
      page_size = page_size,
      page_token = page_token
    )
    query_params <- query_params[!vapply(query_params, is.null, logical(1))]

    # Send GET request
    res <- httr::GET(
      url = base_url,
      query = query_params,
      httr::add_headers(Authorization = paste("Bearer", cohere_api_key))
    )
    if (httr::http_error(res)) {
      cli::cli_abort("{httr::http_status(res)$message}")
    }
    content <- httr::content(res)

    # Accumulate models
    if (!is.null(content$models)) {
      all_models <- c(all_models, content$models)
    }

    # Check pagination token
    if (!is.null(content$next_page_token)) {
      page_token <- content$next_page_token
    } else {
      break
    }
  }

  # Flatten into a tibble; rename columns for consistency
  tib <- purrr::map(
    all_models,
    function(m) {
      # Remove NULL fields so they don't cause trouble.
      m <- Filter(Negate(is.null), m)

      # Wrap character vectors in a list, so that each row is a list-column.
      if ("endpoints" %in% names(m)) {
        m$endpoints <- list(m$endpoints)
      }
      if ("default_endpoints" %in% names(m)) {
        m$default_endpoints <- list(m$default_endpoints)
      }

      dplyr::as_tibble(m)
    }
  ) |>
    purrr::list_rbind() |>
    tidyr::unnest("endpoints") |>
    tidyr::unnest("endpoints") |>
    dplyr::select(-"tokenizer_url", -"default_endpoints")


  # Rename 'name' -> 'id' and 'context_length' -> 'max_context_length'
  tib <- tib |>
    dplyr::rename(id = "name",
                  max_context_length = "context_length",
                  mode = "endpoints") |>
    dplyr::relocate(id, .before = dplyr::everything())

  tib
}


#' @rdname list_models
#' @exportS3Method list_models voyage
list_models.voyage <- function(api,
                               ...) {
  embed_models <- c("voyage-3-large", "voyage-3", "voyage-3-lite",
              "voyage-code-3", "voyage-finance-2", "voyage-multilingual-2",
              "voyage-law-2", "voyage-code-2")
  rerank_models <- c("rerank-2", "rerank-2-lite")

  models_df <- dplyr::tibble(
    id = c(embed_models, rerank_models),
    mode = c(rep("embed", length(embed_models)), rep("rerank", length(rerank_models)))
  )
  models_df
}

cost_info <- function() {
  input_cost_per_token <- litellm_provider <- model <- output_cost_per_token <- NULL
  json_file <- "https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"
  models <- jsonlite::fromJSON(json_file) |>
    purrr::map(\(x) tibble::as_tibble_row(x) |>
                 dplyr::mutate(dplyr::across(dplyr::everything(), as.character))) |>
    purrr::list_rbind(names_to = "model") |>
    dplyr::filter(model != "sample_spec") |>
    dplyr::mutate(dplyr::across(dplyr::matches("max_|cost_|output_vector_size"), as.numeric),
                  dplyr::across(dplyr::matches("supports"), as.logical))

  models_df <- models |>
    dplyr::mutate(api = dplyr::case_when(litellm_provider == "anthropic" ~ "claude",
                                  TRUE ~ litellm_provider),
           model = stringr::str_remove(model, "anthropic/|gemini/|groq/|mistral/|cohere/|voyage/"),
           input_cost = input_cost_per_token,
           output_cost = output_cost_per_token) |>
    dplyr::distinct()

  models_df
}

# -------------------------------------------------------------------------
# DeepSeek method
# -------------------------------------------------------------------------
#' @rdname list_models
#' @exportS3Method list_models deepseek
list_models.deepseek <- function(api, ...) {
  # Retrieve API key from environment
  deepseek_api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (!nzchar(deepseek_api_key)) {
    stop("DeepSeek API key is not set in environment variables (DEEPSEEK_API_KEY).")
  }

  base_url <- "https://api.deepseek.com/models"

  # Send GET request
  res <- httr::GET(
    url = base_url,
    httr::add_headers(
      "Authorization" = paste("Bearer", deepseek_api_key),
      "Accept"        = "application/json"
    )
  )
  if (httr::http_error(res)) {
    cli::cli_abort("{httr::http_status(res)$message}")
  }

  # Parse response, which should have shape:
  # {
  #   "object": "list",
  #   "data": [
  #     {
  #       "id": "deepseek-chat",
  #       "object": "model",
  #       "owned_by": "deepseek"
  #     },
  #     ...
  #   ]
  # }
  content <- httr::content(res)
  models <- content$data

  # Convert each model’s list to a tibble, then row-bind
  purrr::map(models, dplyr::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      # You can rename or reorder columns as desired
      created = NA,  # If the API doesn't provide a 'created' field
      mode    = "chat"
    ) |>
    dplyr::relocate(id, .before = dplyr::everything())
}


#' @rdname list_models
#' @exportS3Method list_models all
list_models.all <- function(api, mode = "chat", ...) {
  available <- supported_methods <- NULL
  available_apis <- get_available_apis(mode = mode) |>
    dplyr::filter(available)

  apis <- available_apis$api |>
    purrr::set_names(available_apis$api)
  model_list <- apis |>
    purrr::map(\(x) list_models(x)|>
                 dplyr::select(dplyr::any_of(c("id", "created", "mode")))) |>
    purrr::list_rbind(names_to = "api") |>
    dplyr::rename(supported_methods = "mode") |>
    dplyr::filter(supported_methods == mode)


  model_list
}

