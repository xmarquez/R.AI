#' List Available Models
#'
#' Fetches model information from Mistral, OpenAI, Gemini, or a local directory
#' (llamafile).
#'
#' @param api `character` Specifies the source to query:
#'   * `"mistral"` - Fetch models from the Mistral API.
#'   * `"openai"` - Fetch models from the OpenAI API.
#'   * `"gemini"` - Fetch models from the Gemini API.
#'   * `"llamafile"` - List locally stored models in a directory.
#'
#' @param local_dir `character` Path to the local directory containing models
#'   (used only when `api = "llamafile"`). Defaults to `"models"`.
#'
#' @param page_size `integer` Maximum number of models to return (applies to
#'   `"gemini"` only). Defaults to 50.
#'
#' @return A tidy data frame (`tibble`) for API queries or a `character` vector
#'   of filenames for `llamafile`.
#'
#' @section Environment Variables: Ensure the following environment variables
#'   are set for API queries:
#' * `MISTRAL_API_KEY`: Required for `"mistral"`.
#' * `OPENAI_API_KEY`: Required for `"openai"`.
#' * `GEMINI_API_KEY`: Required for `"gemini"`.
#'
#' @family model utilities
#'
#' @export
list_models <- function(api, local_dir = "models", page_size = 50) {
  id <- max_context_length <- completion_chat <- vision <- NULL
  # Validate the API parameter
  if (!api %in% c("mistral", "openai", "gemini", "llamafile")) {
    stop("Invalid API option. Choose 'mistral', 'openai', 'gemini', or 'llamafile'.")
  }

  # Handle Mistral API
  if (api == "mistral") {
    api_key <- Sys.getenv("MISTRAL_API_KEY")
    if (api_key == "") stop("Mistral API key is not set in environment variables.")

    url <- "https://api.mistral.ai/v1/models"
    response <- httr::GET(url,
                          httr::add_headers(Authorization = paste("Bearer", api_key)))

    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
    }

    models <- httr::content(response)

    models <- models$data |>
      jsonlite::toJSON() |>
      jsonlite::fromJSON() |>
      tidyr::unnest(id:max_context_length) |>
      tidyr::unnest(completion_chat:vision)

    return(models)
  }

  # Handle OpenAI API
  if (api == "openai") {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") stop("OpenAI API key is not set in environment variables.")

    url <- "https://api.openai.com/v1/models"
    response <- httr::GET(url,
                          httr::add_headers(Authorization = paste("Bearer", api_key)))

    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
    }

    models <- httr::content(response)

    models <- models$data |>
      purrr::map(dplyr::as_tibble) |>
      purrr::list_rbind()

    return(models)
  }

  # Handle Gemini API
  if (api == "gemini") {
    api_key <- Sys.getenv("GEMINI_API_KEY")
    if (api_key == "") stop("Gemini API key is not set in environment variables.")

    url <- "https://generativelanguage.googleapis.com/v1beta/models"
    query_params <- list(key = api_key, pageSize = page_size)

    response <- httr::GET(url, query = query_params)

    if (httr::http_error(response)) {
      cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
    }

    models <- httr::content(response)

    # Handle pagination if nextPageToken is present
    all_models <- models$models
    while (!is.null(models$nextPageToken)) {
      query_params$pageToken <- models$nextPageToken
      response <- httr::GET(url, query = query_params)
      models <- httr::content(response)
      all_models <- c(all_models, models$models)
    }

    all_models <- all_models |>
      purrr::map(\(x) Filter(Negate(is.null), x) |>
                   dplyr::as_tibble()) |>
      purrr::list_rbind()
    return(all_models)
  }

  # Handle local "llamafile" API
  if (api == "llamafile") {
    if (!dir.exists(local_dir)) stop("Local path does not exist: ", local_dir)
    return(fs::dir_ls(local_dir, glob = "*.llamafile*"))
  }
}
