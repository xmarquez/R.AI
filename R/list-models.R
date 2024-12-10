#' List Available Models
#'
#' Fetches model information from Mistral, OpenAI, or a local directory
#' (llamafile).
#'
#' @param api `character` Specifies the source to query:
#'   * `"mistral"` - Fetch models from the Mistral API.
#'   * `"openai"` - Fetch models from the OpenAI API.
#'   * `"llamafile"` - List locally stored models in a directory.
#'
#' @param local_dir `character` Path to the local directory containing models
#'   (used only when `api = "llamafile"`). Defaults to `"models"`.
#'
#' @return A tidy data frame (`tibble`) for API queries or a `character` vector
#'   of filenames for `llamafile`.
#'
#' @section Environment Variables: Ensure the following environment variables
#'   are set for API queries:
#' * `MISTRAL_API_KEY`: Required for `"mistral"`.
#' * `OPENAI_API_KEY`: Required for `"openai"`.
#'
#' @family model utilities
#'
#' @export
list_models <- function(api, local_dir = "models") {
  id <- max_context_length <- completion_chat <- vision <- NULL
  # Validate the API parameter
  if (!api %in% c("mistral", "openai", "llamafile")) {
    stop("Invalid API option. Choose 'mistral', 'openai', or 'llamafile'.")
  }

  # Handle Mistral API
  if (api == "mistral") {
    api_key <- Sys.getenv("MISTRAL_API_KEY")
    if (api_key == "") stop("Mistral API key is not set in environment variables.")

    url <- "https://api.mistral.ai/v1/models"
    response <- httr::GET(url,
                          httr::add_headers(Authorization = paste("Bearer", api_key)))

    httr::stop_for_status(response)
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

    httr::stop_for_status(response)
    models <- httr::content(response)

    models <- models$data |>
      purrr::map(dplyr::as_tibble) |>
      purrr::list_rbind()

    return(models)
  }

  # Handle local "llamafile" API
  if (api == "llamafile") {
    if (!dir.exists(local_dir)) stop("Local path does not exist: ", local_dir)
    return(fs::dir_ls(local_dir, glob = "*.llamafile*"))
  }
}
