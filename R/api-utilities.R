#' Get the Default Model for an API
#'
#' This function returns the default model for a specified API based on the
#' provided type. It selects the model from the internal `models_df` data frame
#' that contains supported models and their associated APIs.
#'
#' @param api A character string specifying the API for which the default model
#'   is needed. Must be one of "groq", "claude", "openai", "mistral", or
#'   "gemini".
#' @param type A character string specifying the type of model to return. Must
#'   be one of "cheapest", "largest", or "best". Default is "cheapest".
#'
#' @return A character string representing the default model for the specified
#'   API and type.
#'
#' @details The function checks the `models_df` data frame to find the models
#'   associated with the specified API. It then selects a model based on the
#'   given type:
#'   - "cheapest": Selects the model with the lowest input cost.
#'   - "largest": Selects the largest model based on predefined choices for
#'   each API.
#'   - "best": Selects the best model based on predefined choices for each API.
#'
#' @examples
#' # Get the cheapest model for the OpenAI API
#' get_default_model("openai", type = "cheapest")
#'
#' # Get the largest model for the GROQ API
#' get_default_model("groq", type = "largest")
#'
#' @seealso [get_available_models()] for retrieving all available models for an
#'   API and [get_available_apis()] for retrieving all available APIs.
#' @family model utilities
#'
#' @export
get_default_model <- function(api, type = "cheapest") {
  checkmate::assert_choice(api, get_available_apis(mode = "chat")$api)
  type <- match.arg(type, c("cheapest", "largest", "best"))

  model <- preferred_models[preferred_models$api == api, ] |>
    dplyr::pull(dplyr::all_of(c(type)))

  cli::cli_inform(c("i" = "Using the {type} model for the {api} API, {model}.",
                    "i" = "Get available models with {.fun get_available_models}."))
  model
}


#' Get Available Models for an API
#'
#' This function returns the available models for a specified API from the
#' internal `models_df` data frame that contains supported models and their
#' associated APIs. If no API is specified, it returns all available models
#' across all APIs.
#'
#' @param api An optional character string specifying the API for which the
#'   available models are needed. Must be one of "groq", "claude", "openai",
#'   "mistral", or "gemini". If not provided, the function returns models for
#'   all APIs.
#' @param mode The model use: must be one of `"chat"`, `"embedding"`, or
#'   `"rerank"`.
#'
#' @return A character vector containing the available models for the specified
#'   API, or all available models if no API is specified.
#'
#' @details The function checks the `models_df` data frame to find the models
#'   associated with the specified API. If no API is provided, it returns all
#'   models from `models_df`. Typing `R.AI:::models_df` should produce the
#'   models dataframe.
#'
#' @examples
#' # Get available models for the OpenAI API
#' get_available_models("openai")
#'
#' # Get all available models across all APIs
#' get_available_models()
#'
#' @seealso [get_default_model()] for retrieving the default model for a
#'   specified API.
#' @family model utilities
#'
#' @export
get_available_models <- function(api, mode = "chat") {
  completion_chat <- NULL
  checkmate::assert_choice(mode, c("chat", "embedding", "rerank", "completion"))
  if (!missing(api)) {
    checkmate::assert_choice(api, get_available_apis(mode = mode)$api)
    if(api == "llama_cpp") {
      llamafile_models <- fs::dir_ls(recurse = TRUE, regexp = "*.llamafile$|*.llamafile.exe|*.gguf$", type = "file") |>
        basename() |>
        stringr::str_remove(".llamafile.*|*.gguf")
      return(llamafile_models)

    }
    models <- models_df[models_df$api == api & models_df$mode == mode, ]$model
    if(api == "mistral" && mode == "chat") {
      mistral_models <- list_models("mistral") |>
        tidyr::unnest(completion_chat) |>
        dplyr::filter(completion_chat)
      models <- unique(c(models, mistral_models$id))
    }
  } else {
    llamafile_models <- fs::dir_ls(recurse = TRUE, regexp = "*.llamafile$|*.llamafile.exe", type = "file") |>
      basename() |>
      stringr::str_remove(".llamafile.*")
    models <- models_df[models_df$mode == mode, ]$model
    models <- unique(c(models, llamafile_models))
  }
  models
}

#' Check if API Key is Available
#'
#' This function checks whether an API key is available for a specified API.
#'
#' @param api A character string specifying the API to check for an API key.
#'   Must be one of "groq", "claude", "openai", or "gemini".
#'
#' @return A logical value indicating whether an API key is available for the
#'   specified API.
#'
#' @details The function checks the environment variables for the presence of
#'   an API key corresponding to the specified API. The environment variables
#'   are expected to follow the pattern "API_KEY_<API_NAME>", where <API_NAME>
#'   is the uppercase version of the API name.
#'
#' @examples
#' # Check if an API key is available for the OpenAI API
#' is_api_key_available("openai")
#'
#' @family model utilities
#'
#' @export
is_api_key_available <- function(api) {

  if(api == "claude") {
    api <- "anthropic"
  }
  api_key_env_var <- paste0(toupper(api), "_API_KEY")
  !is.na(Sys.getenv(api_key_env_var, unset = NA))
}

#' Check Availability of API Keys for All APIs
#'
#' This function returns a data frame that includes the APIs from the internal
#' `models_df` data and whether an API key is available for each API.
#'
#' @param mode Can be "chat", "embed", "rerank", or "completion". Default is "chat".
#'
#' @return A data frame with columns `api`, `key_available`, `key_needed`, and
#'   `available`. The `api` column lists the APIs, and the `key_available`
#'   column indicates whether an API key is available for each API. The
#'   `key_needed` column lists whether an API key is needed to use.
#'
#' @details The function checks the environment variables for the presence of an
#'   API key corresponding to each API in the `models_df` data frame. The
#'   environment variables are expected to follow the pattern
#'   "<API_NAME>_API_KEY", where <API_NAME> is the uppercase version of the API
#'   name. (Except for the Claude API, where the environment variable is
#'   "ANTHROPIC_API_KEY").
#'
#' @seealso [get_available_models()] for retrieving all available models for an
#'   API.
#'
#' @examples
#' # Get the availability of API keys for all APIs
#' get_available_apis()
#'
#' @family model utilities
#' @export
get_available_apis <- function(mode = "chat") {
  key_available <- key_needed <- NULL
  checkmate::assert_choice(mode, c("chat", "embed", "rerank", "completion"))
  df <- utils::methods(mode) |>
    stringr::str_remove("(^chat|^embed|^rerank|^completion).") |>
    stringr::str_remove("_list$|_character$") |>
    purrr::map_dfr(~tibble::tibble(api = .x, key_available = is_api_key_available(.x)))
  df <- df |>
    dplyr::mutate(key_needed = dplyr::case_when(api %in% c("llama_cpp", "ollama") ~ FALSE,
                                                TRUE ~ TRUE),
                  available = key_available | !key_needed)
  df

}


