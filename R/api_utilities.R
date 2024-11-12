#' Get the Default Model for an API
#'
#' This function returns the default model for a specified API based on the
#' provided type. It selects the model from the internal `models_df` data frame
#' that contains supported models and their associated APIs.
#'
#' @param api A character string specifying the API for which the default model
#'   is needed. Must be one of "groq", "claude", "openai", or "gemini".
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
#'     each API.
#'   - "best": Selects the best model based on predefined choices for each API.
#'
#' @examples
#' # Get the cheapest model for the OpenAI API
#' get_default_model("openai", type = "cheapest")
#'
#' # Get the largest model for the GROQ API
#' get_default_model("groq", type = "largest")
#'
#' @seealso [get_available_models()] for retrieving all available models for
#'   an API and [get_available_apis()] for retrieving all available APIs.
#'
#' @export
get_default_model <- function(api, type = "cheapest") {
  checkmate::assert_choice(api, c("groq", "claude", "openai", "gemini"))
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
#'   or "gemini". If not provided, the function returns models for all APIs.
#'
#' @return A data frame containing the available models for the specified API,
#'   or all available models if no API is specified.
#'
#' @details The function checks the `models_df` data frame to find the models
#'   associated with the specified API. If no API is provided, it returns all
#'   rows from the `models_df`.
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
#'
#' @export
get_available_models <- function(api) {
  if (!missing(api)) {
    checkmate::assert_choice(api, c("groq", "claude", "openai", "gemini"))
    models <- models_df[models_df$api == api & models_df$mode == "chat", ]
  } else {
    models <- models_df
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
#' @export
is_api_key_available <- function(api) {
  checkmate::assert_choice(api, c("groq", "claude", "openai", "gemini"))

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
#' @return A data frame with columns `api` and `key_available`. The `api` column
#'   lists the APIs, and the `key_available` column indicates whether an API key
#'   is available for each API.
#'
#' @details The function checks the environment variables for the presence of an
#'   API key corresponding to each API in the `models_df` data frame. The
#'   environment variables are expected to follow the pattern
#'   "<API_NAME>_API_KEY", where <API_NAME> is the uppercase version of the API
#'   name. (Except for the Claude API, where the environment variable is
#'   "ANTHROPIC_API_KEY").
#'
#' @seealso [get_available_models()] for retrieving all available models for
#'   an API.
#'
#' @examples
#' # Get the availability of API keys for all APIs
#' get_available_apis()
#'
#' @export
get_available_apis <- function() {
  unique_apis <- unique(models_df$api)

  api_key_status <- sapply(unique_apis, is_api_key_available)

  tibble::tibble(
    api = unique_apis,
    key_available = api_key_status
  )
}
