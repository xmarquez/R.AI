#' Call the Mistral API
#'
#' This function sends a single prompt to the Mistral API for processing,
#' allowing users to customize parameters such as the model, number of candidates,
#' and more. It also handles retries and extracts the relevant response content.
#'
#' @param prompts A list of messages, each with a role (e.g., "user" or
#'   "assistant") and content, used as input for the model. Must be in the
#'   format `list(list(role = "user", content = "Hello world!"))`.
#' @param model The model to use for generating responses. Defaults to a
#'   suitable model depending on the type of prompts. If not provided, it will
#'   use a default model determined by the prompt type.
#' @param prompt_name A character string specifying the prompt type. If not
#'   provided, it defaults to "json" if `json_mode` is true, otherwise "default".
#' @param n_candidates The number of response candidates to generate. Defaults
#'   to 1.
#' @param max_retries The maximum number of retry attempts in case of request
#'   failures. Defaults to 10.
#' @param temperature A numeric value between 0 and 1 that controls the
#'   randomness of the response. Higher values make the output more random.
#'   Defaults to 0.2.
#' @param top_p A numeric value that specifies nucleus sampling probability.
#'   It controls the probability mass of the tokens considered at each step.
#'   Defaults to 1.
#' @param max_tokens The maximum number of tokens to include in the response.
#'   Defaults to 300.
#' @param stop Optional. A character vector specifying the sequences where the
#'   model should stop generating further tokens. Defaults to `NULL`.
#' @param random_seed An optional integer specifying a random seed for
#'   reproducibility. Defaults to `NULL`.
#' @param presence_penalty A numeric value that encourages the model to talk about
#'   new topics. Defaults to 0.
#' @param frequency_penalty A numeric value that decreases the likelihood of repeating
#'   the same line verbatim. Defaults to 0.
#' @param safe_prompt A logical value indicating whether to inject a safety prompt
#'   before all conversations. Defaults to `FALSE`. When set to `TRUE`, an additional
#'   safety layer is added to the prompt to ensure content safety.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param log A logical value indicating whether the function should log the API
#'   request details. Defaults to `TRUE`.
#' @param stream Optional. A logical value that, if set to `TRUE`, indicates that the
#'   model's output should be streamed back in chunks. Defaults to `FALSE`.
#' @export
#'
#' @return A tibble containing the usage statistics (tokens used) and the
#'   generated response(s).
call_api.mistral <- function(prompts,
                             model,
                             prompt_name,
                             n_candidates = 1,
                             max_retries = 10,
                             temperature = 0.2,
                             top_p = 1,
                             max_tokens = 300,
                             stop = NULL,
                             random_seed = NULL,
                             presence_penalty = 0,
                             frequency_penalty = 0,
                             safe_prompt = FALSE,
                             pause_cap = 1200,
                             log = TRUE) {

  if(missing(model)) {
    model <- get_default_model(class(prompts)[1])
  }

  if(missing(prompt_name)) {
    if(json_mode) {
      prompt_name <- "json"
    } else {
      prompt_name <- "default"
    }
  }

  validate_args_call_api(prompts = prompts,
                         model = model,
                         prompt_name = prompt_name,
                         n_candidates = n_candidates,
                         max_retries = max_retries,
                         temperature = temperature,
                         max_tokens = max_tokens,
                         top_p = top_p,
                         stop = stop,
                         random_seed = random_seed,
                         presence_penalty = presence_penalty,
                         frequency_penalty = frequency_penalty,
                         safe_prompt = safe_prompt,
                         pause_cap = pause_cap,
                         log = log)

  single_request_fun <- paste("mistral_single_request")
  response_validation_fun <- paste("mistral", prompt_name, "response_validation", sep = "_")
  content_extraction_fun <- paste("mistral", prompt_name, "content_extraction", sep = "_")

  NextMethod(prompts,
             model = model,
             prompt_name = prompt_name,
             max_retries = max_retries,
             n_candidates = n_candidates,
             temperature = temperature,
             top_p = top_p,
             max_tokens = max_tokens,
             stream = stream,
             stop = stop,
             random_seed = random_seed,
             presence_penalty = presence_penalty,
             frequency_penalty = frequency_penalty,
             safe_prompt = safe_prompt,
             response_validation_fun = response_validation_fun,
             content_extraction_fun = content_extraction_fun,
             single_request_fun = single_request_fun,
             pause_cap = pause_cap,
             log = log)
}

#' Send a Single Request to the Mistral API
#'
#' This function sends a single prompt to the Mistral API for processing,
#' allowing users to customize parameters such as model, number of candidates,
#' and more. It also handles retries and extracts the relevant response content.
#'
#' @inheritParams openai_single_request
#' @param safe_prompt A logical value indicating whether to inject a safety prompt before all conversations.
#'   Default is `FALSE`. When set to `TRUE`, an additional safety layer is added to the prompt to ensure content safety.
#' @export
mistral_single_request <- function(prompt,
                                   model,
                                   n_candidates = 1,
                                   max_retries = 10,
                                   temperature = 0.2,
                                   top_p = 1,
                                   max_tokens = 300,
                                   stream = FALSE,
                                   stop = NULL,
                                   random_seed = NULL,
                                   presence_penalty = 0,
                                   frequency_penalty = 0,
                                   safe_prompt = FALSE,
                                   response_validation_fun,
                                   content_extraction_fun,
                                   pause_cap = 1200,
                                   quiet = FALSE) {

  body <- jsonlite::toJSON(
    list(
      model = model,
      temperature = temperature,
      top_p = top_p,
      max_tokens = max_tokens,
      stream = stream,
      stop = stop,
      random_seed = random_seed,
      messages = prompt,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      n = n_candidates,
      safe_prompt = safe_prompt
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- retry_response(base_url = "https://api.mistral.ai/v1/chat/completions",
                        api_key = Sys.getenv("MISTRAL_API_KEY"),
                        response_format = NULL,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  httr::stop_for_status(res)
  response <- httr::content(res)

  df <- mistral_usage(response)
  if (missing(content_extraction_fun)) {
    content_extraction_fun <- get("mistral_default_content_extraction")
  }

  content <- do.call(content_extraction_fun, list(response))

  df <- df |>
    dplyr::mutate(response = list(content))

  df
}

mistral_usage <- function(response) {
  usage_stats <- dplyr::tibble(prompt_tokens = response$usage$prompt_tokens,
                               completion_tokens  = response$usage$completion_tokens,
                               total_tokens = response$usage$total_tokens) |>
    dplyr::mutate(model = response$model)

  usage_stats
}

mistral_default_content_extraction <- function(response_content) {
  response_content$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

mistral_json_content_extraction <- function(response_content) {
  response_content |>
    mistral_default_content_extraction() |>
    default_json_content_extraction()
}

