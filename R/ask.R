#' Ask a Language Model
#'
#' The `ask` function provides a unified interface for sending a single prompt
#' to a language model API. It supports different input types and APIs, ensuring
#' integration with models from the Groq, Gemini, OpenAI, Anthropic, and Mistral
#' APIs, and local llamafiles.
#'
#' @param prompt The input prompt to send to the API. Can be a character vector
#'   (including file names - see [prompt_list()]), a prompt produced by
#'   [prompt()], or an object of class `combined_prompts` produced by
#'   [prompt_list()].
#' @param api A string specifying the API to use. Must be one of `"gemini"`,
#'   `"claude"`, `"openai"`, `"llamafile"`, "`groq`", or `"mistral"`. Must be
#'   specified if the prompt is a character vector.
#' @param system Optional. A character string providing system-level
#'   instructions or context for the API.
#' @param data Optional. A data frame containing variables to be interpolated
#'   into `prompt` using [stringr::str_glue_data()].
#' @param model A character string specifying the model to use. Defaults to the
#'   model returned by [get_default_model()] for the specified API.
#' @param output Where to write the response. Defaults to `stdout()`. If a
#'   character string, writes the response to the specified file. Ignored if
#'   `response_only` is `FALSE`.
#' @param response_only Logical. If `TRUE`, returns only the response content
#'   from the API. If `FALSE`, returns the full response object, including usage
#'   statistics. Defaults to `TRUE`.
#' @param ... Additional arguments passed to the API call in [call_api()],
#'   including, e.g., `temperature` or `max_tokens`. See the documentation in
#'   [call_api()] for more.
#'
#' @return The model's response. By default, the function returns only the
#'   response content as a character string. If `response_only = FALSE`, the
#'   full response object is returned as a [tibble::tibble()], including usage
#'   statistics. Defaults to `TRUE`.
#'
#' @seealso
#' - [prompt_list()] for creating structured prompts from user input.
#' - [call_api()] for sending API requests with structured prompts.
#'
#' @examples
#' \dontrun{
#' # Ask a question using OpenAI API
#' response <- ask(
#'   prompt = "What is the capital of France?",
#'   api = "openai",
#'   system = "You are a helpful assistant."
#' )
#' print(response)
#'
#' # Save the response to a file
#' ask(
#'   prompt = "Explain quantum computing in a brief poem.",
#'   api = "gemini",
#'   system = "You are a helpful tutor.",
#'   output = "response.txt",
#' )
#' }
#'
#' @family chat
#' @export
ask <- function(prompt, api, system, data, model, output = stdout(), response_only = TRUE, ...) {
  UseMethod("ask")
}

#' @export
ask.default <- function(prompt, api, system, data, model,
                        output = stdout(), response_only = TRUE, ...) {
  if(missing(api)) {
    api <- class(prompt)[1]
    checkmate::assert_choice(api, c("gemini", "claude", "openai", "llamafile", "mistral", "groq"))
  }
  if(!"combined_prompts" %in% class(prompt)) {
    old_class <- class(prompt)
    prompt <- list(default = prompt)
    class(prompt) <- old_class
  }

  response <- call_api(prompts = prompt, model, ...)
  if(response_only) {
    response <- unlist(response$response)
    if(is.character(output)) {
      writeLines(response, con = output)
      return(invisible(response))
    }
  }
  response
}

#' @export
ask.character <- function(prompt, api, system, data, model,
                          output = stdout(), response_only = TRUE, ...) {
  checkmate::assert_choice(api, c("gemini", "claude", "openai", "llamafile", "mistral", "groq"))

  prompt <- prompt_list(prompt, api, system, data)

  NextMethod(prompt = prompt, api = api,
             system, data,
             model, output,
             response_only, ...)
}

#' @export
ask.combined_prompts <- function(prompt, api, system, data, model,
                          output = stdout(), response_only = TRUE, ...) {
  api <- class(prompt)[1]

  if(missing(model)) {
    model <- get_default_model(api = class(prompt)[1])
  }
  NextMethod(prompt = prompt, api = api,
             system, data,
             model = model, output = output,
             response_only = response_only, ...)
}
