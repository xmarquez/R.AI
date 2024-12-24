#' Create a Chat Completion
#'
#' A generic function for creating chat completions. The default method
#' dispatches to different registered backends, such as OpenAI.
#'
#' @param message The message(s) to create a completion for.
#' @param ... Additional arguments passed to methods.
#'
#' @section Supported Backends:
#'
#' - OpenAI: Dispatches to the [OpenAI Chat API](https://platform.openai.com/docs/api-reference/chat).
#' - Gemini: Dispatches to the [Gemini Chat API](https://ai.google.dev/api/generate-content).
#' - Claude: Dispatches to the [Anthropic Claude API](https://docs.anthropic.com/en/api/messages).
#' - Mistral: Dispatches to the [Mistral Chat API](https://docs.mistral.ai/api/#tag/chat).
#' - Groq: Dispatches to the [Groq Chat API](https://console.groq.com/docs/api-reference#chat).
#' - Cerebras: Dispatches to the [Cerebras Chat API](https://inference-docs.cerebras.ai/api-reference/chat-completions).
#' - Llama.cpp: Dispatches to a local LlamaFile instance or a Llama.CPP server running a GGUF formatted model.
#' - Ollama: Dispatches to a local model running on an [Ollama local server](https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion).
#'
#' @section Environment Variables:
#'
#'   Each backend, except for local backends (llama_cpp and ollama), requires an
#'   API key to be set in the appropriate environment variable. Use
#'   [get_available_apis()] to check if a key is set. Most keys should be of the
#'   form `API_NAME_API_KEY`, except for Claude, where the key should be
#'   `ANTHROPIC_API_KEY`. Set key by using Sys.setenv("API_NAME_API_KEY" =
#'   "YOUR_API_KEY"), or saving it in your .Renviron file.
#'
#' @param messages A list of user and system messages in the correct format for
#'   the intended API. This is typically produced by [format_chat()], with class
#'   `c("{api}_list", "list")`. The class should match the backend API.
#' @param `...`: Additional arguments passed to backend methods.
#'
#' @param model A string specifying the ID of the model to use. Check the
#'   documentation for your chosen API for details or use [list_models()] to
#'   find available models. The default is typically set with
#'   [get_default_model()]. These are usually the cheapest good models for each
#'   API as of the time of writing this documentation (`r lubridate::today()`):
#'
#'   ```{r}
#'   preferred_models[,1:2]
#'   ```
#'
#' @param temperature Numeric, defaults to 0.2. Controls the randomness of the
#'   sampling. Higher values (closer to 1) increase randomness, while lower
#'   values (closer to 0) make outputs more deterministic.
#' @param max_tokens Integer, defaults to 300. Maximum number of tokens to
#'   generate. The API may stop earlier than this limit.
#' @param presence_penalty Numeric or `NULL`. A value between -2.0 and 2.0 that
#'   penalizes new tokens if they appear in the text so far, encouraging the
#'   model to talk about new topics. Defaults to `NULL` (no penalty).
#' @param top_p Numeric or `NULL`, defaults to 1. Another way of controlling
#'   randomness via nucleus sampling. Only the tokens comprising the `top_p`
#'   probability mass are considered.
#' @param tools List or `NULL`, specifying a list of tools (functions) the model
#'   may call.
#' @param tool_choice Controls which (if any) tool is called by the model.
#'    - For Anthropic: List or `NULL`. Specifies how the model should use tools. For
#'   example, `list(name = "get_stock_price", description = "Get stock price",
#'   input_schema = ...)`.
#'    - For Openai String or list or `NULL`. "none" means no
#'   tool is called. "auto" means the model can choose. A named list
#'   `list(type="function", function=list(name="my_function"))` would force the
#'   model to call that tool. Defaults to `NULL`.
#' @param top_k Integer or `NULL`, defaults to `NULL`. Limits sampling to the
#'   top K tokens for each step.
#' @param max_retries Integer, defaults to 3. Maximum number of retries if the
#'   API request fails.
#' @param quiet Logical, defaults to `FALSE`. If `TRUE`, suppress messages
#'   during retries.
#'
#' @return A response object from the chosen backend. All objects returned by
#'   chat methods should have class `{api}_chat`, so that they can be dispatched
#'   to [get_message()], [get_content()], and other methods.
#'
#' @export
chat <- function(messages, ...) {
  UseMethod("chat")
}

#' @export
class_prompt_for_api <- function(prompt, api) {
  structure(prompt, class = c(paste(api, class(prompt), sep = "_"),
                              class(prompt)))
}

#' @export
get_content <- function(response, ...) {
  UseMethod("get_content")
}

#' @export
get_message <- function(response, ...) {
  UseMethod("get_message")
}

#' @export
completion <- function(prompt, ...) {
  UseMethod("completion")
}

#' @export
embed <- function(content, ...) {
  UseMethod("embed")
}

#' @export
get_usage <- function(response, ...) {
  UseMethod("get_usage")
}

#' @exportS3Method
get_usage.default <- function(response, ...) {
  usage <- response$usage

  # Extract main columns
  prompt_tokens <- usage$prompt_tokens %||% NULL
  completion_tokens <- usage$completion_tokens %||% NULL
  total_tokens <- usage$total_tokens %||% NULL

  # Extract details columns (ensure they are lists or NULL)
  prompt_tokens_details <- (usage$prompt_tokens_details %||% NULL) |> dplyr::as_tibble()
  completion_tokens_details <- (usage$completion_tokens_details %||% NULL)  |> dplyr::as_tibble()

  if(nrow(prompt_tokens_details) == 0) {
    prompt_tokens_details <- NULL
  }

  if(nrow(completion_tokens_details) == 0) {
    completion_tokens_details <- NULL
  }
  # Construct a tibble
  tibble::tibble(
    input_tokens = prompt_tokens,
    output_tokens = completion_tokens,
    total_tokens = total_tokens,
    input_tokens_details = list(prompt_tokens_details),
    output_tokens_details = list(completion_tokens_details),
    model = response$model
  )
}
#' @export
tool_calls <- function(response, ...) {
  UseMethod("tool_calls")
}

#' @export
tokenize <- function(content, ...) {
  UseMethod("tokenize")
}

#' @export
detokenize <- function(tokens, ...) {
  UseMethod("detokenize")
}

#' @exportS3Method
as.matrix.embedding <- function(embedding) {
  ncol <- length(embedding[[1]])
  nrow <- length(embedding)

  res <- matrix(unlist(embedding), nrow = nrow, ncol = ncol)
  res
}
