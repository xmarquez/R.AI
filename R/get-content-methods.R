#' Extract Main Text Content from a Response
#'
#' A generic function that extracts the **textual content** from various LLM
#' (Large Language Model) response objects (e.g., OpenAI, Gemini, Claude, Cerebras,
#' etc.). By default, it returns **one character element per generated candidate** or choice.
#'
#' @param response A response object returned by a chat or completion method. Each
#'   backend typically stores the generated text content in a slightly different place.
#'
#' @return A character vector. Usually one element per "choice" or "candidate"
#'   that the backend produced. If a backend only ever returns one choice, then
#'   you'll typically get a length-1 character vector.
#'
#' @section OpenAI:
#'   - `get_content.openai_chat()` returns a character vector of length `n`, where `n`
#'     is the number of choices (`response$choices`).
#'
#' @section Gemini:
#'   - `get_content.gemini_chat()` returns **one string per candidate** in
#'     `response$candidates`. Each candidate’s text may be split into multiple
#'     parts (`candidate$content$parts`). These parts are concatenated. If there
#'     are multiple candidates, you get a vector of length > 1.
#'
#' @section Other Methods:
#'   - **Claude** (`claude_chat`): Returns the `text` from the first item in
#'     `response$content`.
#'   - **Cerebras** (`cerebras_chat`): Returns the `content` from the first
#'     item in `response$choices`.
#'   - **Groq** (`groq_chat`): Similar to OpenAI, returns `message$content` from
#'     the first item in `response$choices`.
#'   - **Llama.CPP** (`llama_cpp_completion`, `llama_cpp_chat`): May differ
#'     slightly between “completion” and “chat” responses. For chat style,
#'     returns a vector of choices (if multiple). For completion style,
#'     typically a single string in `response$content`.
#'   - **Mistral** (`mistral_chat`): Modeled after the same pattern; returns
#'     multiple strings if the API returned multiple choices.
#'   - **Ollama** (`ollama_completion`, `ollama_chat`): Depending on whether
#'     it’s a chat or a completion, the text may be in different fields:
#'     `response$message$content` or `response$response`.
#'
#' @examples
#' \dontrun{
#' # OpenAI example (may return multiple choices if n>1)
#' resp_oa <- chat.openai_list(messages, model="gpt-3.5-turbo")
#' texts_oa <- get_content(resp_oa)
#' print(texts_oa)
#'
#' # Gemini example with multiple candidates
#' resp_gem <- chat.gemini_list(messages, model="gemini-v1", ...)
#' texts_gem <- get_content(resp_gem)
#' print(texts_gem) # Possibly a vector of multiple candidates
#'
#' # Claude example
#' resp_claude <- chat.claude_list(messages)
#' text_claude <- get_content(resp_claude)
#' cat(text_claude)
#' }
#'
#' @export
get_content <- function(response) {
  UseMethod("get_content")
}

#' @rdname get_content
#' @exportS3Method get_content openai_chat
get_content.openai_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

#' @rdname get_content
#' @exportS3Method get_content cerebras_chat
get_content.cerebras_chat <- function(response) {
  # Typically Cerebras returns exactly one choice
  response$choices[[1]]$message$content
}

#' @rdname get_content
#' @exportS3Method get_content claude_chat
get_content.claude_chat <- function(response) {
  # Claude often stores the main text in response$content[[1]]$text
  response$content[[1]]$text
}

#' @rdname get_content
#' @exportS3Method get_content gemini_chat
get_content.gemini_chat <- function(response) {
  # If the API returns multiple candidates, each candidate may have multiple 'parts'.
  # Here we concatenate parts within each candidate, and return a character vector of length(#candidates).
  response$candidates |>
    purrr::map_chr(\(candidate) {
      candidate$content$parts |>
        purrr::map_chr("text") |>
        paste(collapse = "")
    })
}

#' @rdname get_content
#' @exportS3Method get_content groq_chat
get_content.groq_chat <- function(response) {
  response$choices[[1]]$message$content
}

#' @rdname get_content
#' @exportS3Method get_content llama_cpp_completion
get_content.llama_cpp_completion <- function(response) {
  response$content
}

#' @rdname get_content
#' @exportS3Method get_content llama_cpp_chat
get_content.llama_cpp_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

#' @rdname get_content
#' @exportS3Method get_content mistral_chat
get_content.mistral_chat <- function(response) {
  response$choices |>
    purrr::map(\(x) purrr::pluck(x, "message", "content")) |>
    unlist()
}

#' @rdname get_content
#' @exportS3Method get_content ollama_completion
get_content.ollama_completion <- function(response) {
  response$response
}

#' @rdname get_content
#' @exportS3Method get_content ollama_chat
get_content.ollama_chat <- function(response) {
  response$message$content
}

#' @rdname get_content
#' @exportS3Method get_content cohere_chat
get_content.cohere_chat <- function(response) {
  response$message$content[[1]]$text
}
