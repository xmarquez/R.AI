#' Extract Message Objects from a Chat Response
#'
#' A generic function to retrieve the **"message"** portion from various LLM
#' (Large Language Model) chat response objects (OpenAI, Cerebras, Claude, etc.).
#' Depending on the backend, the response may contain one or multiple "choices"
#' or "candidates." For most backends, this function extracts only the *first*
#' choice by default (except where otherwise noted).
#'
#' @param response A chat response object from a function like
#'   \code{\link{chat.openai_list}} or similar. Must be an object with an S3
#'   class such as `"openai_chat"`, `"cerebras_chat"`, `"claude_chat"`, etc.
#'
#' @return A list (or list-of-lists) representing the "message" component. For
#'   backends that produce a single choice, this is typically a single list with
#'   S3 class \code{c("{backend}_list", "list")}. For backends that produce
#'   multiple choices, you may get a list of lists.
#'
#' @section Behavior by Backend:
#' \describe{
#'   \item{OpenAI (`openai_chat`)}{Extracts the \code{message} from the *first*
#'     element in \code{response$choices}, wraps it in a one-element list with
#'     class \code{c("openai_list", "list")}.}
#'
#'   \item{Cerebras (`cerebras_chat`)}{Similar to OpenAI, extracts the
#'     \code{message} from the first choice as a one-element list with class
#'     \code{c("cerebras_list", "list")}.}
#'
#'   \item{Claude (`claude_chat`)}{Returns \code{response$content} *as-is*,
#'     assigning class \code{c("claude_list", "list")}. This may contain one or
#'     more message-like items.}
#'
#'   \item{Groq (`groq_chat`)}{Maps over all \code{choices}, extracting the
#'     \code{message} from each, creating a list of \code{"groq_list"} objects.
#'     So if there are multiple choices, you get multiple messages.}
#'
#'   \item{Llama.CPP (`llama_cpp_chat`)}{Extracts the \code{message} from
#'     \code{response$choices[[1]]} and returns it as a single-element list
#'     (\code{c("llama_cpp_list", "list")}).}
#'
#'   \item{Mistral (`mistral_chat`)}{Extracts the first choice’s message,
#'     ignoring null/empty fields, returning a single-element list with class
#'     \code{"mistral_list"}.}
#'
#'   \item{Gemini (`gemini_chat`)}{Extracts the \code{content} from
#'     \code{response$candidates[[1]]}, returning a list with class
#'     \code{"gemini_list"}. For advanced usage with multiple candidates,
#'     you may need to modify this code or manually parse \code{response$candidates}
#'     yourself.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example for an OpenAI response (first choice only):
#' resp_openai <- chat.openai_list(messages, model="gpt-3.5-turbo")
#' msg_oa <- get_message(resp_openai)
#' str(msg_oa)  # -> List of length 1, class c("openai_list", "list")
#'
#' # Example for Groq which may have multiple choices:
#' resp_groq <- chat.groq_list(messages, model="groq-model")
#' msgs_groq <- get_message(resp_groq)
#' str(msgs_groq)  # -> List of "groq_list" objects, one per choice
#'
#' # Example for Gemini which fetches the first candidate's content:
#' resp_gemini <- chat.gemini_list(messages, model="gemini-v1")
#' msg_gem <- get_message(resp_gemini)
#' str(msg_gem) # -> c("gemini_list", "list")
#' }
#'
#' @export
get_message <- function(response) {
  UseMethod("get_message")
}

#' @rdname get_message
#' @exportS3Method get_message openai_chat
get_message.openai_chat <- function(response) {
  res <- response$choices[[1]] |>
    purrr::pluck("message") |>
    list() |>
    structure(class = c("openai_list", "list"))
  res
}

#' @rdname get_message
#' @exportS3Method get_message cerebras_chat
get_message.cerebras_chat <- function(response) {
  res <- response$choices[[1]] |>
    purrr::pluck("message") |>
    list() |>
    structure(class = c("cerebras_list", "list"))
  res
}

#' @rdname get_message
#' @exportS3Method get_message claude_chat
get_message.claude_chat <- function(response) {
  # Directly convert response$content into class c("claude_list", "list")
  response$content |>
    structure(class = c("claude_list", "list"))
}

#' @rdname get_message
#' @exportS3Method get_message groq_chat
get_message.groq_chat <- function(response) {
  # Each choice becomes its own item in the returned list
  response$choices |>
    purrr::map(\(x) {
      msg <- purrr::pluck(x, "message")
      structure(msg, class = c("groq_list", class(msg)))
    })
}

#' @rdname get_message
#' @exportS3Method get_message llama_cpp_chat
get_message.llama_cpp_chat <- function(response) {
  res <- response$choices[[1]] |>
    purrr::pluck("message") |>
    list() |>
    structure(class = c("llama_cpp_list", "list"))
  res
}

#' @rdname get_message
#' @exportS3Method get_message mistral_chat
get_message.mistral_chat <- function(response) {
  res <- response$choices[[1]]$message |>
    purrr::compact() |>
    list() |>
    structure(class = c("mistral_list", "list"))
  res
}

#' @rdname get_message
#' @exportS3Method get_message gemini_chat
get_message.gemini_chat <- function(response) {
  # Pull from the first candidate’s content
  # If you want multiple candidates, you would map over them
  response$candidates[[1]]$content |>
    list() |>
    structure(class = c("gemini_list", "list"))
}

#' @rdname get_message
#' @exportS3Method get_message cohere_chat
get_message.cohere_chat <- function(response) {
  # Pull from the first candidate’s content
  # If you want multiple candidates, you would map over them
  response$message |>
    structure(class = c("cohere_list", "list"))
}

#' @rdname get_message
#' @exportS3Method get_message deepseek_chat
get_message.deepseek_chat <- function(response) {
  # If 'choices' is empty or missing, return an empty list
  if (is.null(response$choices) || length(response$choices) == 0) {
    return(
      structure(list(), class = c("deepseek_list", "list"))
    )
  }

  # Extract the first choice's "message"
  msg <- response$choices[[1]]$message

  # Wrap in a single-element list for consistency with other backends
  list(msg) |>
    structure(class = c("deepseek_list", "list"))
}

# ----------------------------
# get_message for QWEN
# ----------------------------
#' @rdname get_message
#' @exportS3Method get_message qwen_chat
get_message.qwen_chat <- function(response) {
  # By default, extract the *first* choice's message, wrapping it in a list
  if (is.null(response$choices) || length(response$choices) == 0) {
    return(structure(list(), class = c("qwen_list", "list")))
  }
  first_msg <- response$choices[[1]]$message
  structure(list(first_msg), class = c("qwen_list", "list"))
}
