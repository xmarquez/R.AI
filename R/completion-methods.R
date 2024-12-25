#' Generate Text Completions
#'
#' A generic function to produce text completions from various backends.
#' Currently supports Llama.CPP and Ollama. By dispatching on the class of
#' `prompt` (e.g., `"llama_cpp_character"`, `"ollama_character"`), the
#' appropriate method is chosen.
#'
#' @param prompt Character string(s) serving as the prompt(s). Must be a
#'   character vector of length one or more. The S3 method for each backend
#'   may support different additional parameters.
#' @param ... Additional arguments passed to the specific backend method.
#'   For instance:
#'   \itemize{
#'     \item Llama.CPP: `n_predict`, `temperature`, `top_k`, `top_p`, `stream`
#'     \item Ollama: `model`, `suffix`, `images`, `options`, `system`, etc.
#'   }
#'
#' @return Each method returns a structure containing the completion text or a
#'   list of tokens, depending on the backend. Typically, the object has a class
#'   like `"llama_cpp_completion"` or `"ollama_completion"`.
#'
#' @examples
#' \dontrun{
#' # Example calling Llama.CPP:
#' prompt_llama <- "Tell me a story about dragons."
#' class(prompt_llama) <- c("llama_cpp_character", "character")
#' resp_llama <- completion(prompt_llama, n_predict=100, temperature=0.7)
#'
#' # Example calling Ollama:
#' prompt_ollama <- "What are the main differences between cats and dogs?"
#' class(prompt_ollama) <- c("ollama_character", "character")
#' resp_ollama <- completion(prompt_ollama, model="my-cool-model")
#' }
#'
#' @export
completion <- function(prompt, ...) {
  UseMethod("completion")
}

# -------------------------------------------------------------------
# Llama.CPP method
# -------------------------------------------------------------------
#' @rdname completion
#' @exportS3Method completion llama_cpp_character
completion.llama_cpp_character <- function(prompt,
                                           n_predict = -1,
                                           temperature = 0.8,
                                           top_k = 40,
                                           top_p = 0.95,
                                           stream = FALSE,
                                           ...) {
  checkmate::assert_string(prompt)

  body <- list(
    prompt = prompt,
    n_predict = n_predict,
    temperature = temperature,
    top_k = top_k,
    top_p = top_p,
    stream = stream,
    ...
  ) |> purrr::compact()

  res <- httr::RETRY(
    verb = "POST",
    url = "http://localhost:8080/completion",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = 5,
    pause_min = 1,
    pause_cap = 10
  )
  handle_errors(res)
  content_obj <- httr::content(res)
  structure(content_obj, class = c("llama_cpp_completion", class(content_obj)))
}

# -------------------------------------------------------------------
# Ollama method
# -------------------------------------------------------------------
#' @rdname completion
#' @exportS3Method completion ollama_character
completion.ollama_character <- function(prompt,
                                        model,
                                        suffix = NULL,
                                        images = NULL,
                                        options = NULL,
                                        system = NULL,
                                        template = NULL,
                                        stream = FALSE,
                                        raw = FALSE,
                                        keep_alive = "5m",
                                        ...) {
  body <- list(
    model = model,
    prompt = prompt,
    suffix = suffix,
    images = images,
    options = options,
    system = system,
    template = template,
    stream = stream,
    raw = raw,
    keep_alive = keep_alive
  ) |> purrr::compact()

  res <- httr::POST(
    url = "http://localhost:11434/api/generate",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    ...
  )
  handle_errors(res)
  content_obj <- httr::content(res)
  structure(content_obj, class = c("ollama_completion", class(content_obj)))
}
