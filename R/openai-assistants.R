#' Create, list, retrieve or delete an OpenAI Assistant
#'
#' These functions interact at a low level with the [OpenAI Assistants
#' API](https://platform.openai.com/docs/api-reference/assistants). Check the
#' official documentation for details.
#'
#' @param assistant_id Character. ID of the assistant.
#' @param name Character. Name of the assistant. The maximum length is 256
#'   characters.
#' @param description Character. The description of the assistant. The maximum
#'   length is 512 characters.
#' @param instructions Character. Instructions for the assistant. The maximum
#'   length is 256,000 characters.
#' @param model Character. Model to use for the assistant (e.g., "gpt-4o"). Get
#'   available models via [list_models()].
#' @param tools List. Tools the assistant can use. A list of tools enabled on
#'   the assistant. There can be a maximum of 128 tools per assistant. Tools can
#'   be of types `code_interpreter`, `file_search`, or `function`. See
#'   [https://platform.openai.com/docs/api-reference/assistants/createAssistant](https://platform.openai.com/docs/api-reference/assistants/createAssistant)
#'   for details. Normally you would specify them as `list(type =
#'   "code_interpreter")`.
#' @param tool_resources A set of resources that are used by the assistant's
#'   tools. The resources are specific to the type of tool. For example, the
#'   `code_interpreter` tool requires a list of file IDs, while the
#'   `file_search` tool requires a list of vector store IDs. See
#'   [https://platform.openai.com/docs/api-reference/assistants/createAssistant](https://platform.openai.com/docs/api-reference/assistants/createAssistant)
#'   for details.
#' @param metadata List. Set of 16 key-value pairs that can be attached to an
#'   assistant. This can be useful for storing additional information about the
#'   assistant in a structured format. Keys can be a maximum of 64 characters
#'   long and values can be a maximum of 512 characters long.
#' @param temperature Number or NULL. Optional. Defaults to 1. What sampling
#'   temperature to use, between 0 and 2. Higher values like 0.8 will make the
#'   output more random, while lower values like 0.2 will make it more focused
#'   and deterministic.
#' @param top_p Number or NULL. Optional. Defaults to 1. An alternative to
#'   sampling with temperature, called nucleus sampling, where the model
#'   considers the results of the tokens with top_p probability mass. So 0.1
#'   means only the tokens comprising the top 10% probability mass are
#'   considered. OpenAI generally recommends altering this or temperature but
#'   not both.
#' @param response_format A character "auto" or a list. Optional Specifies the
#'   format that the model must output. Compatible with GPT-4o, GPT-4 Turbo, and
#'   all GPT-3.5 Turbo models since gpt-3.5-turbo-1106. Setting to a list that
#'   corresponds to the JSON `{ "type": "json_schema", "json_schema": {...} }`
#'   enables Structured Outputs which ensures the model will match your supplied
#'   JSON schema. Learn more in the [Structured
#'   Outputs](https://platform.openai.com/docs/guides/structured-outputs) guide.
#'   Setting to `{ "type": "json_object" }` enables JSON mode, which ensures the
#'   message the model generates is valid JSON. Important: when using JSON mode,
#'   you must also instruct the model to produce JSON yourself via a system or
#'   user message. Without this, the model may generate an unending stream of
#'   whitespace until the generation reaches the token limit, resulting in a
#'   long-running and seemingly "stuck" request. Also note that the message
#'   content may be partially cut off if finish_reason="length", which indicates
#'   the generation exceeded max_tokens or the conversation exceeded the max
#'   context length.
#' @param limit Integer. Maximum number of results to return (default: 20).
#' @param order Character. Sort order (`asc` or `desc`) based on creation
#'   timestamp.
#' @param after Character. Cursor for pagination to fetch results after this ID.
#' @param before Character. Cursor for pagination to fetch results before this
#'   ID.
#'
#' @return For `openai_create_assistant`, `openai_get_assistant`, or
#'   `openai_update_assistant`, a list containing the following elements:
#' * `id`: string. The identifier, which can be referenced in API endpoints.
#' * `object`: string. The object type, which is always "assistant".
#' * `created_at`: integer. The Unix timestamp (in seconds) for when the assistant was created.
#' * `name`: string or `NULL`. The name of the assistant. The maximum length is 256 characters.
#' * `description`: string or `NULL`. The description of the assistant. The maximum length is 512 characters.
#' * `model`: string. ID of the model to use. Use the [list_models()] to see all available models or refer to the [Model Overview](https://platform.openai.com/docs/models) for descriptions.
#' * `instructions`: string or `NULL`. The system instructions that the assistant uses. The maximum length is 256,000 characters.
#' * `tools`: array. A list of tools enabled on the assistant. Maximum of 128 tools per assistant. Tools can be of types `"code_interpreter"`, `"file_search"`, or `"function"`.
#' * `tool_resources`: object or `NULL`. A set of resources used by the assistant's tools. The resources are specific to the type of tool. For example, the `"code_interpreter"` tool requires a list of file IDs, while the `"file_search"` tool requires a list of vector store IDs.
#' * `metadata`: map. A set of up to 16 key-value pairs that can be attached to the object. Useful for storing additional information in a structured format. Keys can be a maximum of 64 characters long, and values can be a maximum of 512 characters long.
#' * `temperature`: number or `NULL`. Sampling temperature to use, between 0 and 2. Defaults to 0.2. Higher values like 0.8 make output more random, while lower values like 0.2 make it more focused and deterministic.
#' * `top_p`: number or `NULL`. An alternative to sampling with temperature, called nucleus sampling. `top_p` specifies the cumulative probability mass of the tokens to consider. For example, `top_p = 0.1` considers only the tokens comprising the top 10% probability mass. Altering this or `temperature`, but not both, is recommended.
#' * `response_format`: `"auto"` or object. Specifies the format the model must output. Compatible with GPT-4o, GPT-4 Turbo, and all GPT-3.5 Turbo models since `"gpt-3.5-turbo-1106"`. Supported formats:
#'   - `type: "json_schema"`: Ensures the model matches a supplied JSON schema. Learn more in the [Structured Outputs guide](https://platform.openai.com/docs/guides/structured-outputs).
#'   - `type: "json_object"`: Ensures the message the model generates is valid JSON.
#'
#' For `openai_list_assistants`, a list of assistant objects.
#'
#' For `openai_delete_assistant`, a list with the deletion status.
#'
#' @rdname openai-assistants
#' @family openai
#' @family assistants
#' @examples
#' \dontrun{
#' openai_create_assistant(
#'   name = "Math Tutor",
#'   instructions = paste("You are a personal math tutor.",
#'                        "When asked a question, write and ",
#'                        "run Python code to answer the question."),
#'   model = "gpt-4o",
#'   tools = list(type = "code_interpreter")
#'   )
#' }
#' @export
openai_create_assistant <- function(name = NULL,
                                    description = NULL,
                                    instructions = NULL,
                                    model,
                                    tools = NULL,
                                    tool_resources = NULL,
                                    metadata = NULL,
                                    temperature = 0.2,
                                    top_p = NULL,
                                    response_format = "auto") {
  body <- list(
    name = name,
    description = description,
    instructions = instructions,
    model = model,
    tools = list(tools),
    tool_resources = tool_resources,
    metadata = metadata,
    temperature = temperature,
    top_p = top_p,
    response_format = response_format
  )

  body <- Filter(Negate(is.null), body)

  response <- httr::POST(
    url = "https://api.openai.com/v1/assistants",
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` =  "assistants=v2"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-assistants
#' @export
openai_list_assistants <- function(limit = 20, order = "desc", after = NULL, before = NULL) {
  query_params <- list(limit = limit, order = order, after = after, before = before)
  query_params <- Filter(Negate(is.null), query_params)  # Remove NULL values

  response <- httr::GET(
    url = "https://api.openai.com/v1/assistants",
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
                      `OpenAI-Beta` =  "assistants=v2"),
    query = query_params
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}


#' @rdname openai-assistants
#' @export
openai_get_assistant <- function(assistant_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/assistants/", assistant_id),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
                      `OpenAI-Beta` =  "assistants=v2")
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-assistants
#' @export
openai_update_assistant <- function(assistant_id,
                                    name = NULL,
                                    description = NULL,
                                    instructions = NULL,
                                    model = NULL,
                                    tools = NULL,
                                    tool_resources = NULL,
                                    metadata = NULL,
                                    temperature = NULL,
                                    top_p = NULL,
                                    response_format = NULL) {
  body <- list(
    name = jsonlite::unbox(name),
    description = jsonlite::unbox(description),
    instructions = jsonlite::unbox(instructions),
    model = jsonlite::unbox(model),
    tools = Filter(Negate(is.null), tools),
    tool_resources = tool_resources,
    metadata = metadata,
    temperature = jsonlite::unbox(temperature),
    top_p = jsonlite::unbox(top_p),
    response_format = jsonlite::unbox(response_format)
  )

  body <- Filter(Negate(is.null), body)

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/assistants/", assistant_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` =  "assistants=v2"
    ),
    body = jsonlite::toJSON(body)
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-assistants
#' @export
openai_delete_assistant <- function(assistant_id) {
  response <- httr::DELETE(
    url = paste0("https://api.openai.com/v1/assistants/", assistant_id),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
                      `OpenAI-Beta` =  "assistants=v2")
  )

  if(httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}
