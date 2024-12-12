#' Create, List, Retrieve, Update, Poll, or Cancel Runs in OpenAI
#'
#' These functions interact with the OpenAI [Runs
#' API](https://platform.openai.com/docs/api-reference/runs). See the
#' documentation there for more details.
#'
#' @param thread_id Character. Required. The ID of the thread the run belongs
#'   to.
#' @param run_id Character. Optional. The ID of the run to retrieve, update, or
#'   cancel.
#' @param assistant_id Character. Required for run creation. The ID of the
#'   assistant executing the run.
#' @param model Character or NULL. Optional. The model ID for the run.
#' @param instructions Character or NULL. Optional. Instructions for the
#'   assistant's behavior during the run.
#' @param additional_instructions Character or NULL. Optional. Additional
#'   instructions for the assistant.
#' @param additional_messages List or NULL. Optional. Messages to prepend to the
#'   thread.
#' @param tools List or NULL. Optional. Tools available for the run.
#' @param metadata List or NULL. Optional. Metadata for the run.
#' @param temperature Numeric or NULL. Optional. Sampling temperature (default:
#'   1).
#' @param top_p Numeric or NULL. Optional. Nucleus sampling value (default: 1).
#' @param stream Logical or NULL. Optional. Enables streaming during the run.
#' @param max_prompt_tokens Integer or NULL. Optional. Maximum prompt tokens.
#' @param max_completion_tokens Integer or NULL. Optional. Maximum completion
#'   tokens.
#' @param truncation_strategy List or NULL. Optional. Strategy for truncating
#'   thread context.
#' @param tool_choice List or Character or NULL. Optional. Controls tool usage.
#' @param parallel_tool_calls Logical. Optional. Defaults to TRUE. Enables
#'   parallel tool calls.
#' @param response_format List or Character or NULL. Optional. Specifies the
#'   output format.
#' @param limit Integer. Optional. A limit on the number of objects to return.
#'   Defaults to 20, with a maximum of 100.
#' @param order Character. Optional. Sort order by the `created_at` timestamp.
#'   Defaults to `"desc"` for descending order. Use `"asc"` for ascending order.
#' @param after Character. Optional. A cursor for pagination to fetch results
#'   after this ID.
#' @param before Character. Optional. A cursor for pagination to fetch results
#'   before this ID.
#' @param tool_resources A set of resources that are used by the assistant's
#'   tools. The resources are specific to the type of tool. For example, the
#'   `code_interpreter` tool requires a list of file IDs, while the
#'   `file_search` tool requires a list of vector store IDs. See
#'   [https://platform.openai.com/docs/api-reference/assistants/createAssistant](https://platform.openai.com/docs/api-reference/assistants/createAssistant)
#'   for details.
#' @param thread Optional. A thread object created with
#'   [openai_create_thread()]. Only used in `openai_create_thread_and_run()`
#' @param timeout How long to wait before polling the API for the run status
#'   again. Default is 5 seconds.
#' @param quiet Whether to print informative messages when polling the status of
#'   a run.
#' @return A list containing details of the created, retrieved, updated, or
#'   cancelled run, depending on the function used.
#'
#'   For `openai_create_run` or `openai_create_thread_and_run`, the created run
#'   object.
#'
#'   For `openai_list_runs`, a list of run objects in the thread.
#'
#'   For `openai_get_run` or `openai_poll_run`, the details of a specific run.
#'
#'   For `openai_cancel_run`, the updated run object with status set to
#'   "cancelling" or similar.
#'
#' @rdname openai-run
#' @family run
#' @family openai
#' @export
openai_create_run <- function(
    thread_id, assistant_id, model = NULL, instructions = NULL, additional_instructions = NULL,
    additional_messages = NULL, tools = NULL, metadata = NULL, temperature = 1, top_p = 1, stream = NULL,
    max_prompt_tokens = NULL, max_completion_tokens = NULL, truncation_strategy = NULL, tool_choice = NULL,
    parallel_tool_calls = TRUE, response_format = NULL
) {
  body <- list(
    assistant_id = assistant_id,
    model = model,
    instructions = instructions,
    additional_instructions = additional_instructions,
    additional_messages = additional_messages,
    tools = tools,
    metadata = metadata,
    temperature = temperature,
    top_p = top_p,
    stream = stream,
    max_prompt_tokens = max_prompt_tokens,
    max_completion_tokens = max_completion_tokens,
    truncation_strategy = truncation_strategy,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    response_format = response_format
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs"),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-run
#' @export
openai_list_runs <- function(thread_id, limit = 20, order = "desc", after = NULL, before = NULL) {
  query_params <- list(
    limit = limit,
    order = order,
    after = after,
    before = before
  )
  query_params <- Filter(Negate(is.null), query_params)

  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs"),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    query = query_params
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-run
#' @export
openai_get_run <- function(thread_id, run_id) {
  response <- httr::GET(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    )
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}

#' @rdname openai-run
#' @export
openai_cancel_run <- function(thread_id, run_id) {
  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/cancel"),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    )
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}


#' Submit Tool Outputs to a Run in OpenAI
#'
#' Submit the outputs from tool calls for a specific run when the run requires
#' action.
#'
#' @param thread_id Character. Required. The ID of the thread to which the run
#'   belongs.
#' @param run_id Character. Required. The ID of the run that requires tool
#'   output submission.
#' @param tool_outputs List. Required. A list of tool outputs to submit. Each
#'   item should include `tool_call_id` and `output`.
#' @param stream Logical or NULL. Optional. If TRUE, streams events during the
#'   submission process.
#' @return A list containing the updated run object.
#' @family run
#' @family openai
#' @family assistants
#' @export
openai_submit_tool_outputs <- function(thread_id, run_id, tool_outputs, stream = NULL) {
  body <- list(
    tool_outputs = tool_outputs,
    stream = stream
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/submit_tool_outputs"),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}


#' @rdname openai-run
#' @export
openai_create_thread_and_run <- function(assistant_id,
                                         thread = NULL,
                                         model = NULL,
                                         instructions = NULL,
                                         tools = NULL,
                                         tool_resources = NULL,
                                         metadata = NULL, temperature = 1,
                                         top_p = 1,
                                         stream = NULL,
                                         max_prompt_tokens = NULL,
                                         max_completion_tokens = NULL,
                                         truncation_strategy = NULL,
                                         tool_choice = NULL,
                                         parallel_tool_calls = TRUE,
                                         response_format = NULL
                                         ) {
  body <- list(
    assistant_id = assistant_id,
    thread = thread,
    model = model,
    instructions = instructions,
    tools = tools,
    tool_resources = tool_resources,
    metadata = metadata,
    temperature = temperature,
    top_p = top_p,
    stream = stream,
    max_prompt_tokens = max_prompt_tokens,
    max_completion_tokens = max_completion_tokens,
    truncation_strategy = truncation_strategy,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    response_format = response_format
  )
  body <- Filter(Negate(is.null), body)  # Remove NULL values

  response <- httr::POST(
    url = "https://api.openai.com/v1/threads/runs",
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json",
      `OpenAI-Beta` = "assistants=v2"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::http_error(response)) {
    cli::cli_abort("{httr::http_status(response)$message}. {httr::content(response)$error$message}")
  }

  httr::content(response)
}


#' @rdname openai-run
#' @export
openai_poll_run <- function(thread_id, run_id, timeout = 5, quiet = FALSE) {
  while (TRUE) {
    run_status <- openai_get_run(thread_id, run_id)
    if (run_status$status %in% c("completed")) {
      if(!quiet) {
        cli::cli_alert_success("Run has completed.")
      }
      return(run_status)
    }
    if (run_status$status %in% c("requires_action")) {
      if(!quiet) {
        cli::cli_alert_info("Run requires action.")
      }
      return(run_status)
    }
    if (run_status$status %in% c("cancelling", "cancelled", "expired", "failed", "incomplete")) {
      if(!quiet) {
        cli::cli_alert_warning("Run is {run_status$status}.")
      }
      return(run_status)
    }
    if (run_status$status %in% c("in_progress", "queued")) {
      if(!quiet) {
        cli::cli_alert_warning("Run is still in progress.")
      }
    }
    Sys.sleep(timeout)
  }
}

