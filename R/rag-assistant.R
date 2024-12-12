#' Create a Retrieval-Augmented Generation (RAG) Assistant
#'
#' Creates an OpenAI assistant using the RAG pattern with an associated vector
#' store for file search. This function supports creating assistants from
#' various types of inputs, including files from a [Zotero](https://zotero.org/)
#' collection.
#'
#' If working with Zotero collections, consider using the helper function
#' [zotero_collection_attachments()] to extract PDFs from your Zotero database.
#'
#' @param files A list or object containing file paths or file metadata. For
#'   Zotero collections, use a `zotero_collection` object.
#' @param name Character. Optional. The name of the assistant. Defaults to the
#'   name(s) of the provided file collection if not supplied.
#' @param description Character. Optional. A description of the assistant.
#'   Defaults to a generic description based on the provided name.
#' @param instructions Character. Optional. Instructions for the assistant's
#'   behavior. Defaults to standard RAG instructions provided in the package,
#'   which you can access by calling
#'   `system.file("extdata/rag_standard_instructions.md", package = "R.AI")`.
#' @param model Character. Optional. The ID of the OpenAI model to use. Defaults
#'   to `gpt-4o`.
#' @param temperature Numeric. Optional. Sampling temperature for assistant
#'   responses. Defaults to `0.2`.
#' @param expires_after Integer. Optional. Number of days after which the vector
#'   store will expire. Defaults to `7`.
#' @param convert_to_text Logical. Optional. Whether to convert files to text
#'   before uploading to OpenaAI. Defaults to `FALSE`. Use `TRUE` to minimize
#'   the size of uploaded files and to convert non-standard file types to text
#'   readable by OpenAI (e.g., if you are uploading your whole R package
#'   codebase). This requires the
#'   [rtika](https://cran.r-project.org/package=rtika) package.
#' @param quiet Logical. Optional. If `TRUE`, suppresses CLI output. Defaults to
#'   `FALSE`.
#' @param ... Additional arguments passed to methods.
#' @return An `openai_assistant` object representing the created assistant.
#' @examples
#' \dontrun{
#' # Create an assistant from a Zotero collection
#' library(dplyr)
#' zotero_files <- zotero_collection_attachments("My Research Collection")
#' assistant <- rag_assistant(
#'   files = zotero_files,
#'   name = "Research Assistant",
#'   temperature = 0.3
#' )
#' }
#' @export
rag_assistant <- function(files,
                          name,
                          description,
                          instructions,
                          model,
                          temperature,
                          expires_after,
                          convert_to_text,
                          quiet,
                          ...) {
  UseMethod("rag_assistant")
}

#' @export
rag_assistant.character <- function(files,
                                    name,
                                    description,
                                    instructions,
                                    model = "gpt-4o",
                                    temperature = 0.2,
                                    expires_after = 7,
                                    convert_to_text = FALSE,
                                    quiet = FALSE,
                                    ...) {

  checkmate::check_int(expires_after, lower = 1)
  expires_after <- list(anchor = "last_active_at", days = expires_after)

  if(missing(name)) {
    name <- paste(unique(files$collectionName), collapse = ", ")
  }
  if(missing(description)) {
    description <- sprintf("An assistant that helps the user to understand a collection of documents about %s.", name)
  }
  if(missing(instructions)) {
    rag_standard_instructions <- system.file("extdata/rag_standard_instructions.md", package = "R.AI") |>
      readLines() |>
      paste(collapse = "")
    instructions <- glue::glue(rag_standard_instructions)
  }

  if(!quiet) {
    cli::cli_alert_info("Creating assistant via the OpenAI assistants API with model {model}...")
  }

  assistant <-  openai_create_assistant(
    name = name,
    description = description,
    instructions,
    model = model,
    tools = list(type = "file_search"),
    temperature = temperature
  )
  if(!quiet) {
    cli::cli_alert_info("Assistant successfully created.")
    cli::cli_alert_info("Creating new vector store for use with assistant '{name}'...")
  }

  vector_store <- openai_create_vector_store(name = glue::glue("Knowledge base for '{name}'"),
                                             expires_after = expires_after)

  if(!quiet) {
    cli::cli_alert_info("Vector store successfully created.")
    cli::cli_alert_info("Uploading files to OpenAI for use with the vector store...")
  }

  uploaded_files <- upload_file_helper(files,
                                       convert_to_text = convert_to_text,
                                       quiet = quiet)

  if(!quiet) {
    cli::cli_alert_info("Files successfully uploaded.")
    cli::cli_alert_info("Adding files to the vector store...")
    cli::cli_alert_danger("Note you will be charged for usage beyond 2GB.")
  }

  uploaded_ids <- uploaded_files |>
    purrr::map_chr("id")

  file_batch <- openai_create_vector_store_file_batch(
    vector_store$id,
    file_ids = uploaded_ids
  )

  assistant <- openai_update_assistant(assistant$id,
                                       tool_resources = list(file_search = list(vector_store_ids = c(vector_store$id))))

  class(assistant) <- c("openai_assistant", class(assistant))
  assistant

}

#' @export
rag_assistant.zotero_collection <- function(files,
                                            name,
                                            description,
                                            instructions,
                                            model = "gpt-4o",
                                            temperature = 0.2,
                                            expires_after = 7,
                                            convert_to_text = FALSE,
                                            quiet = FALSE,
                                            ...) {

  files <- files$path
  rag_assistant.character(files, name, description,
                          instructions, model = model,
                          temperature = temperature,
                          expires_after = expires_after,
                          convert_to_text = convert_to_text,
                          quiet = quiet)

}

#' Interact with an OpenAI Assistant
#'
#' Sends a query or conversation to a previously created assistant and retrieves
#' its response, optionally including Retrieval-Augmented Generation (RAG)
#' search results.
#'
#' @param messages Character or list. Required. A single message string or a
#'   list of messages to send to the assistant. If a list, each message should
#'   be a list with `role` (e.g., `"user"`) and `content`; you can create these
#'   messages with [prompt_list()].
#' @param assistant An `openai_assistant` object. Required. The assistant to
#'   interact with, typically created by [rag_assistant()].
#' @param thread Character or list. Optional. The ID of an existing thread, or a
#'   prior `assistant_response` object from which a thread id can be extracted.
#'   If missing, a new thread is created.
#' @param instructions Character. Optional. Instructions (system prompt) for the
#'   assistant for this interaction. Defaults to the assistant's standard
#'   instructions.
#' @param timeout Numeric. Optional. Time in seconds to wait between polling
#'   requests for run completion. Defaults to `5`.
#' @param include_rag_results Logical. Optional. If `TRUE`, includes results
#'   from RAG tool calls in the output. Defaults to `TRUE`.
#' @param quiet Logical. Optional. If `TRUE`, suppresses CLI output. Defaults to
#'   `FALSE`.
#' @return A list with the following elements:
#'   - `response`: Formatted assistant response.
#'   - `rag_results`: RAG search results, if included.
#'   - `thread_id`: ID of the thread used for the interaction. You can extract further details from the thread by using [openai_get_thread()].
#'   - `run_id`: ID of the run for this interaction. You can extract further details from the thread by using [openai_get_run()].
#' @examples
#' \dontrun{
#' # Send a query to the assistant
#' response <- ask_assistant(
#'   messages = "What was the significance of Wang Dongxing during the Cultural Revolution?",
#'   assistant = my_assistant
#' )
#' cat(response$response)
#' }
#' @export
ask_assistant <- function(messages, assistant, thread, instructions, timeout = 5, include_rag_results = TRUE, quiet = FALSE) {

  checkmate::assert_class(assistant, "openai_assistant")

  if(missing(thread)) {
    thread <- openai_create_thread()
  }
  if(is.character(thread)) {
    old_thread <- thread
    thread <- list()
    thread$id <- old_thread
  }
  if(inherits(thread, "assistant_response")) {
    old_thread <- thread
    thread <- list()
    thread$id <- old_thread$thread_id
  }
  if(is.character(messages)) {
    messages <- list(list(role = "user", content = messages))
  }
  if(missing(instructions)) {
    instructions <- assistant$instructions
  }

  messages_sent <- messages |>
    purrr::map(\(x) openai_create_message(
      thread_id = thread$id,
      role = x[["role"]],
      content = x[["content"]]))

  run <- openai_create_run(
    thread_id = thread$id,
    assistant_id = assistant$id,
    instructions = instructions
    )

  run_result <- openai_poll_run(thread_id = thread$id,
                                run_id = run$id,
                                timeout = timeout)

  if(!quiet) {
    cli::cli_alert_info("Now retrieving and processing response.")
  }
  messages <- openai_list_messages(thread$id)

  messages_formatted <- process_message(messages$data[[1]])

  if(include_rag_results) {
    if(!quiet) {
      cli::cli_alert_info("Now retrieving and processing RAG search results.")
    }
    run_steps <- openai_list_run_steps(thread$id, run$id, include = "step_details.tool_calls[*].file_search.results[*].content")

    rag_results <- run_steps$data |>
      purrr::map(\(x) x$step_details$tool_calls |>
                   purrr::map("file_search") |>
                   purrr::map("results") |>
                   purrr::flatten() |>
                   purrr::map(\(x) format_rag_results(x)) |>
                   purrr::list_rbind())

    assistant_response <- list(response = messages_formatted,
                               rag_results = rag_results,
                               thread_id = thread$id,
                               run_id = run_result$id)
    class(assistant_response) <- c("assistant_response", class(assistant_response))
    return(assistant_response)
  }
  assistant_response <- list(response = messages_formatted,
                             thread_id = thread$id,
                             run_id = run_result$id)
  class(assistant_response) <- c("assistant_response", class(assistant_response))
  return(assistant_response)

}

upload_file_helper <- function(paths, convert_to_text, quiet) {

  if(convert_to_text) {
    if(!rlang::is_installed("rtika")) {
      cli::cli_abort("The {.var convert_to_text} option for converting to text requires {.pkg rtika} package.")
    }
    tmpdir <- tempdir()
    paths <- as.character(paths)
    file_sizes <- fs::file_size(paths) |>
      sum()
    if(!quiet) {
      cli::cli_alert_danger("You are about to convert to text {length(paths)} file{?s} ({file_sizes}).")
    }
    res <- rtika::tika_text(paths, threads = 10, output_dir = tmpdir)
    paths <- fs::dir_ls(tmpdir, recurse = TRUE, glob = "*.txt")
  }

  file_sizes <- fs::file_size(paths) |>
    sum()

  if(!quiet) {
    cli::cli_alert_danger("You are about to upload {length(paths)} file{?s} to OpenAI ({file_sizes}).")
  }

  uploads <- list(length(paths))
  for(file in paths) {
    if(!quiet) {
      cli::cli_alert_info("Now uploading {file}, number {which(file == paths)} of {length(paths)}.")
    }
    uploaded <- openai_upload_file(file, purpose = "assistants")
    uploads[[which(file == paths)]] <- uploaded
  }

  uploads
}

format_rag_results <- function(x) {
  file_id <- x$file_id
  file_name <- x$file_name
  score <- x$score
  content <- purrr::map(x$content, "text")
  tibble::tibble(file_id = file_id,
         file_name = file_name,
         score = score,
         content = content) |>
    tidyr::unnest(content) |>
    dplyr::mutate(content = stringr::str_squish(content))

}

process_message <- function(message) {
  content <- message$content[[1]]$text$value
  annotations <- message$content[[1]]$text$annotations
  citations <- character()  # Store citation footnotes

  # Process annotations
  for (i in seq_along(annotations)) {
    annotation <- annotations[[i]]
    footnote_ref <- sprintf("[@%d]", i)
    if (!is.null(annotation$file_citation)) {
      file_id <- annotation$file_citation$file_id
      cited_file <- openai_file_details(file_id)
      citations <- c(citations, cited_file$filename)
    }

    # Replace the annotated text with the footnote reference
    content <- sub(annotation$text, footnote_ref, content, fixed = TRUE)

  }

  # Append footnotes to the message content
  if (length(citations) > 0) {
    content <- paste(content, "\n## References", "\n", paste(unique(citations), collapse = "\n"))
  }

  # Return updated content
  list(updated_content = content, citations = citations)
}

# Danger! These functions are just for testing and development purposes.
cleanup <- function() {
  assistants <- openai_list_assistants()
  assistants$data |>
    purrr::map_chr("id") |>
    purrr::map(openai_delete_assistant)

  vector_stores <- openai_list_vector_stores()
  vector_stores <- vector_stores$data |>
    purrr::map_chr("id")

  vector_stores |>
    purrr::map(openai_delete_vector_store)
}

delete_all_files <- function() {
  files <- openai_list_files()
  ids <- files$data |>
    purrr::map_chr("id")
  purposes <- files$data |>
    purrr::map_chr("purpose")
  ids <- ids[which(purposes == "assistants")]
  ids |>
    purrr::map(openai_delete_file)
}


