#' Create a Message Batch Using the Anthropic API
#'
#' This function sends a batch of message creation requests to the Anthropic API
#' using the [Message Batches
#' endpoint](https://docs.anthropic.com/en/api/creating-message-batches). It
#' allows you to process multiple requests at once, useful for scenarios
#' requiring bulk message completions.
#'
#' @param prompts A list of prompts created by [build_prompts_from_files()].
#' @param model The model to use to process the batch. Currently supports only
#'   Claude 3 Haiku, Claude 3 Opus, and Claude 3.5 Sonnet. Defaults to
#'   "claude-3-haiku-20240307" (i.e., `get_default_model("claude", type =
#'   "cheapest")`.
#' @param max_tokens The maximum number of output tokens for each item in the
#'   batch.
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A list containing the response details, including batch ID,
#'   processing status, and the URL for retrieving results.
#' @export
claude_create_batch <- function(prompts,
                                model = get_default_model("claude"),
                                max_tokens = 300,
                                max_retries = 3,
                                pause_cap = 1200, quiet = FALSE) {

  checkmate::assert_class(prompts, "claude")
  requests <- claude_format_prompts_for_batch(prompts,
                                              model = model,
                                              max_tokens = max_tokens)

  # Construct the JSON body for the batch request
  body <- jsonlite::toJSON(
    list(
      requests = requests
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Make the API call using httr::RETRY to handle retries
  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.anthropic.com/v1/messages/batches",
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "message-batches-2024-09-24",
      "content-type" = "application/json"
    ),
    body = body,
    encode = "json",
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check if the response contains an error
  httr::stop_for_status(res)
  response <- httr::content(res)

  # Return the response details
  response
}


#' List Message Batches Using the Anthropic API
#'
#' This function retrieves all message batches within a workspace that are
#' currently in process. It uses the [List Message Batches
#' endpoint](https://docs.anthropic.com/en/api/message-batches-beta). The most
#' recently created batches are returned first.
#'
#' @param limit An integer specifying the maximum number of batches to return.
#'   Defaults to 20, and ranges from 1 to 100.
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A list containing information about any message batches,
#'   including batch IDs, processing status, and URLs for retrieving results.
#' @export
claude_list_batches <- function(limit = 20, max_retries = 3, pause_cap = 1200, quiet = FALSE) {

  # Ensure limit is within the allowed range
  limit <- min(max(limit, 1), 100)

  # Make the API call using httr::RETRY to handle retries
  res <- httr::RETRY(
    verb = "GET",
    url = "https://api.anthropic.com/v1/messages/batches",
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "message-batches-2024-09-24"
    ),
    query = list(limit = limit),
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check if the response contains an error
  httr::stop_for_status(res)
  response <- httr::content(res)

  response
}

#' Retrieve the status of a specific Message Batch Using the Anthropic API
#'
#' This function retrieves details for a specific message batch using the
#' [Message Batch
#' endpoint](https://docs.anthropic.com/en/api/message-batches-beta). It can be
#' used to poll for batch completion or to access the batch metadata.
#'
#' @param batch_response The list returned by [claude_create_batch()], containing the string
#'   representing the unique ID of the message batch to retrieve. (It's also
#'   possible to pass the specific object from [claude_list_batches()], e.g.,
#'   `claude_list_batches()$data[[1]]`).
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A list containing information about the specified message batch,
#'   including processing status and result URLs.
#' @export
claude_check_batch_status <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE) {

  message_batch_id <- batch_response$id

  # Construct the URL for the specific message batch
  url <- paste0("https://api.anthropic.com/v1/messages/batches/", message_batch_id)

  # Make the API call using httr::RETRY to handle retries
  res <- httr::RETRY(
    verb = "GET",
    url = url,
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "message-batches-2024-09-24"
    ),
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check if the response contains an error
  httr::stop_for_status(res)
  response <- httr::content(res)

  # Return the response details
  response
}

#' Retrieve Message Batch Results Using the Anthropic API
#'
#' This function retrieves the results for a specific message batch using the
#' [Message Batch
#' endpoint](https://docs.anthropic.com/en/api/message-batches-beta). The
#' results are streamed as a .jsonl file.
#'
#' @param batch_response The list returned by [claude_create_batch()],
#'   containing the string representing the unique ID of the message batch to
#'   retrieve. (It's also possible to pass the specific object from
#'   [claude_list_batches()], e.g., `claude_list_batches()$data[[1]]`).
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#' @param tidy A logical value indicating whether to attempt to tidy the
#'   resulting json into a tidy [tibble]. Default is `TRUE`; `FALSE` is useful
#'   if you want to do the tidying separately or prefer the raw json.
#'
#' @return A character vector containing each line of the .jsonl result file.
#' @export
claude_download_batch_results <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE, tidy = TRUE) {

  if(batch_response$processing_status == "ended") {
    results_url <- batch_response$results_url
  } else {
    message <- glue::glue("Processing status is `{batch_response$processing_status}`. No results to download.")
    stop(message)
  }

  # Make the API call using httr::RETRY to handle retries
  res <- httr::RETRY(
    verb = "GET",
    url = results_url,
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "message-batches-2024-09-24"
    ),
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check if the response contains an error
  httr::stop_for_status(res)
  response_content <- httr::content(res, as = "text", encoding = "UTF8")

  if(tidy) {
    # Tidy response
    response_content <- jsonlite::stream_in(textConnection(response_content), verbose = !quiet) |>
      tibble::as_tibble() |>
      tidyr::unnest("result") |>
      dplyr::rename(request_status = "type") |>
      tidyr::unnest("message") |>
      dplyr::rename(content_type = "type",
                    anthropic_id = "id",
                    id = "custom_id") |>
      tidyr::unnest("content") |>
      dplyr::rename(response = "text") |>
      tidyr::unnest("usage")
  }

  # Return the response
  response_content
}

#' Poll Message Batch Status and Retrieve Results
#'
#' This function continuously polls the status of a message batch until the
#' processing is complete. If the status is "ended", it downloads the results.
#' If the status is "canceling", it stops and provides an informative message.
#' If the status is "in_progress", it waits for the specified timeout before
#' polling again.
#'
#' @param batch_response A list returned by [claude_create_batch()] or
#'   [claude_check_batch_status()] containing batch details. (It's also
#'   possible to pass the specific object from [claude_list_batches()], e.g.,
#'   `claude_list_batches()$data[[1]]`).
#' @param timeout A numeric value representing the time (in seconds) to wait
#'   between polling attempts. Defaults to 3600 seconds (1 hour).
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param tidy A logical value indicating whether to attempt to tidy the
#'   resulting json into a tidy [tibble]. Default is `TRUE`; `FALSE` is useful
#'   if you want to do the tidying separately or prefer the raw json.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A character vector containing each line of the .jsonl result file if
#'   the batch ends successfully.
#' @export
claude_poll_and_retrieve_results <- function(batch_response, timeout = 3600,
                                             max_retries = 3, pause_cap = 1200,
                                             tidy = TRUE, quiet = FALSE) {
  repeat {
    # Retrieve the current status of the message batch
    status_response <- claude_check_batch_status(batch_response,
                                                 max_retries = max_retries,
                                                 pause_cap = pause_cap, quiet = quiet)

    processing_status <- status_response$processing_status
    results_url <- status_response$results_url

    if (processing_status == "ended" && !is.null(results_url)) {
      if(!quiet) {
        message("Batch processing ended. Retrieving results...")
      }
      results <- claude_download_batch_results(status_response,
                                               max_retries = max_retries,
                                               pause_cap = pause_cap,
                                               quiet = quiet,
                                               tidy = tidy)
      return(results)
    } else if (processing_status == "canceling") {
      stop("Batch processing was canceled. No results available.")
    } else if (processing_status == "in_progress") {
      if(!quiet) {
        message(glue::glue("{lubridate::now()}: Batch still in progress. Waiting for {timeout} seconds before next poll..."))
      }
      Sys.sleep(timeout)
    } else if  (processing_status == "ended" && is.null(results_url)) {
      stop("Processing status ended but no download link available - batch was probably cancelled.")
    } else {
      stop("Unknown problem. Processing status: ", processing_status)
    }
  }
}

claude_format_prompts_for_batch <- function(prompts, model, max_tokens) {
  lapply(names(prompts), function(name) {
    list(
      custom_id = name,
      params = list(
        model = model,  # Replace with your desired model,
        max_tokens = max_tokens,
        messages = prompts[[name]]
      )
    )
  })
}

#' Cancel a Message Batch Using the Anthropic API
#'
#' This function cancels a message batch that is currently in progress using the
#' [Cancel a Message Batch
#' endpoint](https://docs.anthropic.com/en/api/message-batches-beta). The batch
#' may enter a canceling state, during which any in-progress, non-interruptible
#' requests may still complete.
#'
#' @param batch_response The list returned by [claude_create_batch()], containing the string
#'   representing the unique ID of the message batch to retrieve. (It's also
#'   possible to pass the specific object from [claude_list_batches()], e.g.,
#'   `claude_list_batches()$data[[1]]`).
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A list containing the response details, including batch ID,
#'   processing status, and counts of canceled requests.
#' @export
claude_cancel_batch <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  # Construct the URL for canceling the specific message batch

  message_batch_id <- batch_response$id
  url <- paste0("https://api.anthropic.com/v1/messages/batches/", message_batch_id, "/cancel")

  # Make the API call using httr::RETRY to handle retries
  res <- httr::RETRY(
    verb = "POST",
    url = url,
    config = httr::add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "anthropic-beta" = "message-batches-2024-09-24"
    ),
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check if the response contains an error
  httr::stop_for_status(res)
  response <- httr::content(res)

  # Return the response details
  response
}

#' Create and Poll a Claude Batch
#'
#' This function creates a message batch using the Anthropic API and then polls
#' the status until processing is complete. It simplifies the workflow of
#' creating and retrieving the results of a batch, especially for scenarios
#' where multiple message completions are required.
#'
#' @param prompts A list of prompts created by [build_prompts_from_files()].
#' @param model The model to use for processing the batch. Defaults to the
#'   result of `get_default_model("claude")`.
#' @param timeout A numeric value representing the time (in seconds) to wait
#'   between polling attempts. Defaults to 3600 seconds (1 hour).
#' @param max_tokens The maximum number of output tokens for each item in the
#'   batch. Defaults to 300.
#' @param tidy A logical value indicating whether to attempt to tidy the
#'   resulting json into a tidy [tibble]. Default is `TRUE`; `FALSE` is useful
#'   if you want to do the tidying separately or prefer the raw json.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries and polling. Defaults to `FALSE`.
#'
#' @return A character vector containing each line of the .jsonl result file if
#'   the batch completes successfully.
#' @export
claude_batch <- function(prompts, model, timeout = 3600, max_tokens = 300, tidy = TRUE, quiet = FALSE) {

  checkmate::assert_class(prompts, "claude")
  if(missing(model)) {
    model <- get_default_model("claude")
  }
  batch <- claude_create_batch(prompts, model = model, quiet = quiet)
  result <- claude_poll_and_retrieve_results(batch, timeout = timeout, tidy = tidy,
                                             quiet = quiet)
  result
}

#' Create a Message Batch Using the Anthropic API
#'
#' This function sends a batch of message creation requests to the Anthropic API
#' using the [Message Batches
#' endpoint](https://docs.anthropic.com/en/api/creating-message-batches). It
#' allows you to process multiple requests at once, useful for scenarios
#' requiring bulk message completions.
#'
#' @inheritParams claude_create_batch
#' @export
claude_batch_job <- function(prompts, model, max_tokens = 300, quiet = FALSE) {
  claude_create_batch(prompts = prompts, model = model, max_tokens = tokens, quiet = quiet)
}
