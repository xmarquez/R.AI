claude_create_batch <- function(prompts,
                                model = get_default_model("claude"),
                                max_tokens = 300,
                                temperature = 0.2,
                                max_retries = 3,
                                pause_cap = 1200, quiet = FALSE) {

  checkmate::assert_class(prompts, "claude")
  requests <- claude_format_prompts_for_batch(
    prompts = prompts,
    model = model,
    max_tokens = max_tokens,
    temperature = temperature
  )

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

  class(response) <- c("batch_claude", class(response))

  # Return the response details
  response
}

claude_format_prompts_for_batch <- function(prompts, model, max_tokens, temperature) {
  lapply(names(prompts), function(name) {
    list(
      custom_id = name,
      params = list(
        model = model,
        max_tokens = max_tokens,
        temperature = temperature,
        messages = prompts[[name]]
      )
    )
  })
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

#' Check the Status of a Message Batch Using the Claude API
#'
#' This function retrieves details about a specific message batch using the
#' Claude API. It is useful for monitoring batch progress, identifying errors,
#' or retrieving the URL for completed results.
#'
#' @param batch_response A `list` returned by [claude_batch_job()] containing
#'   the batch details. Alternatively, pass an object from
#'   [claude_list_batches()], such as `claude_list_batches()$data[[1]]`.
#' @param max_retries An integer specifying the maximum number of retry attempts
#'   in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages
#'   during retries. Defaults to `FALSE`.
#'
#' @return A `list` containing detailed information about the specified message
#'   batch. The fields in the result include:
#'   - **`id`**: A unique identifier for the batch (e.g., `"msgbatch_013Zva2CMHLNnXjNJJKqJ2EF"`).
#'   - **`type`**: The type of the object (e.g., `"message_batch"`).
#'   - **`processing_status`**: The current processing status of the batch (e.g., `"in_progress"`, `"ended"`, `"canceling"`).
#'   - **`request_counts`**: A breakdown of requests in the batch:
#'       - **`processing`**: Number of requests currently being processed.
#'       - **`succeeded`**: Number of successfully completed requests.
#'       - **`errored`**: Number of requests that resulted in errors.
#'       - **`canceled`**: Number of requests that were canceled.
#'       - **`expired`**: Number of requests that expired.
#'   - **`ended_at`**: A timestamp indicating when the batch processing ended, if applicable (e.g., `"2024-08-20T18:37:24.100435Z"`).
#'   - **`created_at`**: A timestamp indicating when the batch was created (e.g., `"2024-08-20T18:37:24.100435Z"`).
#'   - **`expires_at`**: A timestamp indicating when the batch will expire, if applicable (e.g., `"2024-08-20T18:37:24.100435Z"`).
#'   - **`archived_at`**: A timestamp indicating when the batch was archived, if applicable (e.g., `"2024-08-20T18:37:24.100435Z"`).
#'   - **`cancel_initiated_at`**: A timestamp indicating when cancellation of the batch was initiated, if applicable (e.g., `"2024-08-20T18:37:24.100435Z"`).
#'   - **`results_url`**: A URL to download the results for the batch if processing is complete (e.g., `"https://api.anthropic.com/v1/messages/batches/msgbatch_013Zva2CMHLNnXjNJJKqJ2EF/results"`).
#'
#' @details This function uses the [Anthropic Message Batch
#' API](https://docs.anthropic.com/en/api/message-batches-beta) to retrieve
#' detailed status information for a batch. The `processing_status` field
#' indicates the current state of the batch and can include values such as
#' `"in_progress"`, `"ended"`, or `"canceling"`.
#'
#' The `request_counts` field provides a detailed breakdown of the status of
#' individual requests within the batch.
#'
#' @seealso
#' - [claude_batch_job()] for batch creation.
#' - [claude_download_batch_results()] for retrieving completed results.
#' - [claude_list_batches()] for listing existing batches.
#'
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

  # Set the class of the response as a batch object
  class(response) <- c("batch_claude", class(response))

  # Return the response details
  response
}

#' Download the Results of a Claude Message Batch
#'
#' This function retrieves the results of a completed message batch processed by
#' the Claude API. It supports retrying failed requests and returning results in
#' a tidy format.
#'
#' @param batch_response A `list` containing details about the batch, typically
#'   returned by [claude_batch_job()] or [claude_check_batch_status()]. The
#'   `batch_response` must include a valid `results_url`.
#' @param max_retries An integer specifying the maximum number of retry attempts
#'   in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages
#'   during retries. Defaults to `FALSE`.
#' @param tidy A logical value indicating whether to return the results in a
#'   tidy format (e.g., a `data.frame`). Defaults to `TRUE`.
#'
#' @return A `list` or `data.frame` containing the batch results:
#'   - If `tidy = TRUE`, results are returned in a structured format, typically as a `data.frame` where each row corresponds to a prompt-completion pair.
#'   - If `tidy = FALSE`, raw results are returned as received from the API.
#'
#' @details The function downloads batch results using the Claude API's [message
#' batch results
#' endpoint](https://docs.anthropic.com/en/api/message-batches-beta). The
#' returned results typically include model completions for each prompt
#' submitted in the batch.
#'
#' @seealso
#' - [claude_batch_job()] for initiating batch requests.
#' - [claude_check_batch_status()] for verifying the status of a batch before downloading results.
#' - [download_results()] for generic batch result handling.
#'
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

#' Poll and Download the Results of a Claude Message Batch
#'
#' This function repeatedly checks the status of a Claude message batch, waiting
#' for its completion before downloading the results.
#'
#' @param batch_response A `list` containing details about the batch to poll,
#'   typically returned by [claude_create_batch()] or
#'   [claude_check_batch_status()].
#' @param timeout An integer specifying the maximum time (in seconds) to wait
#'   for the batch to complete. Defaults to `3600` (1 hour).
#' @param max_retries An integer specifying the maximum number of retry attempts
#'   for status checks or download failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages
#'   during polling and downloading. Defaults to `FALSE`.
#' @param tidy A logical value indicating whether to return the results in a
#'   tidy format (e.g., a `data.frame`). Defaults to `TRUE`.
#'
#' @return A `list` or `data.frame` containing the results of the batch:
#'   - If `tidy = TRUE`, the results are returned in a structured format (e.g., a `data.frame` where each row corresponds to a prompt-completion pair).
#'   - If `tidy = FALSE`, the raw results are returned as received from the Claude API.
#'
#' @details The function repeatedly checks the status of the batch using the
#' Claude API's [batch status
#' endpoint](https://docs.anthropic.com/en/api/message-batches-beta), and
#' downloads the results once the batch is complete.
#'
#' @seealso
#' - [poll_and_download()] for the generic polling and downloading function.
#' - [claude_batch_job()] for initiating a batch request.
#' - [claude_download_batch_results()] for downloading results without polling.
#'
#' @export
claude_poll_and_download <- function(batch_response, timeout = 3600,
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


#' Cancel a Claude Message Batch
#'
#' This function cancels a batch of requests submitted to the Claude API.
#' The operation attempts to stop further processing of the batch.
#'
#' @param batch_response A `list` containing details about the batch to cancel, typically returned by
#'   [claude_batch_job()] or [claude_check_batch_status()].
#' @param max_retries An integer specifying the maximum number of retry attempts in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages during retries. Defaults to `FALSE`.
#'
#' @return A `list` containing the updated batch status after the cancellation attempt. Typical fields include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The new status of the batch (e.g., `"canceling"`, `"canceled"`).
#'   - Additional metadata as provided by the Claude API.
#'
#' @details
#' This function uses the Claude API's [cancel batch endpoint](https://docs.anthropic.com/en/api/message-batches-beta)
#' to attempt cancellation of the specified batch. The operation's outcome depends on the batch's current state.
#'
#' @seealso
#' - [claude_batch_job()] for initiating batch requests.
#' - [claude_check_batch_status()] for checking the status of a batch.
#' - [download_results()] for retrieving results from a batch.
#'
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

#' Submit a Batch Job to the Claude API
#'
#' This function submits a batch of prompts to the Claude API for processing.
#'
#' @param prompts A `list` of prompts to process, created using
#'   [build_prompts_from_files()].
#' @param model A character string specifying the Claude model to use. Refer to
#'   the [Claude API
#'   documentation](https://docs.anthropic.com/en/api/message-batches-beta) for
#'   available models.
#' @param max_tokens An integer specifying the maximum number of output tokens
#'   per prompt. Defaults to `300`.
#' @param temperature A number for the temperature per prompt. Defaults to `0.2`.
#' @param quiet A logical value indicating whether to suppress log messages.
#'   Defaults to `FALSE`.
#'
#' @return A `list` containing the batch response details, with class
#'   `"batch_claude"`. Typical fields include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The initial status of the batch (e.g., `"queued"` or `"in_progress"`).
#'   - Additional metadata as provided by the Claude API.
#'
#' @seealso
#' - [batch_job()] for the generic batch submission function.
#' - [check_batch_status()] for monitoring the status of a batch.
#' - [download_results()] for retrieving the results of a batch.
#'
#' @export
claude_batch_job <- function(prompts, model, max_tokens = 300, temperature = 0.2, quiet = FALSE) {
  claude_create_batch(prompts = prompts, model = model, max_tokens = max_tokens, temperature = temperature, quiet = quiet)

}
