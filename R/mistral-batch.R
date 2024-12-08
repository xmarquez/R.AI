mistral_format_prompts_for_batch <- function(prompts, max_tokens = 100, temperature = 0.2) {
  json_lines <- lapply(names(prompts), function(name) {
    list(
      custom_id = name,
      body = list(
        max_tokens = max_tokens,
        temperature = temperature,
        messages = prompts[[name]]
      )
    )
  })
  jsonl_string <- paste(sapply(json_lines, jsonlite::toJSON, auto_unbox = TRUE), collapse = "\n")
  jsonl_string
}

mistral_upload_batch_file <- function(jsonl, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  tmp_file <- fs::file_temp(ext = "jsonl")
  writeLines(jsonl, tmp_file)

  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.mistral.ai/v1/files",
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")),
      "Content-Type" = "multipart/form-data"
    ),
    body = list(
      file = httr::upload_file(tmp_file),
      purpose = "batch"
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

mistral_create_batch <- function(upload_response, model, endpoint = "/v1/chat/completions", metadata = NULL,
                                     max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  file_id <- upload_response$id

  body <- jsonlite::toJSON(
    list(
      model = model,
      input_files = list(file_id),
      endpoint = endpoint,
      metadata = metadata
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.mistral.ai/v1/batch/jobs",
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")),
      "Content-Type" = "application/json"
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

  class(response) <- c("batch_mistral", class(response))

  # Return the response details
  response
}

#' Check the Status of a Message Batch Using the Mistral API
#'
#' This function retrieves details about a specific message batch using the
#' Mistral API. It is useful for monitoring batch progress, checking for errors,
#' or accessing output files.
#'
#' @param batch_response A `list` returned by [mistral_create_batch()]
#'   containing the batch details. Alternatively, pass an object from
#'   [mistral_list_batches()], such as `mistral_list_batches()$data[[1]]`.
#' @param max_retries An integer specifying the maximum number of retry attempts
#'   in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages
#'   during retries. Defaults to `FALSE`.
#'
#' @return A `list` containing detailed information about the specified message
#'   batch. The fields in the result include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`object`**: The type of object returned (e.g., `"batch"`).
#'   - **`input_files`**: A list of input file IDs associated with the batch.
#'   - **`metadata`**: A list containing metadata associated with the batch (if provided).
#'   - **`endpoint`**: The API endpoint used for the batch.
#'   - **`model`**: The model specified for the batch.
#'   - **`output_file`**: The ID of the file containing the batch's output.
#'   - **`error_file`**: The ID of the file containing the error logs, if available.
#'   - **`errors`**: A list of errors encountered during processing, where each error includes:
#'       - **`message`**: A description of the error.
#'       - **`count`**: The number of occurrences of the error.
#'   - **`status`**: The current processing status of the batch (e.g., `"QUEUED"`, `"IN_PROGRESS"`, `"COMPLETED"`, `"FAILED"`).
#'   - **`created_at`**: A UNIX timestamp indicating when the batch was created.
#'   - **`started_at`**: A UNIX timestamp indicating when processing began.
#'   - **`completed_at`**: A UNIX timestamp indicating when the batch was completed, if applicable.
#'   - **`total_requests`**: The total number of requests in the batch.
#'   - **`completed_requests`**: The number of requests that have been processed.
#'   - **`succeeded_requests`**: The number of requests that succeeded.
#'   - **`failed_requests`**: The number of requests that failed.
#'
#' @details This function uses the [Mistral Batch
#' API](https://docs.mistral.ai/capabilities/batch/) to retrieve detailed status
#' information for a batch. The `status` field indicates the current state of
#' the batch and can include values such as `"QUEUED"`, `"IN_PROGRESS"`,
#' `"COMPLETED"`, or `"FAILED"`. The `errors` field provides a breakdown of any
#' issues encountered during batch processing.
#'
#' @seealso
#' - [mistral_create_batch()] for batch creation.
#' - [mistral_download_batch_results()] for retrieving completed results.
#' - [mistral_list_batches()] for listing existing batches.
#'
#' @export
mistral_check_batch_status <- function(batch_response, quiet = FALSE) {
  # Validate input
  if (is.null(batch_response$id)) {
    stop("Invalid batch response: 'id' field is missing.")
  }

  # Extract the job ID
  job_id <- batch_response$id

  # Construct the URL for the batch job status endpoint
  url <- paste0("https://api.mistral.ai/v1/batch/jobs/", job_id)

  # Make the API request
  res <- httr::GET(
    url = url,
    config = httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")))
  )

  # Check for HTTP errors
  httr::stop_for_status(res)

  # Parse the response content
  response <- httr::content(res, as = "parsed", type = "application/json")

  # If not quiet, print the status
  if (!quiet) {
    message(glue::glue("Batch job status: {response$status}"))
  }

  # Set the class of the response as a batch object
  class(response) <- c("batch_mistral", class(response))

  # Return the parsed response
  return(response)
}

#' Download the Results of a Mistral Message Batch
#'
#' This function retrieves the results of a completed message batch processed by
#' the Mistral API. It supports retrying failed requests and returning results
#' in a tidy format.
#'
#' @param batch_response A `list` containing details about the batch, typically
#'   returned by [mistral_create_batch()] or [mistral_check_batch_status()]. The
#'   `batch_response` must include a valid `output_file`.
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
#' @details The function downloads batch results using the Mistral API's [batch
#' output endpoint](https://docs.mistral.ai/capabilities/batch/). The returned
#' results typically include model completions for each prompt submitted in the
#' batch.
#'
#' @seealso
#' - [mistral_create_batch()] for initiating batch requests.
#' - [mistral_check_batch_status()] for verifying the status of a batch before downloading results.
#' - [download_results()] for generic batch result handling.
#'
#' @export
mistral_download_batch_results <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE, tidy = TRUE) {
  # Validate input
  if (is.null(batch_response$output_file)) {
    stop("Invalid batch response: 'output_file' field is missing.")
  }

  # Extract the output file ID
  output_file_id <- batch_response$output_file
  error_file_id <- batch_response$error_file

  # Construct the URL for the output file download
  url <- paste0("https://api.mistral.ai/v1/files/", output_file_id, "/content")

  # Make the API request
  res <- httr::RETRY(
    verb = "GET",
    url = url,
    config = httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY"))),
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check for HTTP errors
  httr::stop_for_status(res)

  # Parse and return the response content
  response_content <- httr::content(res, as = "text", encoding = "UTF-8")

  error_content <- NULL

  if(!is.null(error_file_id)) {
    # Construct the URL for the output file download
    error_url <- paste0("https://api.mistral.ai/v1/files/", error_file_id, "/content")

    # Make the API request
    res <- httr::RETRY(
      verb = "GET",
      url = error_url,
      config = httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY"))),
      times = max_retries,
      pause_base = 1,
      pause_cap = pause_cap,
      quiet = quiet
    )

    # Check for HTTP errors
    httr::stop_for_status(res)

    # Parse and return the response content
    error_content <- httr::content(res, as = "text", encoding = "UTF-8")
  }

  if(tidy) {
    # Tidy response
    response_content <- jsonlite::stream_in(textConnection(response_content), verbose = !quiet) |>
      tibble::as_tibble() |>
      tidyr::unnest("response") |>
      dplyr::rename(mistral_id = "id") |>
      tidyr::unnest("body") |>
      dplyr::rename(object_id = "id",
                    id = "custom_id") |>
      tidyr::unnest("choices") |>
      tidyr::unnest("message") |>
      dplyr::rename(response = "content") |>
      tidyr::unnest("usage")

    if(!is.null(error_content)) {
      error_content <- jsonlite::stream_in(textConnection(error_content), verbose = !quiet) |>
        tibble::as_tibble() |>
        tidyr::unnest("response") |>
        dplyr::rename(mistral_id = "id",
                      id = "custom_id")

      response_content <- dplyr::bind_rows(response_content,
                                           error_content)
    }

  }

  return(response_content)
}

#' Cancel a Mistral Message Batch
#'
#' This function cancels a batch of requests submitted to the Mistral API.
#' The operation attempts to stop further processing of the batch.
#'
#' @param batch_response A `list` containing details about the batch to cancel, typically returned by
#'   [mistral_create_batch()] or [mistral_check_batch_status()].
#' @param max_retries An integer specifying the maximum number of retry attempts in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages during retries. Defaults to `FALSE`.
#'
#' @return A `list` containing the updated batch status after the cancellation attempt. Typical fields include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The new status of the batch (e.g., `"canceling"`, `"canceled"`).
#'   - Additional metadata as provided by the Mistral API.
#'
#' @details
#' This function uses the Mistral API's [cancel batch endpoint](https://docs.mistral.ai/capabilities/batch)
#' to attempt cancellation of the specified batch. The operation's outcome depends on the batch's current state.
#'
#' @seealso
#' - [mistral_create_batch()] for initiating batch requests.
#' - [mistral_check_batch_status()] for checking the status of a batch.
#' - [download_results()] for retrieving results from a batch.
#'
#' @export
mistral_cancel_batch <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  # Validate input
  if (is.null(batch_response$id)) {
    stop("Invalid batch response: 'id' field is missing.")
  }

  # Extract the job ID
  job_id <- batch_response$id

  # Construct the cancellation URL
  url <- paste0("https://api.mistral.ai/v1/batch/jobs/", job_id, "/cancel")

  # Make the API request
  res <- httr::RETRY(
    verb = "POST",
    url = url,
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")),
      "Content-Type" = "application/json"
    ),
    times = max_retries,
    pause_base = 1,
    pause_cap = pause_cap,
    quiet = quiet
  )

  # Check for HTTP errors
  httr::stop_for_status(res)

  # Parse and return the response content
  response <- httr::content(res, as = "parsed", type = "application/json")
  return(response)
}

#' List Mistral Batch Jobs
#'
#' This function lists all the batch jobs being processed for a user using the Mistral Batch API.
#'
#' @param status An optional filter to specify the status of jobs to return. Defaults to NULL (returns all statuses).
#' @param limit An integer specifying the maximum number of batches to return. Defaults to 10.
#' @param max_retries An integer indicating the maximum number of retry attempts in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress messages during retries. Defaults to `FALSE`.
#'
#' @return A list containing the details of the batches.
#' @export
mistral_list_batches <- function(page = 0, page_size = 100, model = NULL, status = NULL, quiet = FALSE) {
  # Validate inputs
  checkmate::assert_integerish(page, lower = 0, len = 1)
  checkmate::assert_integerish(page_size, lower = 1, upper = 100, len = 1)

  # Construct query parameters
  query_params <- list(
    page = page,
    page_size = page_size,
    model = model,
    status = status
  )

  # Remove NULL values
  query_params <- purrr::compact(query_params)

  # API call
  res <- httr::GET(
    url = "https://api.mistral.ai/v1/batch/jobs",
    query = query_params,
    config = httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")))
  )

  # Check for HTTP errors
  httr::stop_for_status(res)
  # Parse response
  response <- httr::content(res, as = "parsed", type = "application/json")

  return(response)
}


#' Poll and Download the Results of a Mistral Message Batch
#'
#' This function repeatedly checks the status of a Mistral message batch, waiting for its completion before downloading the results.
#'
#' @param batch_response A `list` containing details about the batch to poll, typically returned by
#'   [mistral_create_batch()] or [mistral_check_batch_status()].
#' @param timeout An integer specifying the maximum time (in seconds) to wait for the batch to complete. Defaults to `3600` (1 hour).
#' @param max_retries An integer specifying the maximum number of retry attempts for status checks or download failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages during polling and downloading. Defaults to `FALSE`.
#' @param tidy A logical value indicating whether to return the results in a tidy format (e.g., a `data.frame`). Defaults to `TRUE`.
#'
#' @return A `list` or `data.frame` containing the results of the batch:
#'   - If `tidy = TRUE`, the results are returned in a structured format (e.g., a `data.frame` where each row corresponds to a prompt-completion pair).
#'   - If `tidy = FALSE`, the raw results are returned as received from the Mistral API.
#'
#' @details
#' The function repeatedly checks the status of the batch using the Mistral API's [batch status endpoint](https://docs.mistral.ai/capabilities/batch/),
#' and downloads the results once the batch is complete.
#'
#' @seealso
#' - [poll_and_download()] for the generic polling and downloading function.
#' - [mistral_batch_job()] for initiating a batch request.
#' - [mistral_download_batch_results()] for downloading results without polling.
#'
#' @export
mistral_poll_and_download <- function(batch_response, timeout = 3600,
                                      max_retries = 3, pause_cap = 1200,
                                      tidy = TRUE, quiet = FALSE) {
  repeat {
    # Check the current status of the batch job
    status_response <- mistral_check_batch_status(batch_response, quiet = quiet)
    processing_status <- status_response$status
    output_file_id <- status_response$output_file

    if (processing_status == "SUCCESS" && !is.null(output_file_id)) {
      message("Batch processing completed. Retrieving results...")
      # Retrieve and return the results
      results <- mistral_download_batch_results(status_response, max_retries = max_retries,
                                                pause_cap = pause_cap, quiet = quiet, tidy = tidy)
      return(results)
    } else if (processing_status %in% c("QUEUED", "RUNNING")) {
      # If job is still processing, wait and retry
      message(glue::glue("{lubridate::now()}: Batch still in progress. Waiting for {timeout} seconds before next poll..."))
      Sys.sleep(timeout)
    } else {
      # Handle unexpected or failed states
      stop(glue::glue("Batch job failed or is in an unexpected state: {processing_status}."))
    }
  }
}


#' Submit a Batch Job to the Mistral API
#'
#' This function submits a batch of prompts to the Mistral API for processing.
#'
#' @param prompts A `list` of prompts to process. Each prompt should follow the
#'   Mistral API's expected format.
#' @param model A character string specifying the Mistral model to use. Refer to
#'   the [Mistral API
#'   documentation](https://docs.mistral.ai/capabilities/batch/) for available
#'   models.
#' @param max_tokens An integer specifying the maximum number of output tokens
#'   per prompt. Defaults to `300`.
#' @param temperature A number for the temperature per prompt. Defaults to `0.2`.
#' @param quiet A logical value indicating whether to suppress log messages.
#'   Defaults to `FALSE`.
#'
#' @return A `list` containing the batch response details, with class
#'   `"batch_mistral"`. Typical fields include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The initial status of the batch (e.g., `"queued"` or `"in_progress"`).
#'   - Additional metadata as provided by the Mistral API.
#'
#' @seealso
#' - [batch_job()] for the generic batch submission function.
#' - [check_batch_status()] for monitoring the status of a batch.
#' - [download_results()] for retrieving the results of a batch.
#'
#' @export
mistral_batch_job <- function(prompts, model, max_tokens = 300, temperature = 0.2, quiet = FALSE) {
  checkmate::assert_class(prompts, "mistral")
  jsonl <- mistral_format_prompts_for_batch(prompts, max_tokens = max_tokens, temperature = temperature)
  batch_file <- mistral_upload_batch_file(jsonl = jsonl, quiet = quiet)
  batch_job <- mistral_create_batch(batch_file, model = model, quiet = quiet)
  batch_job
}
