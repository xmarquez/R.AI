openai_format_prompts_for_batch <- function(prompts, model, max_tokens = 300, temperature = 0.2) {
  json_lines <- lapply(names(prompts), function(name) {
    list(
      custom_id = name,
      method = "POST",
      url = "/v1/chat/completions",
      body = list(
        model = model,
        messages = prompts[[name]],
        max_tokens = max_tokens,
        temperature = temperature
      )
    )
  })
  jsonl_string <- paste(sapply(json_lines, jsonlite::toJSON, auto_unbox = TRUE), collapse = "\n")
  jsonl_string
}

openai_upload_batch_file <- function(jsonl, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  # Make the API call to upload the file using httr::RETRY to handle retries

  tmp_file <- fs::file_temp(ext = "jsonl")
  writeLines(jsonl, tmp_file)

  res <- httr::RETRY(
    verb = "POST",
    url = "https://api.openai.com/v1/files",
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "multipart/form-data"
    ),
    body = list(
      purpose = "batch",
      file = httr::upload_file(tmp_file)
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

openai_create_batch <- function(upload_response, endpoint = "/v1/chat/completions",
                                completion_window = "24h", max_retries = 3,
                                pause_cap = 1200, quiet = FALSE) {

  stopifnot(completion_window == "24h")

  file_id <- upload_response$id

  body <- jsonlite::toJSON(
    list(
      input_file_id = file_id,
      endpoint = endpoint,
      completion_window = completion_window
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  res <- retry_response(base_url = "https://api.openai.com/v1/batches",
                        api_key = Sys.getenv("OPENAI_API_KEY"),
                        response_format = NULL,
                        body = body,
                        max_retries = max_retries,
                        pause_cap = pause_cap,
                        quiet = quiet)

  # Check if the response contains an error
  httr::stop_for_status(res)
  response <- httr::content(res)

  class(response) <- c("batch_openai", class(response))

  # Return the response details
  response
}

#' Check the Status of a Message Batch Using the OpenAI API
#'
#' This function retrieves details about a specific message batch using the
#' OpenAI API. It is used to poll the status of a batch, monitor its progress,
#' or retrieve associated metadata.
#'
#' @param batch_response A `list` returned by [openai_batch_job()] containing
#'   the batch details. Alternatively, pass an object from
#'   [openai_list_batches()], such as `openai_list_batches()$data[[1]]`.
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
#'   - **`endpoint`**: The API endpoint used for the batch.
#'   - **`errors`**: A list of errors encountered during processing, if any.
#'   - **`input_file_id`**: The ID of the file containing input data for the batch.
#'   - **`completion_window`**: The time window for completing the batch (e.g., `"24h"`).
#'   - **`status`**: The current processing status of the batch (e.g., `"completed"`, `"in_progress"`, `"failed"`).
#'   - **`output_file_id`**: The ID of the file containing the batch's completed output.
#'   - **`error_file_id`**: The ID of the file containing error logs, if available.
#'   - **`created_at`**: A UNIX timestamp indicating when the batch was created.
#'   - **`in_progress_at`**: A UNIX timestamp indicating when processing began.
#'   - **`expires_at`**: A UNIX timestamp indicating when the batch will expire.
#'   - **`finalizing_at`**: A UNIX timestamp indicating when the batch began finalization.
#'   - **`completed_at`**: A UNIX timestamp indicating when the batch was completed.
#'   - **`failed_at`**: A UNIX timestamp indicating when the batch failed, if applicable.
#'   - **`expired_at`**: A UNIX timestamp indicating when the batch expired, if applicable.
#'   - **`cancelling_at`**: A UNIX timestamp indicating when the batch began cancellation, if applicable.
#'   - **`cancelled_at`**: A UNIX timestamp indicating when the batch was cancelled, if applicable.
#'   - **`request_counts`**: A summary of request counts in the batch:
#'       - **`total`**: Total number of requests in the batch.
#'       - **`completed`**: Number of successfully completed requests.
#'       - **`failed`**: Number of failed requests.
#'   - **`metadata`**: Additional metadata about the batch, including:
#'       - **`customer_id`**: A customer-provided identifier for tracking (e.g., `"user_123456789"`).
#'       - **`batch_description`**: A description of the batch (e.g., `"Nightly eval job"`).
#'
#' @details This function uses the [OpenAI Batch
#' API](https://platform.openai.com/docs/api-reference/batch) to retrieve
#' detailed status information for a batch. The `status` field can indicate one
#' of several states, such as `"in_progress"`, `"completed"`, or `"failed"`.
#'
#' @seealso
#' - [openai_batch_job()] for batch creation.
#' - [openai_download_batch_results()] for retrieving completed results.
#' - [openai_list_batches()] for listing existing batches.
#'
#' @family openai
#' @family batch
#' @export
openai_check_batch_status <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  batch_id <- batch_response$id

  url <- paste0("https://api.openai.com/v1/batches/", batch_id)

  res <- httr::RETRY(
    verb = "GET",
    url = url,
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
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
  class(response) <- c("batch_openai", class(response))

  # Return the response details
  response
}

#' Download the Results of an OpenAI Message Batch
#'
#' This function retrieves the results of a completed message batch processed by the OpenAI API.
#' It supports retrying failed requests and returning results in a tidy format.
#'
#' @param batch_response A `list` containing details about the batch, typically returned by
#'   [openai_batch_job()] or [openai_check_batch_status()]. The `batch_response` must include a valid `output_file_id`.
#' @param max_retries An integer specifying the maximum number of retry attempts in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages during retries. Defaults to `FALSE`.
#' @param tidy A logical value indicating whether to return the results in a tidy format (e.g., a `data.frame`). Defaults to `TRUE`.
#'
#' @return A `list` or `data.frame` containing the batch results:
#'   - If `tidy = TRUE`, results are returned in a structured format, typically as a `data.frame` where each row corresponds to a prompt-completion pair.
#'   - If `tidy = FALSE`, raw results are returned as received from the API.
#'
#' @details
#' The function downloads batch results using the OpenAI API's [batch output endpoint](https://platform.openai.com/docs/api-reference/batch).
#' The returned results typically include model completions for each prompt submitted in the batch.
#'
#' @seealso
#' - [openai_batch_job()] for initiating batch requests.
#' - [openai_check_batch_status()] for verifying the status of a batch before downloading results.
#' - [download_results()] for generic batch result handling.
#'
#' @family openai
#' @family batch
#' @export
openai_download_batch_results <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE, tidy = TRUE) {
  output_file_id <- batch_response$output_file_id

  if (is.null(output_file_id)) {
    stop("The batch is not yet complete or there is no output file available.")
  }

  url <- paste0("https://api.openai.com/v1/files/", output_file_id, "/content")

  res <- httr::RETRY(
    verb = "GET",
    url = url,
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
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
      tidyr::unnest("response") |>
      dplyr::rename(openai_id = "id") |>
      tidyr::unnest("body") |>
      dplyr::rename(object_id = "id",
                    id = "custom_id") |>
      tidyr::unnest("choices") |>
      tidyr::unnest("message") |>
      dplyr::rename(response = "content") |>
      tidyr::unnest("usage")
  }

  response_content
}

#' Poll and Download the Results of an OpenAI Message Batch
#'
#' This function repeatedly checks the status of an OpenAI message batch,
#' waiting for its completion before downloading the results.
#'
#' @param batch_response A `list` containing details about the batch to poll,
#'   typically returned by [openai_batch_job()] or
#'   [openai_check_batch_status()].
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
#'   - If `tidy = FALSE`, the raw results are returned as received from the OpenAI API.
#'
#' @details The function repeatedly checks the status of the batch using the
#' OpenAI API's [batch status
#' endpoint](https://platform.openai.com/docs/api-reference/batch), and
#' downloads the results once the batch is complete.
#'
#' @seealso
#' - [poll_and_download()] for the generic polling and downloading function.
#' - [openai_batch_job()] for initiating a batch request.
#' - [openai_download_batch_results()] for downloading results without polling.
#'
#' @family openai
#' @family batch
#' @export
openai_poll_and_download <- function(batch_response, timeout = 3600,
                                             max_retries = 3, pause_cap = 1200,
                                             tidy = TRUE, quiet = FALSE) {
  repeat {
    # Retrieve the current status of the batch
    status_response <- openai_check_batch_status(batch_response, max_retries = max_retries,
                                                 pause_cap = pause_cap, quiet = quiet)

    processing_status <- status_response$status
    output_file_id <- status_response$output_file_id

    if (processing_status == "completed" && !is.null(output_file_id)) {
      message("Batch processing completed. Retrieving results...")
      results <- openai_download_batch_results(status_response, max_retries = max_retries,
                                               pause_cap = pause_cap, quiet = quiet, tidy = tidy)
      return(results)
    } else if (processing_status == "expired" && !is.null(output_file_id)) {
      message("Batch processing expired. Retrieving available results...")
      results <- openai_download_batch_results(status_response, max_retries = max_retries,
                                               pause_cap = pause_cap, quiet = quiet, tidy = tidy)
      return(results)
    } else if (processing_status == "in_progress" ||
               processing_status == "validating" ||
               processing_status == "finalizing") {
      if(processing_status == "finalizing") {
        timeout <- timeout/10
      }
      message(glue::glue("{lubridate::now()}: Batch still in progress. Waiting for {timeout} seconds before next poll..."))
      Sys.sleep(timeout)
    } else {
      stop("Batch ", processing_status, ".")
    }
  }
}

#' Cancel an OpenAI Message Batch
#'
#' This function cancels a batch of requests submitted to the OpenAI API.
#' The operation attempts to stop further processing of the batch.
#'
#' @param batch_response A `list` containing details about the batch to cancel, typically returned by
#'   [openai_batch_job()] or [openai_check_batch_status()].
#' @param max_retries An integer specifying the maximum number of retry attempts in case of request failures. Defaults to `3`.
#' @param pause_cap A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#' @param quiet A logical value indicating whether to suppress log messages during retries. Defaults to `FALSE`.
#'
#' @return A `list` containing the updated batch status after the cancellation attempt. Typical fields include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The new status of the batch (e.g., `"canceling"`, `"canceled"`).
#'   - Additional metadata as provided by the OpenAI API.
#'
#' @details
#' This function uses the OpenAI API's [cancel batch endpoint](https://platform.openai.com/docs/api-reference/batch)
#' to attempt cancellation of the specified batch. The operation's outcome depends on the batch's current state.
#'
#' @seealso
#' - [openai_batch_job()] for initiating batch requests.
#' - [openai_check_batch_status()] for checking the status of a batch.
#' - [download_results()] for retrieving results from a batch.
#'
#' @family openai
#' @family batch
#' @export
openai_cancel_batch <- function(batch_response, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  batch_id <- batch_response$id

  url <- paste0("https://api.openai.com/v1/batches/", batch_id, "/cancel")

  res <- httr::RETRY(
    verb = "POST",
    url = url,
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
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

#' List OpenAI Batches
#'
#' This function lists all the batches being processed for a user using the
#' OpenAI Batch API.
#'
#' @param limit An integer specifying the maximum number of batches to return.
#'   Defaults to 10.
#' @param max_retries An integer indicating the maximum number of retry attempts
#'   in case of request failures. Defaults to 3.
#' @param pause_cap A numeric value representing the maximum pause duration (in
#'   seconds) between retries. Defaults to 1200.
#' @param quiet A logical value indicating whether the function should suppress
#'   messages during retries. Defaults to `FALSE`.
#'
#' @return A list containing the details of the batches.
#'
#' @family openai
#' @family batch
#' @export
openai_list_batches <- function(limit = 10, max_retries = 3, pause_cap = 1200, quiet = FALSE) {
  url <- paste0("https://api.openai.com/v1/batches?limit=", limit)

  res <- httr::RETRY(
    verb = "GET",
    url = url,
    config = httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
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

#' Submit a Batch Job to the OpenAI API
#'
#' This function submits a batch of prompts to the OpenAI API for processing.
#'
#' @param prompts A `list` of prompts to process created by [build_prompts_from_files()].
#' @param model A character string specifying the OpenAI model to use. Refer to
#'   the [OpenAI API
#'   documentation](https://platform.openai.com/docs/api-reference/batch) for
#'   available models.
#' @param max_tokens An integer specifying the maximum number of output tokens
#'   per prompt. Defaults to `300`.
#' @param temperature A number for the temperature per prompt. Defaults to `0.2`.
#' @param quiet A logical value indicating whether to suppress log messages.
#'   Defaults to `FALSE`.
#'
#' @return A `list` containing the batch response details, with class
#'   `"batch_openai"`. Typical fields include:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The initial status of the batch (e.g., `"queued"` or `"in_progress"`).
#'   - Additional metadata as provided by the OpenAI API.
#'
#' @seealso
#' - [batch_job()] for the generic batch submission function.
#' - [check_batch_status()] for monitoring the status of a batch.
#' - [download_results()] for retrieving the results of a batch.
#'
#' @family openai
#' @family batch
#' @export
openai_batch_job <- function(prompts, model, max_tokens = 300, temperature = 0.2, quiet = FALSE) {
  checkmate::assert_class(prompts, "openai")
  if(missing(model)) {
    model <- get_default_model("openai")
  }
  jsonl <- openai_format_prompts_for_batch(prompts, model = model, max_tokens = max_tokens, temperature = temperature)
  batch_file <- openai_upload_batch_file(jsonl = jsonl, quiet = quiet)
  batch_job <- openai_create_batch(batch_file, quiet = quiet)
  batch_job
}
