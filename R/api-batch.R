#' Call a Language Model Batch API
#'
#' This function sends batch requests to supported language model APIs,
#' including OpenAI, Claude (Anthropic), and Mistral. It dispatches requests to
#' the appropriate method based on the class of `prompts`.
#'
#' @param prompts A list of prompts to send to the API, typically created via
#'   [prompt_list()]. The class of this object determines the API used:
#'   - `"claude"`: Uses the Anthropic Batch API ([Anthropic](https://docs.anthropic.com/en/api/creating-message-batches)).
#'   - `"openai"`: Uses the OpenAI Batch API ([OpenAI](https://platform.openai.com/docs/api-reference/batch)).
#'   - `"mistral"`: Uses the Mistral Batch API ([Mistral](https://docs.mistral.ai/capabilities/batch/)).
#' @param model A character string specifying the model to use. Refer to the
#'   API-specific documentation for available models.
#' @param ... Additional arguments passed to API-specific methods. Common
#'   options include:
#'   - `max_tokens`: Maximum number of output tokens (default: 300).
#'   - `quiet`: Suppresses logging messages during execution (default: `FALSE`).
#'   - API-specific parameters.
#'
#' @return A `list` containing API-specific response details, including:
#'   - `id`: The batch ID (if available).
#'   - `processing_status`: Current processing status.
#'
#'   For additional details, refer to the relevant API documentation:
#' - Claude: [Anthropic Message Batch API](https://docs.anthropic.com/en/api/creating-message-batches)
#' - OpenAI: [OpenAI Batch API](https://platform.openai.com/docs/api-reference/batch)
#' - Mistral: [Mistral Batch API](https://docs.mistral.ai/capabilities/batch/)
#'
#' @seealso
#' - [call_api()] for single-request API calls.
#' - [prompt_list()] for prompt creation utilities.
#'
#' @family batch
#' @export
call_batch_api <- function(prompts, model, ...) {
  UseMethod("call_batch_api", prompts)
}

#' @export
call_batch_api.claude <- function(prompts, model, ...) {
  args <- list(...)
  max_tokens <- args$max_tokens %||% 300
  quiet <- args$quiet %||% FALSE

  claude_batch_job(prompts = prompts, model = model, max_tokens = max_tokens, quiet = quiet)
}

#' @export
call_batch_api.openai <- function(prompts, model, ...) {
  args <- list(...)
  max_tokens <- args$max_tokens %||% 300
  quiet <- args$quiet %||% FALSE

  openai_batch_job(prompts = prompts, model = model, max_tokens = max_tokens, quiet = quiet)
}

#' @export
call_batch_api.mistral <- function(prompts, model, ...) {
  args <- list(...)
  max_tokens <- args$max_tokens %||% 300
  quiet <- args$quiet %||% FALSE

  mistral_batch_job(prompts = prompts, model = model, max_tokens = max_tokens, quiet = quiet)
}

#' List Batch Jobs Across APIs
#'
#' This function retrieves a list of batch jobs processed by the specified API, delegating the request to the appropriate API-specific function.
#'
#' @param api A character string specifying the target API. Supported values are:
#'   - `"claude"`: Retrieves batch jobs from the Anthropic API.
#'   - `"openai"`: Retrieves batch jobs from the OpenAI API.
#'   - `"mistral"`: Retrieves batch jobs from the Mistral API.
#'
#' @return A list containing the batch job details returned by the specified API. The structure of the response depends on the API used.
#'
#' @details The function dynamically calls the appropriate API-specific batch listing function based on the value of the `api` parameter. Ensure that the necessary authentication and configurations for the specified API are correctly set up before calling this function.
#'
#' @seealso
#' - [claude_list_batches()] for listing batch jobs using the Anthropic API.
#' - [openai_list_batches()] for listing batch jobs using the OpenAI API.
#' - [mistral_list_batches()] for listing batch jobs using the Mistral API.
#'
#' @family batch
#' @examples
#' \dontrun{
#' # List batch jobs from Claude API
#' claude_batches <- list_batches("claude")
#'
#' # List batch jobs from OpenAI API
#' openai_batches <- list_batches("openai")
#'
#' # List batch jobs from Mistral API
#' mistral_batches <- list_batches("mistral")
#' }
#'
#' @export
list_batches <- function(api) {
  switch (api,
    claude = claude_list_batches(),
    openai = openai_list_batches(),
    mistral = mistral_list_batches()
  )
}

#' Check the Status of a Message Batch
#'
#' This generic function retrieves the status of a batch of requests sent to a
#' language model API. It dispatches the request to an API-specific
#' implementation based on the class of the `batch_response`.
#'
#' @param batch_response A `list` containing details about the batch. The class
#'   of this object determines the API used:
#'   - `"batch_claude"`: Uses the Anthropic Batch API ([Anthropic](https://docs.anthropic.com/en/api/message-batches-beta)).
#'   - `"batch_openai"`: Uses the OpenAI API ([OpenAI](https://platform.openai.com/docs/api-reference/batch)).
#'   - `"batch_mistral"`: Uses the Mistral API ([Mistral](https://docs.mistral.ai/capabilities/batch/)).
#' @param ... Other parameters passed to methods, including:
#'   - **`max_retries`**: An integer specifying the maximum number of retry attempts in case of request failures. Defaults to `3`.
#'   - **`pause_cap`**: A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#'   - **`quiet`**: A logical value indicating whether to suppress log messages during retries. Defaults to `FALSE`.
#'
#' @return A `list` containing batch-specific status information, including
#'   fields such as:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The current processing status of the batch (e.g., `"in_progress"`, `"completed"`, `"failed"`).
#'   - Additional metadata depending on the API.
#'
#' @details The structure of the returned `list` depends on the API and its
#' capabilities. Refer to the API-specific documentation for more details:
#' - Claude: [Anthropic Message Batch API](https://docs.anthropic.com/en/api/message-batches-beta)
#' - OpenAI: [OpenAI Batch API](https://platform.openai.com/docs/api-reference/batch)
#' - Mistral: [Mistral Batch API](https://docs.mistral.ai/capabilities/batch/)
#'
#' @seealso
#' - [call_batch_api()] for initiating batch requests.
#' - API-specific status functions:
#'   - [claude_check_batch_status()]
#'   - [openai_check_batch_status()]
#'   - [mistral_check_batch_status()]
#'
#' @family batch
#' @export
check_batch_status <- function(batch_response, ...) {
  UseMethod("check_batch_status")
}


#' @export
check_batch_status.batch_openai <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE

  batch_status <- openai_check_batch_status(batch_response = batch_response,
                            max_retries = max_retries,
                            pause_cap = pause_cap,
                            quiet = quiet)

  batch_status
}

#' @export
check_batch_status.batch_claude <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE

  batch_status <- claude_check_batch_status(batch_response = batch_response,
                                            max_retries = max_retries,
                                            pause_cap = pause_cap,
                                            quiet = quiet)

  batch_status
}

#' @export
check_batch_status.batch_mistral <- function(batch_response, ...) {
  args <- list(...)
  quiet <- args$quiet %||% FALSE

  batch_status <- mistral_check_batch_status(batch_response = batch_response,
                                             quiet = quiet)

  batch_status

}

#' Download the Results of a Message Batch
#'
#' This generic function retrieves the output of a completed batch of requests
#' from a language model API. It dispatches the request to an API-specific
#' implementation based on the class of the `batch_response`.
#'
#' @inheritParams check_batch_status
#' @param ... Other parameters passed to API-specific methods, including:
#'   - **`max_retries`**: An integer specifying the maximum number of retry attempts in case of request failures. Defaults to `3`.
#'   - **`pause_cap`**: A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to `1200`.
#'   - **`quiet`**: A logical value indicating whether to suppress log messages during retries. Defaults to `FALSE`.
#'   - **`tidy`**: A logical value indicating whether to return the results in a tidy format. Defaults to `TRUE`.
#'
#' @return The structure of the returned object depends on the API and the `tidy` parameter:
#'   - If `tidy = TRUE`, the results are returned in a structured format (e.g., a `data.frame` or list, depending on the API).
#'   - If `tidy = FALSE`, the raw results are returned as they are received from the API.
#'
#' @seealso
#' - [check_batch_status()] for verifying the status of a batch before downloading results.
#' - API-specific download functions:
#'   - [claude_download_batch_results()]
#'   - [openai_download_batch_results()]
#'   - [mistral_download_batch_results()]
#'
#' @family batch
#' @export
download_results <- function(batch_response, ...) {
  UseMethod("download_results")
}

#' @export
download_results.batch_claude <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE
  tidy <- args$tidy %||% TRUE

  claude_download_batch_results(batch_response = batch_response,
                                max_retries = max_retries,
                                pause_cap = pause_cap,
                                quiet = quiet,
                                tidy = tidy)
}

#' @export
download_results.batch_openai <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE
  tidy <- args$tidy %||% TRUE

  openai_download_batch_results(batch_response = batch_response,
                                max_retries = max_retries,
                                pause_cap = pause_cap,
                                quiet = quiet,
                                tidy = tidy)
}

#' @export
download_results.batch_mistral <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE
  tidy <- args$tidy %||% TRUE

  mistral_download_batch_results(batch_response = batch_response,
                                 max_retries = max_retries,
                                 pause_cap = pause_cap,
                                 quiet = quiet,
                                 tidy = tidy)
}

#' Cancel a Message Batch
#'
#' This generic function cancels a batch of requests sent to a language model API.
#' It dispatches the request to an API-specific implementation based on the class of the `batch_response`.
#'
#' @inheritParams check_batch_status
#'
#' @return A `list` containing the updated batch status after cancellation. The structure of the returned
#'   object depends on the API, but typically includes:
#'   - **`id`**: A unique identifier for the batch.
#'   - **`status`**: The new status of the batch (e.g., `"canceling"`, `"canceled"`).
#'   - Additional metadata depending on the API.
#'
#' @details
#' The cancel operation attempts to stop further processing of the batch, but results may vary depending on the
#' API and the current state of the batch.
#'
#' @seealso
#' - [check_batch_status()] for verifying the status of a batch.
#' - [download_results()] for retrieving results from a batch.
#'
#' @family batch
#' @export
cancel_batch <- function(batch_response, ...) {
  UseMethod("cancel_batch")
}

#' @export
cancel_batch.batch_openai <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE

  batch_status <- openai_cancel_batch(batch_response = batch_response,
                                      max_retries = max_retries,
                                      pause_cap = pause_cap,
                                      quiet = quiet)

  batch_status
}

#' @export
cancel_batch.batch_claude <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE

  batch_status <- claude_cancel_batch(batch_response = batch_response,
                                            max_retries = max_retries,
                                            pause_cap = pause_cap,
                                            quiet = quiet)

  batch_status
}

#' @export
cancel_batch.batch_mistral <- function(batch_response, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE

  batch_status <- mistral_cancel_batch(batch_response = batch_response,
                                       max_retries = max_retries,
                                       pause_cap = pause_cap,
                                       quiet = quiet)

  batch_status

}

#' Submit a Batch Job to a Language Model API
#'
#' This generic function submits a batch of prompts to a language model API for
#' processing. It dispatches the request to an API-specific implementation based
#' on the class of `prompts`.
#'
#' @inheritParams call_batch_api
#'
#' @inherit call_batch_api return
#'
#' @details The returned object is assigned a class that reflects the API used
#' (e.g., `"batch_claude"`, `"batch_openai"`, `"batch_mistral"`).
#'
#' @seealso
#' - [check_batch_status()] for verifying the status of a batch.
#' - [download_results()] for retrieving the results of a batch.
#'
#' @family batch
#' @export
batch_job <- function(prompts, model, ...) {
  UseMethod("batch_job")
}

#' @export
batch_job.claude <- function(prompts, model, ...) {
  # Extract additional arguments
  args <- list(...)
  max_tokens <- args$max_tokens %||% 300
  temperature <- args$temperature %||% 0.2
  quiet <- args$quiet %||% FALSE

  # Call the Claude-specific batch job function
  batch_response <- claude_batch_job(
    prompts = prompts,
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    quiet = quiet
  )

  # Assign class to the response
  class(batch_response) <- c("batch_claude", class(batch_response))
  batch_response
}
#' @export
batch_job.openai <- function(prompts, model, ...) {
  args <- list(...)
  max_tokens <- args$max_tokens %||% 300
  temperature <- args$temperature %||% 0.2
  quiet <- args$quiet %||% FALSE

  batch_response <- openai_batch_job(
    prompts = prompts,
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    quiet = quiet
  )

  class(batch_response) <- c("batch_openai", class(batch_response))
  batch_response
}

#' @export
batch_job.mistral <- function(prompts, model, ...) {
  args <- list(...)
  max_tokens <- args$max_tokens %||% 300
  temperature <- args$temperature %||% 0.2
  quiet <- args$quiet %||% FALSE

  batch_response <- mistral_batch_job(
    prompts = prompts,
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    quiet = quiet
  )

  class(batch_response) <- c("batch_mistral", class(batch_response))
  batch_response
}

#' Poll and Download the Results of a Message Batch
#'
#' This generic function repeatedly checks the status of a batch of requests
#' sent to a language model API, waiting for completion before downloading the
#' results. It dispatches the request to an API-specific implementation based on
#' the class of `batch_response`.
#'
#' @inheritParams download_results
#'
#' @param timeout An integer specifying the maximum time (in seconds) to wait
#'   for the batch to complete or to poll the API again. Defaults to `3600` (1 hour).
#'
#' @return A `list` or [tibble()] containing the results of the batch:
#'   - If `tidy = TRUE`, the results are returned in a structured format (e.g., a [tibble()] where each row corresponds to a prompt-completion pair).
#'   - If `tidy = FALSE`, the raw results are returned as received from the API.
#'
#' @details
#'   The function repeatedly checks the status of the batch until it is
#'   completed, canceled, or the timeout is reached. Once the batch is complete,
#'   the results are downloaded.
#'
#' @seealso
#' - [check_batch_status()] for monitoring batch status.
#' - [download_results()] for downloading batch results without polling.
#'
#' @family batch
#' @export
poll_and_download <- function(batch_response, timeout = 3600, ...) {
  UseMethod("poll_and_download")
}

#' @export
poll_and_download.batch_claude <- function(batch_response, timeout = 3600, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE
  tidy <- args$tidy %||% TRUE

  results <- claude_poll_and_download(
    batch_response = batch_response,
    timeout = timeout,
    max_retries = max_retries,
    pause_cap = pause_cap,
    quiet = quiet,
    tidy = tidy
  )

  results
}

#' @export
poll_and_download.batch_openai <- function(batch_response, timeout = 3600, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE
  tidy <- args$tidy %||% TRUE

  results <- openai_poll_and_download(
    batch_response = batch_response,
    timeout = timeout,
    max_retries = max_retries,
    pause_cap = pause_cap,
    quiet = quiet,
    tidy = tidy
  )

  results
}

#' @export
poll_and_download.batch_mistral <- function(batch_response, timeout = 3600, ...) {
  args <- list(...)
  max_retries <- args$max_retries %||% 3
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE
  tidy <- args$tidy %||% TRUE

  results <- mistral_poll_and_download(
    batch_response = batch_response,
    timeout = timeout,
    max_retries = max_retries,
    pause_cap = pause_cap,
    quiet = quiet,
    tidy = tidy
  )

  results
}


