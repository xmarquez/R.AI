% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/claude-batch.R
\name{claude_download_batch_results}
\alias{claude_download_batch_results}
\title{Retrieve Message Batch Results Using the Anthropic API}
\usage{
claude_download_batch_results(
  batch_response,
  max_retries = 3,
  pause_cap = 1200,
  quiet = FALSE,
  tidy = TRUE
)
}
\arguments{
\item{batch_response}{The list returned by \code{\link[=claude_create_batch]{claude_create_batch()}},
containing the string representing the unique ID of the message batch to
retrieve. (It's also possible to pass the specific object from
\code{\link[=claude_list_batches]{claude_list_batches()}}, e.g., claude_list_batches()$data[\link{1}]).}

\item{max_retries}{An integer indicating the maximum number of retry attempts
in case of request failures. Defaults to 3.}

\item{pause_cap}{A numeric value representing the maximum pause duration (in
seconds) between retries. Defaults to 1200.}

\item{quiet}{A logical value indicating whether the function should suppress
messages during retries. Defaults to \code{FALSE}.}

\item{tidy}{A logical value indicating whether to attempt to tidy the
resulting json into a tidy \link{tibble}. Default is \code{TRUE}; \code{FALSE} is useful
if you want to do the tidying separately or prefer the raw json.}
}
\value{
A character vector containing each line of the .jsonl result file.
}
\description{
This function retrieves the results for a specific message batch using the
\href{https://docs.anthropic.com/en/api/message-batches-beta}{Message Batch endpoint}. The
results are streamed as a .jsonl file.
}
