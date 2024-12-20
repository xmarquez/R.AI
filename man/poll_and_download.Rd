% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-batch.R
\name{poll_and_download}
\alias{poll_and_download}
\title{Poll and Download the Results of a Message Batch}
\usage{
poll_and_download(batch_response, timeout = 3600, ...)
}
\arguments{
\item{batch_response}{A \code{list} containing details about the batch. The class
of this object determines the API used:
\itemize{
\item \code{"batch_claude"}: Uses the Anthropic Batch API (\href{https://docs.anthropic.com/en/api/message-batches-beta}{Anthropic}).
\item \code{"batch_openai"}: Uses the OpenAI API (\href{https://platform.openai.com/docs/api-reference/batch}{OpenAI}).
\item \code{"batch_mistral"}: Uses the Mistral API (\href{https://docs.mistral.ai/capabilities/batch/}{Mistral}).
}}

\item{timeout}{An integer specifying the maximum time (in seconds) to wait
for the batch to complete or to poll the API again. Defaults to \code{3600} (1 hour).}

\item{...}{Other parameters passed to API-specific methods, including:
\itemize{
\item \strong{\code{max_retries}}: An integer specifying the maximum number of retry attempts in case of request failures. Defaults to \code{3}.
\item \strong{\code{pause_cap}}: A numeric value representing the maximum pause duration (in seconds) between retries. Defaults to \code{1200}.
\item \strong{\code{quiet}}: A logical value indicating whether to suppress log messages during retries. Defaults to \code{FALSE}.
\item \strong{\code{tidy}}: A logical value indicating whether to return the results in a tidy format. Defaults to \code{TRUE}.
}}
}
\value{
A \code{list} or \code{\link[=tibble]{tibble()}} containing the results of the batch:
\itemize{
\item If \code{tidy = TRUE}, the results are returned in a structured format (e.g., a \code{\link[=tibble]{tibble()}} where each row corresponds to a prompt-completion pair).
\item If \code{tidy = FALSE}, the raw results are returned as received from the API.
}
}
\description{
This generic function repeatedly checks the status of a batch of requests
sent to a language model API, waiting for completion before downloading the
results. It dispatches the request to an API-specific implementation based on
the class of \code{batch_response}.
}
\details{
The function repeatedly checks the status of the batch until it is
completed, canceled, or the timeout is reached. Once the batch is complete,
the results are downloaded.
}
\seealso{
\itemize{
\item \code{\link[=check_batch_status]{check_batch_status()}} for monitoring batch status.
\item \code{\link[=download_results]{download_results()}} for downloading batch results without polling.
}

Other batch: 
\code{\link{batch_job}()},
\code{\link{call_batch_api}()},
\code{\link{cancel_batch}()},
\code{\link{check_batch_status}()},
\code{\link{claude_batch_job}()},
\code{\link{claude_cancel_batch}()},
\code{\link{claude_check_batch_status}()},
\code{\link{claude_download_batch_results}()},
\code{\link{claude_list_batches}()},
\code{\link{claude_poll_and_download}()},
\code{\link{download_results}()},
\code{\link{list_batches}()},
\code{\link{openai_batch_job}()},
\code{\link{openai_cancel_batch}()},
\code{\link{openai_check_batch_status}()},
\code{\link{openai_download_batch_results}()},
\code{\link{openai_list_batches}()},
\code{\link{openai_poll_and_download}()}
}
\concept{batch}
