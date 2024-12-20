% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mistral-batch.R
\name{mistral_batch_job}
\alias{mistral_batch_job}
\title{Submit a Batch Job to the Mistral API}
\usage{
mistral_batch_job(
  prompts,
  model,
  max_tokens = 300,
  temperature = 0.2,
  quiet = FALSE
)
}
\arguments{
\item{prompts}{A \code{list} of prompts to process. Each prompt should follow the
Mistral API's expected format.}

\item{model}{A character string specifying the Mistral model to use. Refer to
the \href{https://docs.mistral.ai/capabilities/batch/}{Mistral API documentation} for available
models.}

\item{max_tokens}{An integer specifying the maximum number of output tokens
per prompt. Defaults to \code{300}.}

\item{temperature}{A number for the temperature per prompt. Defaults to \code{0.2}.}

\item{quiet}{A logical value indicating whether to suppress log messages.
Defaults to \code{FALSE}.}
}
\value{
A \code{list} containing the batch response details, with class
\code{"batch_mistral"}. Typical fields include:
\itemize{
\item \strong{\code{id}}: A unique identifier for the batch.
\item \strong{\code{status}}: The initial status of the batch (e.g., \code{"queued"} or \code{"in_progress"}).
\item Additional metadata as provided by the Mistral API.
}
}
\description{
This function submits a batch of prompts to the Mistral API for processing.
}
\seealso{
\itemize{
\item \code{\link[=batch_job]{batch_job()}} for the generic batch submission function.
\item \code{\link[=check_batch_status]{check_batch_status()}} for monitoring the status of a batch.
\item \code{\link[=download_results]{download_results()}} for retrieving the results of a batch.
}
}
