% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-batch.R
\name{call_batch_api}
\alias{call_batch_api}
\title{Call a Language Model Batch API}
\usage{
call_batch_api(prompts, model, ...)
}
\arguments{
\item{prompts}{A \code{list} of prompts to send to the API. The class of this
object determines the API used:
\itemize{
\item \code{"claude"}: Uses the Claude API (\href{https://docs.anthropic.com/en/api/creating-message-batches}{Anthropic}).
\item \code{"openai"}: Uses the OpenAI API (\href{https://platform.openai.com/docs/api-reference/batch}{OpenAI}).
\item \code{"mistral"}: Uses the Mistral API (\href{https://docs.mistral.ai/capabilities/batch/}{Mistral}).
}}

\item{model}{A character string specifying the model to use. Refer to the
API-specific documentation for available models.}

\item{...}{Additional arguments passed to API-specific methods. Common
options include:
\itemize{
\item \code{max_tokens}: Maximum number of output tokens (default: 300).
\item \code{quiet}: Suppresses logging messages during execution (default: \code{FALSE}).
\item API-specific parameters.
}}
}
\value{
A \code{list} containing API-specific response details, including:
\itemize{
\item \code{id}: The batch ID (if available).
\item \code{processing_status}: Current processing status.
\item \code{results_url}: URL to retrieve batch results.
}
}
\description{
This function sends batch requests to supported language model APIs,
including OpenAI, Claude (Anthropic), and Mistral. It dispatches requests to
the appropriate method based on the class of \code{prompts}.
}
\details{
The function dispatches to one of the following methods:
\itemize{
\item \code{\link[=call_batch_api.claude]{call_batch_api.claude()}}
\item \code{\link[=call_batch_api.openai]{call_batch_api.openai()}}
\item \code{\link[=call_batch_api.mistral]{call_batch_api.mistral()}}
}

For additional details, refer to the relevant API documentation:
\itemize{
\item Claude: \href{https://docs.anthropic.com/en/api/creating-message-batches}{Anthropic Message Batch API}
\item OpenAI: \href{https://platform.openai.com/docs/api-reference/batch}{OpenAI Batch API}
\item Mistral: \href{https://docs.mistral.ai/capabilities/batch/}{Mistral Batch API}
}
}
\seealso{
\itemize{
\item \code{\link[=call_api]{call_api()}} for single-request API calls.
\item \code{\link[=build_prompts_from_files]{build_prompts_from_files()}} for prompt creation utilities.
}
}
