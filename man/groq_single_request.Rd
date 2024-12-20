% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groq.R
\name{groq_single_request}
\alias{groq_single_request}
\title{Send a Single Request to the Groq API}
\usage{
groq_single_request(
  prompt,
  model,
  n_candidates = 1,
  max_retries = 10,
  temperature = 0.2,
  max_tokens = 300,
  json_mode = FALSE,
  content_extraction_fun,
  pause_cap = 1200,
  quiet = FALSE
)
}
\arguments{
\item{prompt}{A list containing the prompt message(s). Typically this is the
output of \code{\link[=prompt]{prompt()}}.}

\item{model}{A string specifying the model to use. Get available models with
\code{\link[=list_models]{list_models()}} or \code{\link[=get_available_models]{get_available_models()}}.}

\item{n_candidates}{The number of response candidates to generate. Defaults
to 1.}

\item{max_retries}{The maximum number of retry attempts in case of request
failures. Defaults to 10.}

\item{temperature}{A numeric value between 0 and 1 that controls the
randomness of the response. Higher values make the output more random.
Defaults to 0.2.}

\item{max_tokens}{The maximum number of tokens to include in the response.
Defaults to 300.}

\item{json_mode}{A logical value indicating whether the response should be
parsed as JSON. Defaults to \code{FALSE}. In the Groq API, setting \code{json_mode}
to \code{TRUE} should normally result in valid json, as noted in
\url{https://console.groq.com/docs/text-chat}.}

\item{content_extraction_fun}{A function to extract the desired content from
the API response. If not provided, a default extraction function is used
depending on the value of \code{json_mode}.}

\item{pause_cap}{A numeric value representing the maximum pause duration (in
seconds) between retries. Defaults to 1200.}

\item{quiet}{A logical value indicating whether the function should suppress
messages during retries. Defaults to \code{FALSE}.}
}
\value{
A \code{\link[=tibble]{tibble()}} with a \code{response} column and usage information.
}
\description{
This function sends a single prompt to the \href{https://console.groq.com/docs/overview}{Groq API} for chat. It
also handles retries and extracts the relevant response content.
}
\details{
See the Groq API documentation at
\url{https://console.groq.com/docs/overview}
}
\seealso{
Other single message: 
\code{\link{claude_single_request}()},
\code{\link{llamafile_single_request}()},
\code{\link{mistral_single_request}()},
\code{\link{openai_single_request}()}
}
\concept{groq}
\concept{single message}
