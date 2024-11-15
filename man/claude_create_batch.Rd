% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/claude-batch.R
\name{claude_create_batch}
\alias{claude_create_batch}
\title{Create a Message Batch Using the Anthropic API}
\usage{
claude_create_batch(
  prompts,
  model = get_default_model("claude"),
  max_tokens = 300,
  max_retries = 3,
  pause_cap = 1200,
  quiet = FALSE
)
}
\arguments{
\item{prompts}{A list of prompts created by \code{\link[=build_prompts_from_files]{build_prompts_from_files()}}.}

\item{model}{The model to use to process the batch. Currently supports only
Claude 3 Haiku, Claude 3 Opus, and Claude 3.5 Sonnet. Defaults to
"claude-3-haiku-20240307" (i.e., \code{get_default_model("claude", type = "cheapest")}.}

\item{max_tokens}{The maximum number of output tokens for each item in the
batch.}

\item{max_retries}{An integer indicating the maximum number of retry attempts
in case of request failures. Defaults to 3.}

\item{pause_cap}{A numeric value representing the maximum pause duration (in
seconds) between retries. Defaults to 1200.}

\item{quiet}{A logical value indicating whether the function should suppress
messages during retries. Defaults to \code{FALSE}.}
}
\value{
A list containing the response details, including batch ID,
processing status, and the URL for retrieving results.
}
\description{
This function sends a batch of message creation requests to the Anthropic API
using the \href{https://docs.anthropic.com/en/api/creating-message-batches}{Message Batches endpoint}. It
allows you to process multiple requests at once, useful for scenarios
requiring bulk message completions.
}
