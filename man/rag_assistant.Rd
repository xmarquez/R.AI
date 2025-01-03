% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rag-assistant.R
\name{rag_assistant}
\alias{rag_assistant}
\title{Create a Retrieval-Augmented Generation (RAG) Assistant}
\usage{
rag_assistant(
  files,
  name,
  description,
  instructions,
  model,
  temperature,
  expires_after,
  convert_to_text,
  quiet,
  ...
)
}
\arguments{
\item{files}{A list or object containing file paths or file metadata.}

\item{name}{Character. Optional. The name of the assistant. Defaults to the
name(s) of the provided file collection if not supplied.}

\item{description}{Character. Optional. A description of the assistant.
Defaults to a generic description based on the provided name.}

\item{instructions}{Character. Optional. Instructions for the assistant's
behavior. Defaults to standard RAG instructions provided in the package,
which you can access by calling
\code{system.file("extdata/rag_standard_instructions.md", package = "R.AI")}.}

\item{model}{Character. Optional. The ID of the OpenAI model to use. Defaults
to \verb{gpt-4o}.}

\item{temperature}{Numeric. Optional. Sampling temperature for assistant
responses. Defaults to \code{0.2}.}

\item{expires_after}{Integer. Optional. Number of days after which the vector
store will expire. Defaults to \code{7}.}

\item{convert_to_text}{Logical. Optional. Whether to convert files to text
before uploading to OpenaAI. Defaults to \code{FALSE}. Use \code{TRUE} to minimize
the size of uploaded files and to convert non-standard file types to text
readable by OpenAI (e.g., if you are uploading your whole R package
codebase). This requires the
\href{https://cran.r-project.org/package=rtika}{rtika} package.}

\item{quiet}{Logical. Optional. If \code{TRUE}, suppresses CLI output. Defaults to
\code{FALSE}.}

\item{...}{Additional arguments passed to methods.}
}
\value{
An \code{openai_assistant} object representing the created assistant.
}
\description{
Creates an OpenAI assistant using the RAG pattern with an associated vector
store for file search. This function supports creating assistants from
various types of inputs, including files from a \href{https://zotero.org/}{Zotero}
collection.
}
