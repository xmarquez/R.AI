% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-embeddings.R
\name{call_embedding_api}
\alias{call_embedding_api}
\title{Call Embedding API}
\usage{
call_embedding_api(texts, model, api, ...)
}
\arguments{
\item{texts}{\code{character} A character vector containing the input texts to
process. Must not be empty.}

\item{model}{\code{character} A string specifying the embedding model to use. The
model must be supported by the chosen API. Use \code{get_available_models(mode = "embedding")} to identify the relevant models.}

\item{api}{\code{character} Specifies the API or method for embedding generation.
Supported values are:
\itemize{
\item \code{"mistral"} - Dispatches to the \href{https://docs.mistral.ai/api/#tag/agents/operation/agents_completion_v1_agents_completions_post}{Mistral embedding API}.
\item \code{"openai"} - Dispatches to the \href{https://platform.openai.com/docs/api-reference/embeddings}{OpenAI embedding API}.
\item \code{"cohere"} - Dispatches to the \href{https://docs.cohere.com/reference/embed}{Cohere embedding API}.
\item \code{"voyage"} - Dispatches to the \href{https://docs.voyageai.com/reference/embeddings-api}{Voyage embedding API}.
\item \code{"llamafile"} - Dispatches to a local LlamaFile instance.
}}

\item{...}{Additional arguments passed to API-specific implementations. See
details for supported options.}
}
\value{
A \code{tibble} containing the embeddings and metadata, with the following
columns:
\itemize{
\item \code{id}: Identifier for each input text.
\item \code{text_set}: Hash representing the batch of texts processed together.
\item \code{embedding}: A list-column containing the embedding vector for each input text.
}
}
\description{
A generic function to retrieve text embeddings by sending requests to various
APIs or a local LlamaFile instance.
}
\details{
The function dispatches requests to API-specific methods based on
the value of \code{api}.

Additional arguments for specific APIs include:
\itemize{
\item \strong{LlamaFile}:
\itemize{
\item \code{llamafile_path}: Path to the LlamaFile model.
\item \code{quiet}: Logical, suppresses logging (default: \code{FALSE}).
}
\item \strong{Cohere}:
\itemize{
\item \code{input_type}: Type of input for embeddings (e.g., \code{"search_document"}).
\item \code{embedding_types}: List of desired embedding types (e.g., \code{"float"}).
\item \code{quiet}: Logical, suppresses logging (default: \code{FALSE}).
}
\item \strong{OpenAI}:
\itemize{
\item \code{quiet}: Logical, suppresses logging (default: \code{FALSE}).
}
\item \strong{Mistral}:
\itemize{
\item \code{quiet}: Logical, suppresses logging (default: \code{FALSE}).
}
\item \strong{Voyage}:
\itemize{
\item \code{input_type}: Specifies whether the input is \code{"query"}, \code{"document"}, or \code{NULL} (default: \code{"document"}).
\item \code{truncation}: Logical, whether to truncate input texts (default: \code{TRUE}).
\item \code{output_dimension}: Integer, specifies the output embedding dimension (optional).
\item \code{output_dtype}: Data type for embeddings (e.g., \code{"float"}, \code{"int8"}, etc., default: \code{"float"}).
\item \code{encoding_format}: Format for encoding embeddings (e.g., \code{"base64"} or \code{NULL} for raw lists).
\item \code{quiet}: Logical, suppresses logging (default: \code{FALSE}).
}
}
}
\note{
Note that your API keys must be stored in an environment variable so
they are accessible via \code{\link[=Sys.getenv]{Sys.getenv()}}.
}
\seealso{
\code{\link[=get_available_models]{get_available_models()}} for retrieving available models for
specific APIs.
}
\concept{embeddings}
