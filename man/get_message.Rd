% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-message-methods.R
\name{get_message}
\alias{get_message}
\alias{get_message.openai_chat}
\alias{get_message.cerebras_chat}
\alias{get_message.claude_chat}
\alias{get_message.groq_chat}
\alias{get_message.llama_cpp_chat}
\alias{get_message.mistral_chat}
\alias{get_message.gemini_chat}
\alias{get_message.cohere_chat}
\alias{get_message.deepseek_chat}
\alias{get_message.qwen_chat}
\alias{get_message.default}
\title{Extract Message Objects from a Chat Response}
\usage{
get_message(response)

\method{get_message}{openai_chat}(response)

\method{get_message}{cerebras_chat}(response)

\method{get_message}{claude_chat}(response)

\method{get_message}{groq_chat}(response)

\method{get_message}{llama_cpp_chat}(response)

\method{get_message}{mistral_chat}(response)

\method{get_message}{gemini_chat}(response)

\method{get_message}{cohere_chat}(response)

\method{get_message}{deepseek_chat}(response)

\method{get_message}{qwen_chat}(response)

\method{get_message}{default}(response)
}
\arguments{
\item{response}{A chat response object from a function like
\code{\link{chat.openai_list}} or similar. Must be an object with an S3
class such as \code{"openai_chat"}, \code{"cerebras_chat"}, \code{"claude_chat"}, etc.}
}
\value{
A list (or list-of-lists) representing the "message" component. For
backends that produce a single choice, this is typically a single list with
S3 class \code{c("{backend}_list", "list")}. For backends that produce
multiple choices, you may get a list of lists.
}
\description{
A generic function to retrieve the \strong{"message"} portion from various LLM
(Large Language Model) chat response objects (OpenAI, Cerebras, Claude, etc.).
Depending on the backend, the response may contain one or multiple "choices"
or "candidates." For most backends, this function extracts only the \emph{first}
choice by default (except where otherwise noted).
}
\section{Behavior by Backend}{

\describe{
\item{OpenAI (\code{openai_chat})}{Extracts the \code{message} from the \emph{first}
element in \code{response$choices}, wraps it in a one-element list with
class \code{c("openai_list", "list")}.}

\item{Cerebras (\code{cerebras_chat})}{Similar to OpenAI, extracts the
\code{message} from the first choice as a one-element list with class
\code{c("cerebras_list", "list")}.}

\item{Claude (\code{claude_chat})}{Returns \code{response$content} \emph{as-is},
assigning class \code{c("claude_list", "list")}. This may contain one or
more message-like items.}

\item{Groq (\code{groq_chat})}{Maps over all \code{choices}, extracting the
\code{message} from each, creating a list of \code{"groq_list"} objects.
So if there are multiple choices, you get multiple messages.}

\item{Llama.CPP (\code{llama_cpp_chat})}{Extracts the \code{message} from
\code{response$choices[[1]]} and returns it as a single-element list
(\code{c("llama_cpp_list", "list")}).}

\item{Mistral (\code{mistral_chat})}{Extracts the first choice’s message,
ignoring null/empty fields, returning a single-element list with class
\code{"mistral_list"}.}

\item{Gemini (\code{gemini_chat})}{Extracts the \code{content} from
\code{response$candidates[[1]]}, returning a list with class
\code{"gemini_list"}. For advanced usage with multiple candidates,
you may need to modify this code or manually parse \code{response$candidates}
yourself.}
}
}

\examples{
\dontrun{
# Example for an OpenAI response (first choice only):
resp_openai <- chat.openai_list(messages, model="gpt-3.5-turbo")
msg_oa <- get_message(resp_openai)
str(msg_oa)  # -> List of length 1, class c("openai_list", "list")

# Example for Groq which may have multiple choices:
resp_groq <- chat.groq_list(messages, model="groq-model")
msgs_groq <- get_message(resp_groq)
str(msgs_groq)  # -> List of "groq_list" objects, one per choice

# Example for Gemini which fetches the first candidate's content:
resp_gemini <- chat.gemini_list(messages, model="gemini-v1")
msg_gem <- get_message(resp_gemini)
str(msg_gem) # -> c("gemini_list", "list")
}

}
