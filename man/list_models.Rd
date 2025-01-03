% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list-models.R
\name{list_models}
\alias{list_models}
\alias{list_models.mistral}
\alias{list_models.openai}
\alias{list_models.groq}
\alias{list_models.cerebras}
\alias{list_models.gemini}
\alias{list_models.ollama}
\alias{list_models.claude}
\alias{list_models.llama_cpp}
\alias{list_models.cohere}
\alias{list_models.voyage}
\alias{list_models.deepseek}
\alias{list_models.qwen}
\alias{list_models.all}
\title{List Available Models}
\usage{
list_models(api, ...)

\method{list_models}{mistral}(api, ...)

\method{list_models}{openai}(api, ...)

\method{list_models}{groq}(api, ...)

\method{list_models}{cerebras}(api, ...)

\method{list_models}{gemini}(api, ...)

\method{list_models}{ollama}(api, ...)

\method{list_models}{claude}(api, ...)

\method{list_models}{llama_cpp}(api, local_dir = "models", ...)

\method{list_models}{cohere}(api, ...)

\method{list_models}{voyage}(api, ...)

\method{list_models}{deepseek}(api, ...)

\method{list_models}{qwen}(api, ...)

\method{list_models}{all}(api, mode = "chat", ...)
}
\arguments{
\item{api}{\code{character}. Specifies the source to query. Must be one of:
\itemize{
\item \code{"mistral"}
\item \code{"openai"}
\item \code{"groq"}
\item \code{"cerebras"}
\item \code{"cohere"}
\item \code{"deepseek"}
\item \code{"gemini"}
\item \code{"ollama"}
\item \code{"claude"}
\item \code{"llama_cpp"}
}}

\item{...}{Other parameters passed on to methods}

\item{local_dir}{\code{character}. Path to the local directory containing models
(used only when \code{api = "llama_cpp"}). Defaults to \code{"models"}.}

\item{mode}{If you set \code{api = "all"}, \code{mode} can be any of \code{"chat"},
\code{"embed"}, \code{"completion"}, \code{"rerank"}}
}
\value{
A tidy data frame (\code{tibble}) when querying remote APIs, or a
character vector of filenames for \code{"llama_cpp"}. Ollama returns a tibble of
local models if you choose \code{"ollama"}.
}
\description{
A generic function that fetches model information from various sources
(Mistral, OpenAI, Groq, Cerebras, Cohere, Gemini, Claude, Ollama, or a local
directory for Llama.cpp compatible files). You specify which backend you want
by passing a string (e.g., \code{"mistral"}, \code{"openai"}, \code{"gemini"}, \code{"claude"},
\code{"llamafile"}). Internally, the class of the string is set so that
\code{UseMethod("list_models")} can dispatch to the corresponding S3 method.
\code{"all"} returns all models.
}
\section{Environment Variables}{
 For remote API queries, ensure the relevant
environment variable is set:
\itemize{
\item \code{MISTRAL_API_KEY} (for \code{"mistral"})
\item \code{OPENAI_API_KEY} (for \code{"openai"})
\item \code{GROQ_API_KEY} (for \code{"groq"})
\item \code{CEREBRAS_API_KEY} (for \code{"cerebras"})
\item \code{GEMINI_API_KEY} (for \code{"gemini"})
\item \code{ANTHROPIC_API_KEY} (for \code{"claude"})
\item \code{COHERE_API_KEY} (for \code{"cohere"})
\item \code{DEEPSEEK_API_KEY} (for \code{"deepseek"})
\item \code{QWEN_API_KEY} (for \code{"qwen"} - or Alibaba)
}
}

\examples{
\dontrun{
## List OpenAI models:
list_models("openai")

## List Claude models:
list_models("claude")

## List local llama files:
list_models("llamafile", local_dir = "my_local_models")
}

}
\seealso{
Other model utilities: 
\code{\link{get_available_apis}()},
\code{\link{get_available_models}()},
\code{\link{get_default_model}()},
\code{\link{is_api_key_available}()}
}
\concept{model utilities}
