% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-utilities.R
\name{get_available_apis}
\alias{get_available_apis}
\title{Check Availability of API Keys for All APIs}
\usage{
get_available_apis(mode = "chat")
}
\arguments{
\item{mode}{Can be "chat", "embed", "rerank", or "completion". Default is "chat".}
}
\value{
A data frame with columns \code{api}, \code{key_available}, \code{key_needed}, and
\code{available}. The \code{api} column lists the APIs, and the \code{key_available}
column indicates whether an API key is available for each API. The
\code{key_needed} column lists whether an API key is needed to use.
}
\description{
This function returns a data frame that includes the APIs from the internal
\code{models_df} data and whether an API key is available for each API.
}
\details{
The function checks the environment variables for the presence of an
API key corresponding to each API in the \code{models_df} data frame. The
environment variables are expected to follow the pattern
"<API_NAME>_API_KEY", where <API_NAME> is the uppercase version of the API
name. (Except for the Claude API, where the environment variable is
"ANTHROPIC_API_KEY").
}
\examples{
# Get the availability of API keys for all APIs
get_available_apis()

}
\seealso{
\code{\link[=get_available_models]{get_available_models()}} for retrieving all available models for an
API.

Other model utilities: 
\code{\link{get_available_models}()},
\code{\link{get_default_model}()},
\code{\link{is_api_key_available}()},
\code{\link{list_models}()}
}
\concept{model utilities}
