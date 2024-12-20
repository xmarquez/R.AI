% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prompt_builders.R
\name{build_prompts_from_files}
\alias{build_prompts_from_files}
\title{Build Prompts from Files}
\usage{
build_prompts_from_files(files, roles = "user", api, data)
}
\arguments{
\item{files}{A character vector of file paths containing prompt content.
All files must exist.}

\item{roles}{A character vector specifying the roles for each file.
Must be one or more of "system", "user", or "assistant". Default is "user".
If multiple roles are provided, the length must match the number of files.
Only one "system" role is allowed.}

\item{api}{A string specifying the API to use. Must be one of "groq", "openai",
"claude", "mistral", "llamafile", or "gemini".}

\item{data}{An optional data frame containing variables to be
interpolated into the prompts using \code{stringr::str_glue_data()}.}
}
\value{
A list of combined prompts, with class matching the specified API and
names corresponding to the row names of the input data.
}
\description{
This function constructs prompts for various language model APIs by reading
from specified files and combining them according to the chosen API's
requirements.
}
\details{
The function supports different prompt structures for various APIs:
\itemize{
\item For OpenAI, Groq, and Claude: Can handle "system", "user", and "assistant" roles.
\item For Gemini: Only uses "user" role prompts.
}

Input validation ensures that:
\itemize{
\item All specified files exist.
\item Roles are valid and correctly specified (e.g., only one "system" role).
\item The API choice is valid.
\item If provided, the data argument is a data frame.
}
}
\seealso{
\code{\link[=call_api]{call_api()}} for using the generated prompts to make API calls.

Other prompts: 
\code{\link{prompt}()},
\code{\link{prompt_list}()}
}
\concept{prompts}
