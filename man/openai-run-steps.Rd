% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai-run-steps.R
\name{openai_list_run_steps}
\alias{openai_list_run_steps}
\alias{openai_get_run_step}
\title{List or Retrieve Run Steps in OpenAI}
\usage{
openai_list_run_steps(
  thread_id,
  run_id,
  limit = 20,
  order = "desc",
  after = NULL,
  before = NULL,
  include = NULL
)

openai_get_run_step(thread_id, run_id, step_id, include = NULL)
}
\arguments{
\item{thread_id}{Character. Required. The ID of the thread the run and run steps belong to.}

\item{run_id}{Character. Required. The ID of the run the run steps belong to.}

\item{limit}{Integer. Optional. Maximum number of run steps to retrieve. Defaults to 20.}

\item{order}{Character. Optional. Sort order by creation timestamp. One of "asc" or "desc" (default: "desc").}

\item{after}{Character. Optional. Cursor for pagination after a specific run step ID.}

\item{before}{Character. Optional. Cursor for pagination before a specific run step ID.}

\item{include}{List or NULL. Optional. A list of additional fields to include in the response.}

\item{step_id}{Character. Required for retrieving a specific run step. The ID of the run step to retrieve.}
}
\value{
For \code{openai_list_run_steps}, a list containing run step objects.

For \code{openai_get_run_step}, the details of a specific run step, including:
\itemize{
\item \code{id}: string. The identifier of the run step.
\item \code{status}: string. Status of the run step (\code{"in_progress"}, \code{"cancelled"}, \code{"failed"}, \code{"completed"}, or \code{"expired"}).
\item \code{step_details}: object. Details of the run step (e.g., message creation or tool calls).
\item \code{usage}: object. Token usage statistics for the run step.
}
}
\description{
These functions interact with the OpenAI \href{https://platform.openai.com/docs/api-reference/runs/steps}{Run Steps API}. See the documentation there for more details.
}
\seealso{
Other run: 
\code{\link{openai_create_run}()},
\code{\link{openai_submit_tool_outputs}()}

Other openai: 
\code{\link{openai_batch_job}()},
\code{\link{openai_cancel_batch}()},
\code{\link{openai_check_batch_status}()},
\code{\link{openai_create_assistant}()},
\code{\link{openai_create_message}()},
\code{\link{openai_create_run}()},
\code{\link{openai_create_thread}()},
\code{\link{openai_create_vector_store}()},
\code{\link{openai_create_vector_store_file}()},
\code{\link{openai_create_vector_store_file_batch}()},
\code{\link{openai_download_batch_results}()},
\code{\link{openai_list_batches}()},
\code{\link{openai_poll_and_download}()},
\code{\link{openai_submit_tool_outputs}()},
\code{\link{openai_upload_file}()}

Other assistants: 
\code{\link{openai_create_assistant}()},
\code{\link{openai_submit_tool_outputs}()}
}
\concept{assistants}
\concept{openai}
\concept{run}
