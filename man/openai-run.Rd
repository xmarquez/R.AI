% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai-run.R
\name{openai_create_run}
\alias{openai_create_run}
\alias{openai_list_runs}
\alias{openai_get_run}
\alias{openai_cancel_run}
\alias{openai_create_thread_and_run}
\alias{openai_poll_run}
\title{Create, List, Retrieve, Update, Poll, or Cancel Runs in OpenAI}
\usage{
openai_create_run(
  thread_id,
  assistant_id,
  model = NULL,
  instructions = NULL,
  additional_instructions = NULL,
  additional_messages = NULL,
  tools = NULL,
  metadata = NULL,
  temperature = 1,
  top_p = 1,
  stream = NULL,
  max_prompt_tokens = NULL,
  max_completion_tokens = NULL,
  truncation_strategy = NULL,
  tool_choice = NULL,
  parallel_tool_calls = TRUE,
  response_format = NULL
)

openai_list_runs(
  thread_id,
  limit = 20,
  order = "desc",
  after = NULL,
  before = NULL
)

openai_get_run(thread_id, run_id)

openai_cancel_run(thread_id, run_id)

openai_create_thread_and_run(
  assistant_id,
  thread = NULL,
  model = NULL,
  instructions = NULL,
  tools = NULL,
  tool_resources = NULL,
  metadata = NULL,
  temperature = 1,
  top_p = 1,
  stream = NULL,
  max_prompt_tokens = NULL,
  max_completion_tokens = NULL,
  truncation_strategy = NULL,
  tool_choice = NULL,
  parallel_tool_calls = TRUE,
  response_format = NULL
)

openai_poll_run(thread_id, run_id, timeout = 5, quiet = FALSE)
}
\arguments{
\item{thread_id}{Character. Required. The ID of the thread the run belongs
to.}

\item{assistant_id}{Character. Required for run creation. The ID of the
assistant executing the run.}

\item{model}{Character or NULL. Optional. The model ID for the run.}

\item{instructions}{Character or NULL. Optional. Instructions for the
assistant's behavior during the run.}

\item{additional_instructions}{Character or NULL. Optional. Additional
instructions for the assistant.}

\item{additional_messages}{List or NULL. Optional. Messages to prepend to the
thread.}

\item{tools}{List or NULL. Optional. Tools available for the run.}

\item{metadata}{List or NULL. Optional. Metadata for the run.}

\item{temperature}{Numeric or NULL. Optional. Sampling temperature (default:
1).}

\item{top_p}{Numeric or NULL. Optional. Nucleus sampling value (default: 1).}

\item{stream}{Logical or NULL. Optional. Enables streaming during the run.}

\item{max_prompt_tokens}{Integer or NULL. Optional. Maximum prompt tokens.}

\item{max_completion_tokens}{Integer or NULL. Optional. Maximum completion
tokens.}

\item{truncation_strategy}{List or NULL. Optional. Strategy for truncating
thread context.}

\item{tool_choice}{List or Character or NULL. Optional. Controls tool usage.}

\item{parallel_tool_calls}{Logical. Optional. Defaults to TRUE. Enables
parallel tool calls.}

\item{response_format}{List or Character or NULL. Optional. Specifies the
output format.}

\item{limit}{Integer. Optional. A limit on the number of objects to return.
Defaults to 20, with a maximum of 100.}

\item{order}{Character. Optional. Sort order by the \code{created_at} timestamp.
Defaults to \code{"desc"} for descending order. Use \code{"asc"} for ascending order.}

\item{after}{Character. Optional. A cursor for pagination to fetch results
after this ID.}

\item{before}{Character. Optional. A cursor for pagination to fetch results
before this ID.}

\item{run_id}{Character. Optional. The ID of the run to retrieve, update, or
cancel.}

\item{thread}{Optional. A thread object created with
\code{\link[=openai_create_thread]{openai_create_thread()}}. Only used in \code{openai_create_thread_and_run()}}

\item{tool_resources}{A set of resources that are used by the assistant's
tools. The resources are specific to the type of tool. For example, the
\code{code_interpreter} tool requires a list of file IDs, while the
\code{file_search} tool requires a list of vector store IDs. See
\url{https://platform.openai.com/docs/api-reference/assistants/createAssistant}
for details.}

\item{timeout}{How long to wait before polling the API for the run status
again. Default is 5 seconds.}

\item{quiet}{Whether to print informative messages when polling the status of
a run.}
}
\value{
A list containing details of the created, retrieved, updated, or
cancelled run, depending on the function used.

For \code{openai_create_run} or \code{openai_create_thread_and_run}, the created run
object.

For \code{openai_list_runs}, a list of run objects in the thread.

For \code{openai_get_run} or \code{openai_poll_run}, the details of a specific run.

For \code{openai_cancel_run}, the updated run object with status set to
"cancelling" or similar.
}
\description{
These functions interact with the OpenAI \href{https://platform.openai.com/docs/api-reference/runs}{Runs API}. See the
documentation there for more details.
}
\seealso{
Other run: 
\code{\link{openai_list_run_steps}()},
\code{\link{openai_submit_tool_outputs}()}

Other openai: 
\code{\link{openai_batch_job}()},
\code{\link{openai_cancel_batch}()},
\code{\link{openai_check_batch_status}()},
\code{\link{openai_create_assistant}()},
\code{\link{openai_create_message}()},
\code{\link{openai_create_thread}()},
\code{\link{openai_create_vector_store}()},
\code{\link{openai_create_vector_store_file}()},
\code{\link{openai_create_vector_store_file_batch}()},
\code{\link{openai_download_batch_results}()},
\code{\link{openai_list_batches}()},
\code{\link{openai_list_run_steps}()},
\code{\link{openai_poll_and_download}()},
\code{\link{openai_submit_tool_outputs}()},
\code{\link{openai_upload_file}()}
}
\concept{openai}
\concept{run}
