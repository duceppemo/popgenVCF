\name{external_process_async}
\alias{start_supervised_external_command}
\alias{poll_supervised_external_command}
\alias{read_supervised_external_output}
\alias{finalize_supervised_external_command}
\alias{validate_external_process_handle}
\alias{print.PopgenVCFExternalProcessHandle}
\title{Asynchronous supervised external-process control}
\description{
Start validated external commands without blocking the R session, poll their
lifecycle and output, observe cancellation or timeout conditions while running,
and finalize them into the standard external-process result contract.
}
\usage{
start_supervised_external_command(
  command,
  requirements = new_module_resource_requirements(),
  supervision_policy = new_external_process_supervision_policy(),
  cancellation_token = NULL,
  termination_grace_seconds = 1
)

poll_supervised_external_command(handle, timeout_milliseconds = 0)

read_supervised_external_output(handle)

finalize_supervised_external_command(
  handle,
  wait = TRUE,
  poll_interval_milliseconds = 50
)

validate_external_process_handle(handle)

\method{print}{PopgenVCFExternalProcessHandle}(x, ...)
}
\arguments{
  \item{command}{A validated \code{PopgenVCFExternalCommand}.}
  \item{requirements}{Declared process resource requirements.}
  \item{supervision_policy}{An external-process supervision policy.}
  \item{cancellation_token}{Optional execution cancellation token.}
  \item{termination_grace_seconds}{Non-negative graceful-termination interval before forced tree cleanup.}
  \item{handle}{A \code{PopgenVCFExternalProcessHandle}.}
  \item{timeout_milliseconds}{Maximum I/O polling wait for one poll operation.}
  \item{wait}{Whether finalization should poll until terminal.}
  \item{poll_interval_milliseconds}{Polling interval used by blocking finalization.}
  \item{x}{A process handle.}
  \item{...}{Unused.}
}
\value{
\code{start_supervised_external_command()} returns a live or already-terminal
\code{PopgenVCFExternalProcessHandle}. Polling and validation return the handle
invisibly. \code{read_supervised_external_output()} returns accumulated stdout
and stderr. \code{finalize_supervised_external_command()} returns a
\code{PopgenVCFExternalProcessResult}.
}
\details{
Resource admission, pre-launch cancellation, and executable resolution occur
before process creation. Live commands are launched through
\code{processx::process} with process-tree cleanup enabled. Polling drains only
new stdout and stderr, checks the existing cancellation token, enforces the
supervision timeout, and finalizes naturally completed processes.

Cancellation and timeout initiate graceful interruption. If the process remains
alive after the configured grace interval, the full process tree is killed.
Ordered lifecycle events record process start, termination requests, graceful
attempts, forced cleanup, and finalization. The existing synchronous execution
API is unchanged.
}
