\name{external_process_supervision}
\alias{new_external_process_supervision_policy}
\alias{print.PopgenVCFExternalProcessSupervisionPolicy}
\alias{run_supervised_external_command}
\title{Timeout and resource-aware external-process supervision}
\description{
Create deterministic supervision policies and run validated external commands
with resource admission, cooperative cancellation boundaries, timeout
enforcement, output capture, cleanup, and provenance metadata.
}
\usage{
new_external_process_supervision_policy(
  timeout_seconds = Inf,
  resource_policy = new_execution_resource_policy(),
  label = "default-process-supervision"
)

\method{print}{PopgenVCFExternalProcessSupervisionPolicy}(x, ...)

run_supervised_external_command(
  command,
  requirements = new_module_resource_requirements(),
  supervision_policy = new_external_process_supervision_policy(),
  cancellation_token = NULL
)
}
\arguments{
  \item{timeout_seconds}{Positive maximum elapsed process time in seconds, or \code{Inf}.}
  \item{resource_policy}{A \code{PopgenVCFExecutionResourcePolicy}.}
  \item{label}{Stable supervision-policy label.}
  \item{x}{A supervision policy.}
  \item{...}{Unused.}
  \item{command}{A validated \code{PopgenVCFExternalCommand}.}
  \item{requirements}{Declared process resource requirements.}
  \item{supervision_policy}{An external-process supervision policy.}
  \item{cancellation_token}{Optional execution cancellation token.}
}
\value{
\code{new_external_process_supervision_policy()} returns a validated policy.
\code{run_supervised_external_command()} returns a
\code{PopgenVCFExternalProcessResult} with supervision metadata.
}
\details{
Resource admission and pre-launch cancellation are evaluated before executable
resolution and process launch. Base R timeout enforcement reports timed-out
commands with the distinct \code{timed_out} state and exit status 124.
Cancellation is checked again after process completion. The normalized states
are \code{resource_unavailable}, \code{cancelled}, \code{launch_failed},
\code{timed_out}, \code{nonzero_exit}, and \code{success}.

Capture files are registered for cleanup before launch and removed on all return
paths. Cross-platform descendant-process termination and asynchronous
mid-process cancellation remain later supervision layers.
}
