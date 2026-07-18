\name{execution_cancellation}
\alias{new_execution_cancellation_token}
\alias{print.PopgenVCFExecutionCancellationToken}
\alias{request_execution_cancellation}
\alias{validate_execution_cancellation_token}
\alias{execute_analysis_plan_with_cancellation}
\alias{execute_analysis_registry_with_cancellation}
\title{Deterministic cooperative execution cancellation}
\description{
Create mutable cancellation tokens and execute analysis plans or registries with
cooperative cancellation at deterministic module-launch boundaries.
}
\usage{
new_execution_cancellation_token(label = "execution-cancellation")

\method{print}{PopgenVCFExecutionCancellationToken}(x, ...)

request_execution_cancellation(token, reason = "Cancellation requested")

validate_execution_cancellation_token(token)

execute_analysis_plan_with_cancellation(
  analysis,
  context,
  registry,
  plan,
  cancellation_token = new_execution_cancellation_token(),
  engine = new_execution_engine(fail_fast = FALSE),
  retry_policy = new_execution_retry_policy(),
  checkpoint_path = NULL
)

execute_analysis_registry_with_cancellation(
  analysis,
  context,
  registry,
  selected = NULL,
  cancellation_token = new_execution_cancellation_token(),
  engine = new_execution_engine(fail_fast = FALSE),
  retry_policy = new_execution_retry_policy(),
  checkpoint_path = NULL
)
}
\arguments{
  \item{label}{Stable token identifier recorded in execution metadata.}
  \item{x}{A cancellation token.}
  \item{...}{Unused.}
  \item{token}{A \code{PopgenVCFExecutionCancellationToken}.}
  \item{reason}{Human-readable cancellation reason.}
  \item{analysis}{A \code{PopgenVCFAnalysis} object.}
  \item{context}{Runtime context shared by module runners.}
  \item{registry}{A \code{PopgenVCFRegistry} object.}
  \item{plan}{A \code{PopgenVCFExecutionPlan} object.}
  \item{cancellation_token}{Cancellation token checked before module launch.}
  \item{engine}{Execution engine.}
  \item{retry_policy}{Retry policy. Cancellation is always non-retryable.}
  \item{checkpoint_path}{Optional path for a checkpoint written after cancellation.}
  \item{selected}{Optional module names.}
}
\value{
The constructor returns a mutable cancellation token. The execution functions
return the standard execution result with cancellation metadata and, when
cancellation was requested, a validated execution checkpoint.
}
\details{
Cancellation is cooperative. It is observed before a module runner starts.
Already validated outputs remain valid, while a module that has not started
contributes no output or artifact. Cancelled attempts are never retried, even
when a caller supplies a broad retry predicate. The generated checkpoint can be
passed to \code{resume_analysis_execution()}.
}
