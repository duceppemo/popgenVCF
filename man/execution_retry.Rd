\name{execution_retry}
\alias{new_execution_retry_policy}
\alias{print.PopgenVCFExecutionRetryPolicy}
\alias{execute_analysis_plan_with_retries}
\alias{execute_analysis_registry_with_retries}
\title{Deterministic execution retry and recovery orchestration}
\description{
Create bounded retry policies and execute analysis plans or registries while
preserving validated outputs and retaining a complete module-attempt ledger.
}
\usage{
new_execution_retry_policy(
  max_attempts = 1L,
  retryable = function(module, error_message, attempt, ledger) FALSE,
  backoff_seconds = 0,
  label = "no-retry"
)

\method{print}{PopgenVCFExecutionRetryPolicy}(x, ...)

execute_analysis_plan_with_retries(
  analysis,
  context,
  registry,
  plan,
  engine = new_execution_engine(fail_fast = FALSE),
  retry_policy = new_execution_retry_policy()
)

execute_analysis_registry_with_retries(
  analysis,
  context,
  registry,
  selected = NULL,
  engine = new_execution_engine(fail_fast = FALSE),
  retry_policy = new_execution_retry_policy()
)
}
\arguments{
  \item{max_attempts}{Maximum number of executions for a module, including the initial attempt.}
  \item{retryable}{Function receiving \code{module}, \code{error_message}, \code{attempt}, and \code{ledger}; it must return one non-missing logical value.}
  \item{backoff_seconds}{One non-negative delay or one delay per retry.}
  \item{label}{Stable policy identifier recorded in execution metadata.}
  \item{x}{A retry-policy object.}
  \item{...}{Unused.}
  \item{analysis}{A \code{PopgenVCFAnalysis} object.}
  \item{context}{Runtime context shared by module runners.}
  \item{registry}{A \code{PopgenVCFRegistry} object.}
  \item{plan}{A \code{PopgenVCFExecutionPlan} object.}
  \item{engine}{Execution engine. Retry attempts are evaluated with fail-fast disabled.}
  \item{retry_policy}{A \code{PopgenVCFExecutionRetryPolicy} object.}
  \item{selected}{Optional module names.}
}
\value{
\code{new_execution_retry_policy()} returns a
\code{PopgenVCFExecutionRetryPolicy}. The execution functions return the normal
execution result augmented with \code{attempt_execution}, final attempt counts,
recovery flags, and retry metadata.
}
\details{
A retry proceeds only when every failure from the preceding attempt is
explicitly classified as retryable. Modules whose outputs passed the standard
validation contract are removed from subsequent plans. Failed or unvalidated
outputs are never reused.
}
