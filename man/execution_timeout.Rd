\name{execution_timeout}
\alias{new_execution_timeout_policy}
\alias{print.PopgenVCFExecutionTimeoutPolicy}
\alias{execute_analysis_plan_with_timeouts}
\alias{execute_analysis_registry_with_timeouts}
\title{Deterministic execution timeout policies}
\description{
Create explicit elapsed-time budgets and execute analysis plans or registries
with fail-closed timeout handling, dependency propagation, retry integration,
and auditable timeout metadata.
}
\usage{
new_execution_timeout_policy(
  default_seconds = Inf,
  module_seconds = numeric(),
  label = "no-timeout"
)

\method{print}{PopgenVCFExecutionTimeoutPolicy}(x, ...)

execute_analysis_plan_with_timeouts(
  analysis,
  context,
  registry,
  plan,
  engine = new_execution_engine(fail_fast = FALSE),
  timeout_policy = new_execution_timeout_policy(),
  retry_policy = new_execution_retry_policy()
)

execute_analysis_registry_with_timeouts(
  analysis,
  context,
  registry,
  selected = NULL,
  engine = new_execution_engine(fail_fast = FALSE),
  timeout_policy = new_execution_timeout_policy(),
  retry_policy = new_execution_retry_policy()
)
}
\arguments{
  \item{default_seconds}{Default elapsed-time budget in seconds. Use \code{Inf} for no timeout.}
  \item{module_seconds}{Optional uniquely named numeric vector of positive per-module budgets.}
  \item{label}{Stable policy identifier recorded in execution metadata.}
  \item{x}{A \code{PopgenVCFExecutionTimeoutPolicy} object.}
  \item{...}{Unused.}
  \item{analysis}{A \code{PopgenVCFAnalysis} object.}
  \item{context}{Runtime context shared by module runners.}
  \item{registry}{A \code{PopgenVCFRegistry} object.}
  \item{plan}{A \code{PopgenVCFExecutionPlan} object.}
  \item{engine}{Execution engine.}
  \item{timeout_policy}{A \code{PopgenVCFExecutionTimeoutPolicy} object.}
  \item{retry_policy}{A \code{PopgenVCFExecutionRetryPolicy} object.}
  \item{selected}{Optional module names.}
}
\value{
\code{new_execution_timeout_policy()} returns a
\code{PopgenVCFExecutionTimeoutPolicy}. The execution functions return the
normal execution result with timeout-aware final and attempt ledgers and timeout
policy metadata. The print method returns \code{x} invisibly.
}
\details{
Timeouts are disabled by default. A timed-out module contributes no accepted
result or artifact, is recorded as \code{timed_out}, and blocks dependent modules
unless a bounded retry later completes successfully and passes the normal
validation contracts. R elapsed-time limits are reliable for interruptible R
and native code; external processes require process-level supervision.
}
