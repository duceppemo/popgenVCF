\name{execution_engine}
\alias{execution_engine}
\alias{new_execution_engine}
\alias{print.PopgenVCFExecutionEngine}
\alias{plan_analysis_execution}
\alias{print.PopgenVCFExecutionPlan}
\alias{execute_analysis_plan}
\title{Unified deterministic analysis execution engine}
\description{
Compile a dependency-aware module registry into deterministic execution waves,
apply worker and resource-class limits, and execute the resulting plan through
serial, forked, or portable PSOCK workers. Modules are serial by default and
must explicitly declare themselves parallel-safe before they can share a batch.
}
\usage{
new_execution_engine(
  workers = 1L,
  backend = c("sequential", "multisession", "multicore"),
  resource_limits = NULL,
  fail_fast = TRUE
)

\method{print}{PopgenVCFExecutionEngine}(x, ...)

plan_analysis_execution(registry, config, selected = NULL)

\method{print}{PopgenVCFExecutionPlan}(x, ...)

execute_analysis_plan(
  analysis,
  context,
  registry,
  plan,
  engine = new_execution_engine()
)
}
\arguments{
  \item{workers}{Maximum number of concurrently executing modules.}
  \item{backend}{Execution backend. \code{"sequential"} executes in the calling process, \code{"multisession"} uses portable PSOCK workers on Linux, macOS, and Windows, and \code{"multicore"} uses forked workers on non-Windows systems.}
  \item{resource_limits}{Named positive integer vector with limits for \code{light}, \code{standard}, \code{heavy}, and \code{external} modules.}
  \item{fail_fast}{Whether execution stops after the first module failure. When false, independent modules continue and descendants of failed modules are recorded as blocked without execution.}
  \item{x}{An execution engine or execution plan.}
  \item{...}{Unused.}
  \item{registry}{A \code{PopgenVCFRegistry} object.}
  \item{config}{Validated popgenVCF configuration.}
  \item{selected}{Optional module names.}
  \item{analysis}{A \code{PopgenVCFAnalysis} object.}
  \item{context}{Runtime context shared by module runners.}
  \item{plan}{A \code{PopgenVCFExecutionPlan} object.}
  \item{engine}{A \code{PopgenVCFExecutionEngine} object.}
}
\value{
\code{new_execution_engine()} returns a \code{PopgenVCFExecutionEngine}.
\code{plan_analysis_execution()} returns a \code{PopgenVCFExecutionPlan}.
\code{execute_analysis_plan()} returns updated analysis and context state,
successful execution order, plan, artifact manifest, engine metadata, and a
module-level execution ledger. Print methods return their input invisibly.
}
\details{
Execution plans use topological dependency waves. Within each wave, only
modules whose contracts set \code{parallel_safe = TRUE} may share a batch.
Resource-class limits can further split a wave into smaller batches. A serial
module executes alone. A parallel-safe module must leave the shared context
unchanged; this invariant is validated before its results are merged.

The \code{"multisession"} backend creates portable PSOCK workers and initializes
a deterministic L'Ecuyer-CMRG random-number stream. Worker completion timing is
recorded, but validation and result merging always follow the planned module
order. Consequently, completion timing cannot reorder accepted outputs,
artifacts, validation records, or execution messages.

The execution ledger has one deterministic row per planned module and records
its wave, batch, requirements, resource class, parallel-safety declaration,
status, elapsed time, error message, blocking prerequisites, dispatch sequence,
completion sequence, merge sequence, and worker process identifier. Valid
terminal states are \code{success}, \code{failed}, and \code{blocked}. A blocked
module is never invoked and therefore cannot contribute results or artifacts.
}