\name{execution_checkpoint}
\alias{execution_checkpoint}
\alias{new_execution_checkpoint}
\alias{validate_execution_checkpoint}
\alias{write_execution_checkpoint}
\alias{read_execution_checkpoint}
\alias{resume_analysis_execution}
\title{Deterministic analysis execution checkpoints}
\description{
Capture checksum-verified analysis execution state and resume unfinished modules
without rerunning modules already recorded as successful.
}
\usage{
new_execution_checkpoint(executed, registry)

validate_execution_checkpoint(checkpoint, registry = NULL)

write_execution_checkpoint(checkpoint, path, overwrite = FALSE)

read_execution_checkpoint(path, registry = NULL)

resume_analysis_execution(
  checkpoint,
  registry,
  engine = new_execution_engine()
)
}
\arguments{
  \item{executed}{A complete result returned by \code{execute_analysis_plan()} or \code{execute_analysis_registry()}.}
  \item{registry}{The analysis registry used for execution or compatibility validation.}
  \item{checkpoint}{A \code{PopgenVCFExecutionCheckpoint} object.}
  \item{path}{Destination or source checkpoint \code{.rds} path.}
  \item{overwrite}{Whether existing checkpoint and checksum files may be replaced.}
  \item{engine}{Execution engine used for unfinished modules.}
}
\value{
\code{new_execution_checkpoint()} and \code{read_execution_checkpoint()} return a
\code{PopgenVCFExecutionCheckpoint}. \code{validate_execution_checkpoint()} and
\code{write_execution_checkpoint()} return invisibly. \code{resume_analysis_execution()}
returns a regular execution result with a combined ledger containing a
\code{checkpoint_reused} column.
}
\details{
A checkpoint binds the planned module order, dependency waves, module contract
versions, declared outputs, analysis state, runtime context, artifact manifest,
and execution ledger to a SHA-256 identity. The serialized RDS file has a
separate SHA-256 sidecar and is installed atomically.

Resume validates the checkpoint against the current registry and rejects plan or
contract drift. Only modules recorded as successful are reused. Failed, blocked,
or otherwise unfinished modules are rerun through the normal execution engine
and remain subject to result, artifact, and dependency validation.
}
