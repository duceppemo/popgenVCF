\name{execution_resource_policy}
\alias{new_execution_resource_policy}
\alias{new_module_resource_requirements}
\alias{admit_execution_resources}
\title{Execution resource policies and deterministic admission}
\usage{
new_execution_resource_policy(
  threads = 1L,
  memory_mb = Inf,
  temp_mb = Inf,
  processes = 1L,
  label = "default-resource-policy"
)

new_module_resource_requirements(
  threads = 1L,
  memory_mb = 0,
  temp_mb = 0,
  processes = 1L
)

admit_execution_resources(
  requirements,
  policy = new_execution_resource_policy(),
  in_use = c(threads = 0, memory_mb = 0, temp_mb = 0, processes = 0)
)
}
\arguments{
\item{threads}{Maximum admitted threads for a policy, or required threads for a module.}

\item{memory_mb}{Maximum admitted memory in MiB for a policy, or required memory in MiB for a module.}

\item{temp_mb}{Maximum admitted temporary storage in MiB for a policy, or required temporary storage in MiB for a module.}

\item{processes}{Maximum admitted processes for a policy, or required processes for a module.}

\item{label}{Stable policy identifier recorded in execution metadata.}

\item{requirements}{A validated named numeric vector created by \code{new_module_resource_requirements()}.}

\item{policy}{A \code{PopgenVCFExecutionResourcePolicy} object.}

\item{in_use}{Resources already committed against \code{policy} by other
still-running admissions, as a named vector with the same names as
\code{requirements} (default: none). Subtracted from \code{policy}'s capacity
before checking \code{requirements}.}
}
\value{
\code{new_execution_resource_policy()} returns a validated
\code{PopgenVCFExecutionResourcePolicy} object.

\code{new_module_resource_requirements()} returns a validated named numeric
vector containing thread, memory, temporary-storage, and process requirements.

\code{admit_execution_resources()} returns a
\code{PopgenVCFExecutionAdmissionDecision} object describing whether the
requirements were admitted, which capacities were exceeded, and the evaluated
requirements and capacities.
}
\description{
Creates deterministic execution resource policies and module requirements, then
evaluates requirements against available capacity before execution begins.
Resource policies govern admission only; operating-system enforcement belongs
to supervised external-process execution. Each \code{admit_execution_resources()}
call is itself stateless: it only checks \code{requirements} against
\code{policy}'s fixed capacity, minus whatever \code{in_use} the caller passes.
Callers that admit multiple overlapping (not-yet-finalized) requests against
the same policy -- as \code{start_supervised_external_command()} does -- must
supply \code{in_use} themselves for those already-admitted requests to
actually count against capacity.
}
\examples{
policy <- new_execution_resource_policy(
  threads = 4L,
  memory_mb = 8192,
  temp_mb = 2048,
  processes = 2L,
  label = "workstation"
)

requirements <- new_module_resource_requirements(
  threads = 2L,
  memory_mb = 1024,
  temp_mb = 256,
  processes = 1L
)

decision <- admit_execution_resources(requirements, policy)
decision$admitted
}
