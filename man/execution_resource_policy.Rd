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
  policy = new_execution_resource_policy()
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
to supervised external-process execution.
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
