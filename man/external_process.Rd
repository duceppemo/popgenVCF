\name{external_process}
\alias{new_external_command}
\alias{validate_external_command}
\alias{print.PopgenVCFExternalCommand}
\alias{run_external_command}
\alias{print.PopgenVCFExternalProcessResult}
\title{Canonical external-process execution}
\description{
Create validated external-command specifications and execute them through a
canonical runner that captures output, timing, exit state, and provenance.
}
\usage{
new_external_command(
  executable,
  args = character(),
  working_directory = getwd(),
  environment = character(),
  label = basename(executable)
)

validate_external_command(command)

\method{print}{PopgenVCFExternalCommand}(x, ...)

run_external_command(command)

\method{print}{PopgenVCFExternalProcessResult}(x, ...)
}
\arguments{
  \item{executable}{Executable name or path.}
  \item{args}{Character vector of command-line arguments.}
  \item{working_directory}{Existing working directory used for execution.}
  \item{environment}{Optional uniquely named character vector of environment overrides.}
  \item{label}{Stable command label recorded in provenance.}
  \item{command}{A \code{PopgenVCFExternalCommand} object.}
  \item{x}{A command specification or process result.}
  \item{...}{Unused.}
}
\value{
\code{new_external_command()} returns a validated
\code{PopgenVCFExternalCommand}. \code{run_external_command()} returns a
\code{PopgenVCFExternalProcessResult} containing the normalized status, exit
status, standard output, standard error, UTC timestamps, elapsed time, resolved
executable, command fingerprint, and any launch error.
}
\details{
Executable and argument fields remain separate rather than being represented as
a shell command string. Environment entries are sorted before fingerprinting so
semantically identical specifications have identical provenance.

The runner reports one of three initial states: \code{success},
\code{nonzero_exit}, or \code{launch_failed}. Non-zero output is retained for
diagnosis but does not represent accepted scientific output. Process-level
timeouts, cancellation, process-tree termination, and operating-system resource
enforcement are later Phase 8.8 supervision layers.
}
