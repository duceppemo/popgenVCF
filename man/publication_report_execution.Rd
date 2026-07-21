\name{publication_report_execution}
\alias{new_publication_report_renderer}
\alias{validate_publication_report_renderer}
\alias{quarto_publication_report_renderer}
\alias{execute_publication_report_plan}
\alias{validate_publication_report_execution}
\alias{publication_report_execution_report}
\title{Publication report renderer adapters and execution records}
\usage{
new_publication_report_renderer(id, version, formats, render)

validate_publication_report_renderer(renderer)

quarto_publication_report_renderer(
  executable = Sys.which("quarto"),
  version = NULL
)

execute_publication_report_plan(
  plan,
  manuscript,
  spec,
  renderer,
  output_dir,
  parameters = list()
)

validate_publication_report_execution(execution, plan, output_dir)

publication_report_execution_report(execution)
}
\arguments{
  \item{id}{Stable renderer identifier.}
  \item{version}{Stable renderer version.}
  \item{formats}{Supported output formats.}
  \item{render}{Backend function accepting source path, output path, format, and parameters.}
  \item{renderer}{A publication report renderer adapter.}
  \item{executable}{Path to the Quarto executable.}
  \item{plan}{A validated publication report plan.}
  \item{manuscript}{The originating canonical manuscript.}
  \item{spec}{The originating publication report specification.}
  \item{output_dir}{Destination directory for source and rendered outputs.}
  \item{parameters}{Stable named rendering parameters.}
  \item{execution}{A publication report execution record.}
}
\value{
Renderer constructors return validated adapters. Execution returns a fingerprinted
\code{PopgenVCFPublicationReportExecution}. Validators return \code{TRUE}
invisibly, and \code{publication_report_execution_report()} returns Markdown lines.
}
\description{
Defines replaceable renderer adapters and deterministic execution records for
HTML, PDF, and DOCX publication reports. Execution records preserve renderer
identity, source checksums, per-format outcomes, warnings, failures, and verified
output manifests.
}
