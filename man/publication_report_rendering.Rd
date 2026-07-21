\name{publication_report_rendering}
\alias{new_publication_report_spec}
\alias{validate_publication_report_spec}
\alias{new_publication_report_plan}
\alias{validate_publication_report_plan}
\alias{new_publication_report_output_manifest}
\alias{validate_publication_report_output_manifest}
\alias{publication_report_plan_report}
\title{Deterministic publication report rendering contracts}
\usage{
new_publication_report_spec(
  formats = "html",
  style = "generic",
  numbered_sections = TRUE,
  numbered_figures = TRUE,
  bibliography = TRUE,
  supplementary_index = TRUE
)

validate_publication_report_spec(spec)

new_publication_report_plan(
  manuscript,
  spec = new_publication_report_spec(),
  renderer_id = "quarto",
  renderer_version = "unspecified"
)

validate_publication_report_plan(plan, manuscript, spec)

new_publication_report_output_manifest(plan, output_dir, warnings = character())

validate_publication_report_output_manifest(manifest, plan, output_dir)

publication_report_plan_report(plan)
}
\arguments{
  \item{formats}{Output formats selected from \code{html}, \code{pdf}, and \code{docx}.}
  \item{style}{Stable rendering style identifier.}
  \item{numbered_sections}{Whether report sections are numbered.}
  \item{numbered_figures}{Whether figures and tables are numbered.}
  \item{bibliography}{Whether bibliography rendering is required.}
  \item{supplementary_index}{Whether a supplementary-material index is required.}
  \item{spec}{A publication report specification.}
  \item{manuscript}{A validated \code{PopgenVCFManuscript}.}
  \item{renderer_id}{Stable renderer or backend identity.}
  \item{renderer_version}{Stable renderer version.}
  \item{plan}{A publication report rendering plan.}
  \item{output_dir}{Directory containing rendered outputs.}
  \item{warnings}{Stable renderer warning messages.}
  \item{manifest}{A rendered-output manifest.}
}
\value{
Constructors return deterministic fingerprinted records. Validators return \code{TRUE} invisibly. \code{publication_report_plan_report()} returns Markdown lines.
}
\description{
Defines backend-independent rendering specifications, manuscript-bound plans, and checksum-verifiable output manifests for publication reports.
}
