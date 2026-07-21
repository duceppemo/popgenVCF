\name{render_public_report}
\alias{render_public_report}
\title{Render a Canonical Public Population-Genomics Report}
\usage{
render_public_report(
  request,
  report,
  output_dir,
  title = "Population genomics report",
  formats = "html",
  render = FALSE,
  include = NULL,
  exclude = NULL
)
}
\arguments{
  \item{request}{A canonical public API request for \code{report.render}.}
  \item{report}{A \code{PopgenVCFReportPlan} or named list of canonical results.}
  \item{output_dir}{Internal report output directory.}
  \item{title}{Report title used when constructing a plan from results.}
  \item{formats}{Requested output formats: \code{html} and/or \code{pdf}.}
  \item{render}{Whether to invoke Quarto. When false, deterministic sources and manifests are written.}
  \item{include, exclude}{Optional analysis filters used when constructing a report plan.}
}
\value{A validated \code{PopgenVCFPublicAPIResponse}.}
\description{
Delegates report planning and generation to the existing population-genomics
report engine and translates the resulting report and artifact identities into
the stable Phase 10 public API. Output paths, timestamps, platform metadata,
Quarto process details, and renderer state remain internal.
}
