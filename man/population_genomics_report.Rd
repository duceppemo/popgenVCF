\name{population_genomics_report}
\alias{build_population_genomics_report_plan}
\alias{validate_population_genomics_report_plan}
\alias{write_population_genomics_report}
\alias{print.PopgenVCFReportPlan}
\title{Build and render unified population-genomics reports}
\usage{
build_population_genomics_report_plan(
  results,
  title = "Population genomics report",
  include = NULL,
  exclude = NULL
)

validate_population_genomics_report_plan(x)

write_population_genomics_report(
  results,
  output_dir,
  title = "Population genomics report",
  formats = "html",
  render = TRUE,
  include = NULL,
  exclude = NULL
)

\method{print}{PopgenVCFReportPlan}(x, ...)
}
\arguments{
  \item{results}{A non-empty list of canonical core or ancestry result objects, or an existing \code{PopgenVCFReportPlan}.}
  \item{title}{Report title.}
  \item{include}{Optional analysis identifiers to include.}
  \item{exclude}{Optional analysis identifiers to omit.}
  \item{x}{A \code{PopgenVCFReportPlan}.}
  \item{output_dir}{Directory for report sources and rendered outputs.}
  \item{formats}{Output formats. Supported values are \code{"html"} and \code{"pdf"}.}
  \item{render}{Render through Quarto. Set to \code{FALSE} to create the deterministic plan, section manifest, and QMD source without requiring Quarto.}
  \item{...}{Unused.}
}
\value{
\code{build_population_genomics_report_plan()} returns a validated \code{PopgenVCFReportPlan}. \code{validate_population_genomics_report_plan()} returns \code{x} invisibly. \code{write_population_genomics_report()} returns a list containing the plan, artifact manifest, and output paths.
}
\description{
Discovers available canonical analyses, orders sections deterministically, records reproducibility metadata, writes a Quarto source document, and optionally renders HTML or PDF reports. Analyses not supplied are omitted automatically.
}
