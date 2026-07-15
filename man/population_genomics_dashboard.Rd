\name{population_genomics_dashboard}
\alias{build_dashboard_summary}
\alias{calculate_scientific_quality}
\alias{write_population_genomics_dashboard}
\title{Interactive population-genomics dashboard}
\usage{
build_dashboard_summary(plan)

calculate_scientific_quality(plan)

write_population_genomics_dashboard(
  results,
  output_dir,
  title = "Population genomics dashboard",
  render = TRUE,
  include = NULL,
  exclude = NULL
)
}
\arguments{
  \item{plan}{A validated \code{PopgenVCFReportPlan}.}
  \item{results}{A named list of canonical core or ancestry results, or an existing report plan.}
  \item{output_dir}{Directory in which dashboard sources, exports, and rendered HTML are written.}
  \item{title}{Dashboard title.}
  \item{render}{Whether to render HTML with Quarto. Use \code{FALSE} to generate deterministic sources and the reproducibility bundle without Quarto.}
  \item{include, exclude}{Optional analysis identifiers to include or exclude.}
}
\value{
\code{build_dashboard_summary()} returns a one-row data table. \code{calculate_scientific_quality()} returns the overall score, component table, and validation records. \code{write_population_genomics_dashboard()} returns the report plan, summary, quality assessment, artifact manifest, and output paths.
}
\description{
Builds a dashboard-oriented Quarto report with overview metrics, searchable tables, optional Plotly PCA and ancestry views, transparent scientific-quality components, provenance exports, and a compressed reproducibility bundle. Optional metadata and publication artifacts are reported as not applicable when absent rather than being treated as failed checks.
}
\details{
The quality percentage is a weighted summary of canonical schema validation, explicit scientific validation, provenance completeness, and required artifact integrity. Every component, weight, applicability decision, fraction, and explanatory reason is exported to TSV and JSON. The score is a reporting aid and does not replace scientific review.
}
