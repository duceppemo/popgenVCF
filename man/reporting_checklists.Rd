\name{reporting_checklists}
\alias{new_reporting_checklist}
\alias{validate_reporting_checklist}
\alias{generic_reporting_checklist}
\alias{validate_reporting_checklist_responses}
\alias{render_reporting_checklist}
\alias{write_reporting_checklist}
\title{Deterministic reporting checklists}
\usage{
new_reporting_checklist(
  id,
  items,
  version = "1.0",
  title = id,
  organization = "popgenVCF",
  status = c("generic", "verified", "deprecated"),
  source_url = NA_character_,
  source_date = NA_character_,
  description = "Deterministic reporting checklist"
)

validate_reporting_checklist(x)

generic_reporting_checklist()

validate_reporting_checklist_responses(checklist, responses = NULL, strict = FALSE)

render_reporting_checklist(checklist, responses = NULL)

write_reporting_checklist(
  checklist,
  directory,
  responses = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{id}{Stable checklist identifier.}
\item{items}{Data frame containing \code{item_id}, \code{category}, \code{label}, \code{requirement}, and \code{guidance}.}
\item{version}{Checklist version.}
\item{title}{Human-readable checklist title.}
\item{organization}{Issuing organization or generic source.}
\item{status}{Checklist status: \code{generic}, \code{verified}, or \code{deprecated}.}
\item{source_url, source_date}{Source and verification date for verified checklists.}
\item{description}{Human-readable description.}
\item{x}{A \code{PopgenVCFReportingChecklist} or written checklist directory.}
\item{checklist}{A validated \code{PopgenVCFReportingChecklist}.}
\item{responses}{Data frame with \code{item_id}, \code{response}, and optional \code{evidence} and \code{notes}.}
\item{strict}{Whether incomplete required items raise an error.}
\item{directory}{Parent output directory.}
\item{overwrite}{Whether an existing output directory may be replaced.}
}
\value{
\code{new_reporting_checklist()} and \code{generic_reporting_checklist()} return a validated \code{PopgenVCFReportingChecklist}. \code{validate_reporting_checklist()} returns \code{TRUE} invisibly. \code{validate_reporting_checklist_responses()} returns a deterministic \code{PopgenVCFReportingChecklistReport} data table. \code{render_reporting_checklist()} returns Markdown lines. \code{write_reporting_checklist()} returns the normalized output directory invisibly.
}
\description{
Create, validate, evaluate, render, and write immutable reporting checklists. Checklist completion is based only on explicit author-supplied responses and evidence; missing facts are never inferred.
}
