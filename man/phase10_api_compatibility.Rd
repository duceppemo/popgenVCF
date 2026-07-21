\name{phase10_api_compatibility}
\alias{compare_phase10_api_descriptors}
\alias{validate_phase10_api_compatibility}
\alias{phase10_api_compatibility_report}
\title{Phase 10 Public API Compatibility Contracts}
\usage{
compare_phase10_api_descriptors(baseline, candidate)

validate_phase10_api_compatibility(compatibility, allow_breaking = FALSE)

phase10_api_compatibility_report(compatibility)
}
\arguments{
  \item{baseline}{Baseline public API descriptor.}
  \item{candidate}{Candidate public API descriptor.}
  \item{compatibility}{Compatibility evidence returned by the comparator.}
  \item{allow_breaking}{Whether explicitly reviewed breaking drift may pass validation.}
}
\value{
The comparator returns deterministic compatibility evidence. The validator returns
\code{TRUE} invisibly. The report function returns Markdown lines.
}
\description{
Compares canonical Phase 10 public API descriptors, classifies compatible,
additive, deprecated, and breaking changes, verifies tamper-evident evidence,
and fails closed on unapproved breaking drift.
}
