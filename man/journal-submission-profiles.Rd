\name{journal-submission-profiles}
\alias{new_journal_profile}
\alias{validate_journal_profile}
\alias{generic_journal_profile}
\alias{apply_journal_profile}
\title{Deterministic journal submission profiles}
\usage{
new_journal_profile(id = "generic", required_roles = c("manuscript_source"),
  optional_roles = character(), filenames = NULL,
  description = "Generic popgenVCF journal submission profile")

validate_journal_profile(x)

generic_journal_profile()

apply_journal_profile(plan, profile = generic_journal_profile())
}
\arguments{
  \item{id}{Stable profile identifier.}
  \item{required_roles, optional_roles}{Semantic submission roles.}
  \item{filenames}{Named role-to-filename character vector.}
  \item{description}{Human-readable description.}
  \item{x}{A journal profile.}
  \item{plan}{A submission-plan data frame with role and destination columns.}
  \item{profile}{A validated journal profile.}
}
\description{Creates, validates, and applies deterministic journal package naming and role requirements without modifying source manuscript artifacts.}
\value{A journal profile, \code{TRUE} invisibly, or a profile-aware data table.}
