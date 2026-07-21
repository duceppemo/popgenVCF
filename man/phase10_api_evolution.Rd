\name{phase10_api_evolution}
\alias{phase10_api_evolution_policy}
\alias{new_phase10_migration_guidance}
\alias{new_phase10_api_migration_plan}
\alias{validate_phase10_api_migration_plan}
\alias{phase10_api_migration_report}
\title{Phase 10 Public API Evolution and Migration Planning}
\description{
Create deterministic API evolution policies, operation-level migration guidance,
fingerprinted migration plans, validation evidence, and Markdown reports.
}
\usage{
phase10_api_evolution_policy(minimum_deprecation_minor_releases = 2L)

new_phase10_migration_guidance(operation_id, action,
  successor_operation = NA_character_,
  schema_guidance = "No migration required.",
  deprecated_in = NA_character_, removal_not_before = NA_character_)

new_phase10_api_migration_plan(compatibility, guidance,
  policy = phase10_api_evolution_policy())

validate_phase10_api_migration_plan(plan, compatibility,
  policy = phase10_api_evolution_policy())

phase10_api_migration_report(plan, compatibility,
  policy = phase10_api_evolution_policy())
}
\arguments{
  \item{minimum_deprecation_minor_releases}{Minimum supported minor releases between deprecation and removal.}
  \item{operation_id}{Stable public operation identifier.}
  \item{action}{Migration action: \code{none}, \code{adopt}, \code{migrate}, or \code{replace}.}
  \item{successor_operation}{Optional successor operation identifier.}
  \item{schema_guidance}{Stable schema migration guidance.}
  \item{deprecated_in}{Optional API version where deprecation begins.}
  \item{removal_not_before}{Optional earliest removal API version.}
  \item{compatibility}{Validated Phase 10.2.1 compatibility record.}
  \item{guidance}{Migration-guidance data frame.}
  \item{policy}{API evolution policy record.}
  \item{plan}{Migration-plan record.}
}
\value{
A policy record, guidance row, migration plan, invisible \code{TRUE}, or Markdown report lines, depending on the function.
}
