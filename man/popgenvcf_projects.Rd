\name{popgenvcf_projects}
\alias{new_project_rng}
\alias{new_popgenvcf_project}
\alias{validate_popgenvcf_project}
\alias{project_table}
\alias{write_popgenvcf_project}
\alias{verify_popgenvcf_project}
\alias{read_popgenvcf_project}
\alias{compare_popgenvcf_projects}
\title{Create, bundle, reopen, and compare reproducible popgenVCF projects}
\usage{
new_project_rng(seed = NA_integer_, kind = RNGkind(), streams = list())

new_popgenvcf_project(
  name,
  results = list(),
  inputs = data.table::data.table(),
  parameters = list(),
  modules = list(),
  artifacts = list(),
  reports = list(),
  provenance = list(),
  rng = new_project_rng(),
  project_id = project_uuid(),
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  package_version = tryCatch(as.character(utils::packageVersion("popgenVCF")),
    error = function(e) NA_character_),
  git_sha = Sys.getenv("GITHUB_SHA", unset = NA_character_)
)

validate_popgenvcf_project(x)

project_table(x)

write_popgenvcf_project(project, path, overwrite = FALSE)

verify_popgenvcf_project(path)

read_popgenvcf_project(path, verify = TRUE)

compare_popgenvcf_projects(current, baseline)
}
\arguments{
\item{seed}{Optional integer random seed.}
\item{kind}{R random-number generator settings.}
\item{streams}{Named module or worker seed records.}
\item{name}{Human-readable project name.}
\item{results}{Named canonical analysis results.}
\item{inputs}{Input identity table or named character vector of paths.}
\item{parameters, modules, artifacts, reports, provenance}{Named project components.}
\item{rng}{Project RNG metadata.}
\item{project_id}{Stable project UUID.}
\item{created_at}{UTC creation timestamp.}
\item{package_version, git_sha}{Software identity.}
\item{x}{A project or project comparison.}
\item{project}{A validated project.}
\item{path}{A destination or existing \code{.popgenvcf} bundle.}
\item{overwrite}{Permit replacement of an existing bundle.}
\item{verify}{Verify internal SHA256 checksums while reading.}
\item{current, baseline}{Projects or bundle paths to compare.}
}
\value{
Constructors return canonical project metadata. Writing returns the normalized
bundle path invisibly. Reading returns a validated \code{PopgenVCFProject}.
Comparison returns a \code{PopgenVCFProjectComparison}; table conversion returns
a data table.
}
\description{
Reproducible projects preserve canonical scientific results, software and RNG
identity, input checksums, parameters, module records, artifacts, reports, and
provenance in a portable checksummed bundle that can be reopened without
recomputation.
}
