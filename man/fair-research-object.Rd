\name{fair-research-object}
\alias{new_fair_creator}
\alias{fair_identifier}
\alias{new_fair_metadata}
\alias{validate_fair_metadata}
\alias{fair_documents}
\alias{write_fair_bundle}
\alias{validate_fair_bundle}
\alias{set_project_fair_metadata}
\title{FAIR research-object metadata and exports}
\description{
Create validated FAIR metadata for a reproducible popgenVCF project and export
RO-Crate, CodeMeta, DataCite JSON, CITATION.cff, and checksummed bundle manifests.
}
\usage{
new_fair_creator(
  name,
  orcid = NA_character_,
  affiliation = NA_character_,
  email = NA_character_
)

fair_identifier(project, artifact_id = NULL)

new_fair_metadata(
  project,
  title = project$name,
  description = "Reproducible population-genomics analysis",
  creators = list(),
  license = "MIT",
  keywords = c("population genetics", "VCF", "reproducibility"),
  publisher = "popgenVCF",
  publication_year = as.integer(format(Sys.Date(), "%Y")),
  rights_uri = NA_character_
)

validate_fair_metadata(x)

fair_documents(metadata)

write_fair_bundle(metadata, directory, overwrite = FALSE)

validate_fair_bundle(directory)

set_project_fair_metadata(project, metadata)
}
\arguments{
  \item{name}{Creator name.}
  \item{orcid}{Optional ORCID identifier.}
  \item{affiliation}{Optional affiliation.}
  \item{email}{Optional email address.}
  \item{project}{A \code{PopgenVCFProject}.}
  \item{artifact_id}{Optional artifact identifier.}
  \item{title}{Project title.}
  \item{description}{Project description.}
  \item{creators}{Creator records from \code{new_fair_creator()}.}
  \item{license}{SPDX identifier or license URL.}
  \item{keywords}{Character keywords.}
  \item{publisher}{Publisher or repository.}
  \item{publication_year}{Publication year.}
  \item{rights_uri}{Optional explicit license URI.}
  \item{x}{A \code{PopgenVCFFAIRMetadata}.}
  \item{metadata}{Validated FAIR metadata.}
  \item{directory}{Output directory.}
  \item{overwrite}{Permit replacement of a non-empty output directory.}
}
\value{
Creator and metadata constructors return validated records. \code{fair_documents()}
returns plain standards-facing records. Writers return normalized paths invisibly;
validators return \code{TRUE} or fail with an integrity error.
}
\details{
The immutable VCF/project identity remains authoritative. Project URNs derive from
the project UUID, while artifact URNs derive from the project UUID and immutable
artifact identifier. Complete metadata remains in project RDS bundles; JSON-facing
records contain only plain serializable values.
}
