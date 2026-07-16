\name{publication-companion}
\alias{publication_style}
\alias{new_publication_bundle}
\alias{validate_publication_bundle}
\alias{generate_publication_bundle}
\alias{set_project_publication_bundle}
\title{Publication companion bundles}
\description{
Create validated, checksummed publication companion plans and directories from
reproducible popgenVCF projects without recomputing scientific analyses.
}
\usage{
publication_style(style = "generic")

new_publication_bundle(project, style = "generic", title = project$name)

validate_publication_bundle(x)

generate_publication_bundle(
  project,
  directory,
  style = "generic",
  title = project$name,
  include_project = TRUE,
  include_fair = TRUE,
  overwrite = FALSE
)

set_project_publication_bundle(project, bundle)
}
\arguments{
  \item{style}{A built-in style identifier or custom publication profile.}
  \item{project}{A validated \code{PopgenVCFProject}.}
  \item{title}{Optional manuscript title.}
  \item{x}{A publication bundle plan or written directory.}
  \item{directory}{Output directory.}
  \item{include_project}{Include the portable analysis project.}
  \item{include_fair}{Include embedded FAIR metadata when available.}
  \item{overwrite}{Permit replacement of a non-empty output directory.}
  \item{bundle}{A validated \code{PopgenVCFPublicationBundle}.}
}
\value{
Style and plan constructors return validated objects. The generator returns the
normalized publication directory invisibly. Validators return \code{TRUE}
invisibly or fail with an integrity error.
}
\details{
Built-in profiles include generic, Nature, Molecular Ecology, G3, BMC, and PLOS.
Profiles control labels and citation conventions only; they do not claim full
journal submission-template compliance. Scientific files are copied from the
immutable artifact lineage, and every written file is protected by SHA256 in
\code{publication-manifest.tsv}.
}
