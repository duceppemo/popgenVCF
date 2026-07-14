\name{analysis-artifacts}
\alias{new_analysis_artifact}
\alias{validate_analysis_artifact}
\alias{new_artifact_manifest}
\alias{register_artifact}
\alias{validate_artifact_manifest}
\alias{artifact_manifest_table}
\title{Canonical analysis artifacts and manifests}
\description{
Create and validate stable descriptions of files emitted by analysis modules.
The artifact manifest separates publication outputs from in-memory statistical
results and provides a common interface for reports, provenance, release, and
validation tooling.
}
\usage{
new_analysis_artifact(module, name, type, path, format,
  description = "", required = TRUE, metadata = list())

validate_analysis_artifact(artifact, must_exist = FALSE)

new_artifact_manifest(artifacts = list())

register_artifact(manifest, artifact)

validate_artifact_manifest(manifest, must_exist = FALSE)

artifact_manifest_table(manifest)
}
\arguments{
  \item{module}{Analysis module name.}
  \item{name}{Stable artifact identifier within the module.}
  \item{type}{Artifact type: table, figure, methods, caption, supplementary,
  validation, provenance, report, or data.}
  \item{path}{Artifact file path.}
  \item{format}{Lowercase file format such as tsv, pdf, svg, png, md, or json.}
  \item{description}{Human-readable artifact description.}
  \item{required}{Whether absence of the artifact invalidates publication output.}
  \item{metadata}{Optional named metadata list.}
  \item{artifact}{A \code{PopgenVCFArtifact}.}
  \item{must_exist}{Whether required files must exist on disk.}
  \item{artifacts}{A list of artifacts.}
  \item{manifest}{A \code{PopgenVCFArtifactManifest}.}
}
\value{
Constructors return artifact or manifest objects. Validators return
\code{TRUE} invisibly. \code{artifact_manifest_table()} returns a data table.
}
\examples{
a <- new_analysis_artifact(
  module = "pca", name = "scores", type = "table",
  path = "tables/pca_scores.tsv", format = "tsv"
)
m <- register_artifact(new_artifact_manifest(), a)
artifact_manifest_table(m)
}
