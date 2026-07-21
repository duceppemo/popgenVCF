\name{phase10_public_artifact_provenance}
\alias{list_public_artifacts}
\alias{inspect_public_provenance}
\title{Phase 10 Public Artifact and Provenance Adapters}
\usage{
list_public_artifacts(request, manifest)

inspect_public_provenance(request, dag)
}
\arguments{
  \item{request}{A canonical public API request for \code{artifact.list} or \code{provenance.inspect}.}
  \item{manifest}{An existing \code{PopgenVCFArtifactManifest}.}
  \item{dag}{An existing \code{PopgenVCFProvenanceDAG}.}
}
\value{A validated \code{PopgenVCFPublicAPIResponse}.}
\description{
Translate existing artifact manifests and provenance DAGs into stable Phase 10
public responses. Filesystem paths, timestamps, software details, parameters,
and other implementation-only metadata are intentionally excluded.
}
