\name{external_process_workspace_persistence}
\alias{new_external_process_workspace}
\alias{validate_external_process_workspace}
\alias{write_external_process_workspace}
\alias{read_external_process_workspace}
\title{External-process workspace integrity persistence}
\usage{
new_external_process_workspace(result)

validate_external_process_workspace(workspace)

write_external_process_workspace(workspace, path, overwrite = FALSE)

read_external_process_workspace(path)
}
\arguments{
  \item{result}{A workspace-backed \code{PopgenVCFExternalProcessResult}.}
  \item{workspace}{A canonical \code{PopgenVCFExternalProcessWorkspace} record or, when writing, a workspace-backed process result.}
  \item{path}{Destination or source \code{.rds} path.}
  \item{overwrite}{Whether existing record and sidecar files may be replaced.}
}
\value{
\code{new_external_process_workspace()} and
\code{read_external_process_workspace()} return a validated
\code{PopgenVCFExternalProcessWorkspace}. Validation returns its input
invisibly, and writing returns the normalized destination path invisibly.
}
\description{
Creates, validates, writes, and reads deterministic provenance snapshots for
isolated external-process workspaces. Records protect command linkage,
workspace identity, retention state, staged-input manifests, final contents
fingerprints, and ordered lifecycle events using versioned runtime integrity
envelopes and whole-file SHA-256 sidecars.
}
\details{
The persistence record protects workspace provenance, not the potentially large
workspace directory itself. A retained directory is represented by its recorded
path and final canonical contents fingerprint. Readers fail closed on malformed
sidecars, file corruption, unsupported schemas, envelope mutation, invalid
manifests, inconsistent retention metadata, and malformed lifecycle histories.
}
