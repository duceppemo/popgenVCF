\name{external_process_workspace}
\alias{new_external_process_workspace_policy}
\alias{print.PopgenVCFExternalProcessWorkspacePolicy}
\alias{run_supervised_external_command_in_workspace}
\title{Deterministic external-process workspaces}
\description{
Create isolated workspace policies and run supervised external commands with
explicit input staging, ordered lifecycle events, deterministic retention, and
workspace-content fingerprints.
}
\usage{
new_external_process_workspace_policy(
  root = tempdir(),
  cleanup_on_success = TRUE,
  retain_on_failure = TRUE,
  label = "default-process-workspace"
)

\method{print}{PopgenVCFExternalProcessWorkspacePolicy}(x, ...)

run_supervised_external_command_in_workspace(
  command,
  inputs = character(),
  execution_label = command$label,
  workspace_policy = new_external_process_workspace_policy(),
  requirements = new_module_resource_requirements(),
  supervision_policy = new_external_process_supervision_policy(),
  cancellation_token = NULL
)
}
\arguments{
  \item{root}{Existing directory beneath which workspaces are created.}
  \item{cleanup_on_success}{Whether successful workspaces are removed.}
  \item{retain_on_failure}{Whether unsuccessful workspaces are retained.}
  \item{label}{Stable workspace-policy label.}
  \item{x}{A workspace policy.}
  \item{...}{Unused.}
  \item{command}{A validated \code{PopgenVCFExternalCommand}.}
  \item{inputs}{Regular files copied into the workspace before launch.}
  \item{execution_label}{Stable label included in workspace identity.}
  \item{workspace_policy}{Workspace policy.}
  \item{requirements}{Declared process resource requirements.}
  \item{supervision_policy}{External-process supervision policy.}
  \item{cancellation_token}{Optional execution cancellation token.}
}
\value{
\code{new_external_process_workspace_policy()} returns a validated
\code{PopgenVCFExternalProcessWorkspacePolicy}. The runner returns a
\code{PopgenVCFExternalProcessResult} augmented with workspace identity,
retention state, input manifest, content fingerprint, and ordered lifecycle
events.
}
\details{
Workspace identity is derived from the original command fingerprint, workspace
policy label, execution label, and sorted staged-input fingerprints. Existing
workspace collisions fail before launch. Input files must be regular files with
unique basenames. Only explicitly declared inputs are staged.

Successful workspaces are cleaned by default. Failed workspaces are retained by
default for diagnosis and their contents are fingerprinted. Retention never
changes the scientific interpretation of the underlying supervised process
status.
}
