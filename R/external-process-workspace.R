#' Create an external-process workspace policy
#'
#' Workspace policies isolate supervised commands from caller working directories
#' and define deterministic retention and cleanup behaviour.
#'
#' @param root Existing directory beneath which workspaces are created.
#' @param cleanup_on_success Remove workspaces after successful commands.
#' @param retain_on_failure Retain workspaces for unsuccessful commands.
#' @param label Stable policy label recorded in provenance.
#' @return A validated `PopgenVCFExternalProcessWorkspacePolicy` object.
#' @export
new_external_process_workspace_policy <- function(
    root = tempdir(),
    cleanup_on_success = TRUE,
    retain_on_failure = TRUE,
    label = "default-process-workspace") {
  root <- as.character(root)[1]
  if (is.na(root) || !dir.exists(root)) {
    stop("root must be an existing directory", call. = FALSE)
  }
  cleanup_on_success <- as.logical(cleanup_on_success)[1]
  retain_on_failure <- as.logical(retain_on_failure)[1]
  if (is.na(cleanup_on_success) || is.na(retain_on_failure)) {
    stop("workspace cleanup flags must be TRUE or FALSE", call. = FALSE)
  }
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }
  structure(
    list(
      root = normalizePath(root, mustWork = TRUE),
      cleanup_on_success = cleanup_on_success,
      retain_on_failure = retain_on_failure,
      label = label
    ),
    class = "PopgenVCFExternalProcessWorkspacePolicy"
  )
}

#' Print an external-process workspace policy
#' @param x A workspace policy.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExternalProcessWorkspacePolicy <- function(x, ...) {
  cat("<PopgenVCFExternalProcessWorkspacePolicy>\n")
  cat("  label:", x$label, "\n")
  cat("  root:", x$root, "\n")
  cat("  cleanup on success:", x$cleanup_on_success, "\n")
  cat("  retain on failure:", x$retain_on_failure, "\n")
  invisible(x)
}

workspace_event <- function(sequence, event, detail = NA_character_) {
  data.table::data.table(
    sequence = as.integer(sequence),
    event = as.character(event),
    detail = as.character(detail)
  )
}

workspace_input_manifest <- function(inputs) {
  inputs <- as.character(inputs)
  if (!length(inputs)) {
    return(data.table::data.table(
      source = character(), staged_name = character(), sha256 = character()
    ))
  }
  if (anyNA(inputs) || any(!file.exists(inputs)) || any(dir.exists(inputs))) {
    stop("inputs must contain existing regular files", call. = FALSE)
  }
  normalized <- normalizePath(inputs, mustWork = TRUE)
  staged_names <- basename(normalized)
  if (anyDuplicated(staged_names)) {
    stop("inputs must have unique basenames for deterministic staging", call. = FALSE)
  }
  order_index <- order(staged_names, normalized)
  normalized <- normalized[order_index]
  staged_names <- staged_names[order_index]
  data.table::data.table(
    source = normalized,
    staged_name = staged_names,
    sha256 = vapply(normalized, digest::digest, character(1),
      algo = "sha256", file = TRUE, serialize = FALSE)
  )
}

workspace_identifier <- function(command, policy, manifest, execution_label) {
  digest::digest(
    list(
      command_fingerprint = command$fingerprint,
      workspace_policy = policy$label,
      execution_label = execution_label,
      inputs = manifest[, .(staged_name, sha256)]
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

workspace_contents_fingerprint <- function(path) {
  files <- list.files(path, recursive = TRUE, all.files = TRUE,
                      full.names = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) return(digest::digest(character(), algo = "sha256"))
  relative <- substring(files, nchar(path) + 2L)
  index <- order(relative)
  digest::digest(
    data.frame(
      path = relative[index],
      sha256 = vapply(files[index], digest::digest, character(1),
        algo = "sha256", file = TRUE, serialize = FALSE),
      stringsAsFactors = FALSE
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

#' Run a supervised command in an isolated deterministic workspace
#'
#' Declared input files are copied into an isolated workspace before launch.
#' Lifecycle events, input fingerprints, retention state, and any retained
#' workspace-content fingerprint are attached to the process result.
#'
#' @param command A validated `PopgenVCFExternalCommand`.
#' @param inputs Character vector of regular files to stage.
#' @param execution_label Stable execution label used in workspace identity.
#' @param workspace_policy Workspace policy.
#' @param requirements Declared process resource requirements.
#' @param supervision_policy External-process supervision policy.
#' @param cancellation_token Optional execution cancellation token.
#' @return A `PopgenVCFExternalProcessResult` with workspace provenance.
#' @export
run_supervised_external_command_in_workspace <- function(
    command,
    inputs = character(),
    execution_label = command$label,
    workspace_policy = new_external_process_workspace_policy(),
    requirements = new_module_resource_requirements(),
    supervision_policy = new_external_process_supervision_policy(),
    cancellation_token = NULL) {
  validate_external_command(command)
  if (!inherits(workspace_policy, "PopgenVCFExternalProcessWorkspacePolicy")) {
    stop("workspace_policy must be a PopgenVCFExternalProcessWorkspacePolicy", call. = FALSE)
  }
  execution_label <- as.character(execution_label)[1]
  if (is.na(execution_label) || !nzchar(execution_label)) {
    stop("execution_label must be a non-empty string", call. = FALSE)
  }

  manifest <- workspace_input_manifest(inputs)
  identifier <- workspace_identifier(command, workspace_policy, manifest, execution_label)
  workspace <- file.path(workspace_policy$root, paste0("popgenvcf-", substr(identifier, 1, 20)))
  if (file.exists(workspace)) {
    stop("deterministic workspace already exists", call. = FALSE)
  }
  if (!dir.create(workspace, recursive = FALSE)) {
    stop("unable to create external-process workspace", call. = FALSE)
  }

  events <- workspace_event(1L, "workspace_created", basename(workspace))
  cleanup_required <- TRUE
  on.exit({
    if (cleanup_required && dir.exists(workspace)) {
      unlink(workspace, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  if (nrow(manifest)) {
    copied <- file.copy(manifest$source, file.path(workspace, manifest$staged_name),
                        overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
    if (!all(copied)) {
      stop("unable to stage all declared workspace inputs", call. = FALSE)
    }
  }
  events <- data.table::rbindlist(list(
    events,
    workspace_event(2L, "inputs_staged", as.character(nrow(manifest))),
    workspace_event(3L, "process_dispatched", command$label)
  ))

  workspace_command <- new_external_command(
    executable = command$executable,
    args = command$args,
    working_directory = workspace,
    environment = command$environment,
    label = command$label
  )
  result <- run_supervised_external_command(
    workspace_command,
    requirements = requirements,
    supervision_policy = supervision_policy,
    cancellation_token = cancellation_token
  )
  events <- data.table::rbindlist(list(
    events,
    workspace_event(4L, "process_completed", result$status)
  ))

  successful <- identical(result$status, "success")
  retain <- if (successful) !workspace_policy$cleanup_on_success else workspace_policy$retain_on_failure
  contents_fingerprint <- workspace_contents_fingerprint(workspace)

  if (!retain) {
    unlink(workspace, recursive = TRUE, force = TRUE)
    cleanup_required <- FALSE
    events <- data.table::rbindlist(list(
      events,
      workspace_event(5L, "workspace_cleaned", "completed")
    ))
  } else {
    cleanup_required <- FALSE
    events <- data.table::rbindlist(list(
      events,
      workspace_event(5L, "workspace_retained", result$status)
    ))
  }

  result$workspace <- list(
    policy = workspace_policy$label,
    identifier = identifier,
    path = if (retain) normalizePath(workspace, mustWork = TRUE) else NA_character_,
    retained = retain,
    input_manifest = manifest,
    contents_fingerprint = contents_fingerprint,
    events = events
  )
  result$original_command_fingerprint <- command$fingerprint
  result
}
