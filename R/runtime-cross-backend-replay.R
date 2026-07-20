cross_backend_ledger_projection <- function(execution, order) {
  if (!data.table::is.data.table(execution)) {
    stop("execution must be a data.table", call. = FALSE)
  }
  required <- c("module", "status")
  if (!all(required %in% names(execution))) {
    stop("execution is missing module or status", call. = FALSE)
  }
  modules <- as.character(execution$module)
  if (anyDuplicated(modules) || !identical(modules, as.character(order))) {
    stop("execution modules must uniquely match plan order", call. = FALSE)
  }
  statuses <- as.character(execution$status)
  if (!all(statuses %in% runtime_terminal_statuses())) {
    stop("cross-backend replay equivalence requires terminal execution states", call. = FALSE)
  }
  volatile <- c(
    "started_at", "finished_at", "elapsed_seconds", "worker_pid",
    "dispatch_sequence", "completion_sequence"
  )
  semantic <- setdiff(names(execution), volatile)
  data.table::copy(execution[, ..semantic])
}

cross_backend_replay_components <- function(execution) {
  components <- recovery_equivalence_components(execution)
  components$execution <- cross_backend_ledger_projection(
    execution$execution, execution$plan$order
  )
  components
}

#' Verify deterministic replay equivalence across execution backends
#'
#' Backend-specific scheduling telemetry, worker identities, completion timing,
#' and elapsed-time observations are intentionally excluded. Scientific state,
#' accepted-result order, plan identity, artifacts, terminal outcomes, and
#' canonical ledger semantics must remain deterministic.
#'
#' @param reference A complete execution result from the reference backend.
#' @param candidate A complete execution result from another backend.
#' @return A `PopgenVCFCrossBackendReplayEquivalence` verification report.
#' @export
verify_cross_backend_replay_equivalence <- function(reference, candidate) {
  reference_components <- cross_backend_replay_components(reference)
  candidate_components <- cross_backend_replay_components(candidate)
  component_names <- names(reference_components)
  reference_digests <- vapply(
    reference_components, runtime_payload_digest, character(1), USE.NAMES = TRUE
  )
  candidate_digests <- vapply(
    candidate_components, runtime_payload_digest, character(1), USE.NAMES = TRUE
  )
  differences <- component_names[reference_digests != candidate_digests]
  if (length(differences)) {
    stop(
      "execution backends are not replay-equivalent: ",
      paste(differences, collapse = ", "),
      call. = FALSE
    )
  }
  structure(
    list(
      verified = TRUE,
      components = component_names,
      component_digests = reference_digests,
      replay_fingerprint = runtime_payload_digest(reference_digests)
    ),
    class = "PopgenVCFCrossBackendReplayEquivalence"
  )
}
