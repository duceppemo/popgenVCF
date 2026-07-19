#' Create and verify a deterministic runtime replay bundle
#'
#' @param execution_ledger A validated execution ledger.
#' @param attempt_ledger Optional validated attempt ledger.
#' @param process_results List of validated external-process results.
#' @param process_workspaces List of validated workspace provenance records.
#' @return A verified `PopgenVCFRuntimeReplayBundle`.
#' @export
new_runtime_replay_bundle <- function(
    execution_ledger,
    attempt_ledger = NULL,
    process_results = list(),
    process_workspaces = list()) {
  validate_execution_ledger(execution_ledger)
  if (!is.null(attempt_ledger)) validate_attempt_ledger(attempt_ledger)
  if (!is.list(process_results) || !is.list(process_workspaces)) {
    stop("process_results and process_workspaces must be lists", call. = FALSE)
  }
  lapply(process_results, validate_external_process_result)
  lapply(process_workspaces, validate_external_process_workspace)
  bundle <- structure(
    list(
      execution_ledger = data.table::copy(execution_ledger),
      attempt_ledger = if (is.null(attempt_ledger)) NULL else data.table::copy(attempt_ledger),
      process_results = process_results,
      process_workspaces = process_workspaces
    ),
    class = "PopgenVCFRuntimeReplayBundle"
  )
  verify_runtime_replay(bundle)
}

replay_fingerprints <- function(values, accessor) {
  if (!length(values)) return(character())
  vapply(values, accessor, character(1), USE.NAMES = FALSE)
}

#' Verify cross-artifact replay consistency
#'
#' Validation is fail closed. Individually valid artifacts are rejected when
#' module identities, retry outcomes, command fingerprints, process statuses,
#' or workspace relationships do not form one coherent replay graph.
#'
#' @param bundle A `PopgenVCFRuntimeReplayBundle`.
#' @return A deterministic verification report.
#' @export
verify_runtime_replay <- function(bundle) {
  if (!inherits(bundle, "PopgenVCFRuntimeReplayBundle") || !is.list(bundle)) {
    stop("bundle must be a PopgenVCFRuntimeReplayBundle", call. = FALSE)
  }
  validate_execution_ledger(bundle$execution_ledger)
  execution <- bundle$execution_ledger
  attempts <- bundle$attempt_ledger
  results <- bundle$process_results
  workspaces <- bundle$process_workspaces

  if (!is.null(attempts)) {
    validate_attempt_ledger(attempts)
    execution_modules <- sort(as.character(execution$module))
    attempt_modules <- sort(unique(as.character(attempts$module)))
    if (!identical(execution_modules, attempt_modules)) {
      stop("execution and attempt ledgers contain different module sets", call. = FALSE)
    }
    for (target_module in execution_modules) {
      chain <- attempts[as.character(attempts$module) == target_module][order(attempt)]
      final <- chain[nrow(chain)]
      recorded <- execution[as.character(execution$module) == target_module]
      if (!identical(as.character(final$status), as.character(recorded$status))) {
        stop("final attempt status conflicts with execution ledger for module: ",
             target_module, call. = FALSE)
      }
      if ("attempt" %in% names(execution) &&
          !identical(as.integer(recorded$attempt), max(as.integer(chain$attempt)))) {
        stop("execution attempt counter conflicts with retry chain for module: ",
             target_module, call. = FALSE)
      }
    }
  }

  lapply(results, validate_external_process_result)
  lapply(workspaces, validate_external_process_workspace)
  result_keys <- replay_fingerprints(results, function(x) x$command_fingerprint)
  workspace_keys <- replay_fingerprints(workspaces, function(x) x$workspace_command_fingerprint)
  if (anyDuplicated(result_keys)) {
    stop("replay bundle contains duplicate external process results", call. = FALSE)
  }
  if (anyDuplicated(workspace_keys)) {
    stop("replay bundle contains duplicate external process workspaces", call. = FALSE)
  }
  if (!identical(sort(result_keys), sort(workspace_keys))) {
    stop("external process results and workspaces are orphaned or cross-linked", call. = FALSE)
  }
  if (length(results)) {
    result_index <- setNames(seq_along(results), result_keys)
    for (workspace in workspaces) {
      result <- results[[result_index[[workspace$workspace_command_fingerprint]]]]
      if (!identical(as.character(result$status), as.character(workspace$process_status))) {
        stop("external process status conflicts with workspace provenance", call. = FALSE)
      }
      original <- result$original_command_fingerprint
      if (is.null(original) ||
          !identical(as.character(original)[1], workspace$command_fingerprint)) {
        stop("original command fingerprint conflicts with workspace provenance",
             call. = FALSE)
      }
    }
  }

  component_digests <- list(
    execution_ledger = runtime_payload_digest(execution),
    attempt_ledger = if (is.null(attempts)) NA_character_ else runtime_payload_digest(attempts),
    process_results = sort(vapply(results, runtime_payload_digest, character(1))),
    process_workspaces = sort(vapply(workspaces, runtime_payload_digest, character(1)))
  )
  report <- structure(
    list(
      verified = TRUE,
      module_count = nrow(execution),
      attempt_count = if (is.null(attempts)) 0L else nrow(attempts),
      process_count = length(results),
      workspace_count = length(workspaces),
      component_digests = component_digests,
      replay_fingerprint = runtime_payload_digest(component_digests)
    ),
    class = "PopgenVCFRuntimeReplayVerification"
  )
  bundle$verification <- report
  bundle
}
