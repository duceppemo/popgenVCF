runtime_execution_statuses <- function() {
  c("pending", "running", "success", "failed", "blocked", "cancelled", "skipped")
}

runtime_terminal_statuses <- function() {
  c("success", "cancelled", "skipped")
}

#' Return the canonical runtime state-transition matrix
#'
#' The matrix is deliberately fail closed. Rows are source states and columns
#' are target states. Terminal states may only remain unchanged.
#'
#' @return A logical matrix describing allowed runtime state transitions.
#' @export
runtime_state_transition_matrix <- function() {
  statuses <- runtime_execution_statuses()
  matrix <- matrix(
    FALSE,
    nrow = length(statuses),
    ncol = length(statuses),
    dimnames = list(from = statuses, to = statuses)
  )
  allowed <- list(
    pending = c("pending", "running", "blocked", "cancelled", "skipped"),
    running = c("running", "success", "failed", "blocked", "cancelled"),
    failed = c("pending", "running", "failed", "cancelled", "skipped"),
    blocked = c("pending", "running", "blocked", "cancelled", "skipped"),
    success = "success",
    cancelled = "cancelled",
    skipped = "skipped"
  )
  for (from in names(allowed)) matrix[from, allowed[[from]]] <- TRUE
  matrix
}

#' Validate one runtime state transition
#'
#' @param from Source runtime status.
#' @param to Target runtime status.
#' @return `TRUE`, invisibly.
#' @export
validate_runtime_state_transition <- function(from, to) {
  from <- as.character(from)[1]
  to <- as.character(to)[1]
  statuses <- runtime_execution_statuses()
  if (is.na(from) || !from %in% statuses || is.na(to) || !to %in% statuses) {
    stop("runtime state transition contains an unsupported status", call. = FALSE)
  }
  if (!isTRUE(runtime_state_transition_matrix()[from, to])) {
    stop("forbidden runtime state transition: ", from, " -> ", to, call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate an ordered runtime state history
#'
#' @param statuses Ordered runtime statuses for one module.
#' @param module Optional module identity used in diagnostics.
#' @return `statuses`, invisibly.
#' @export
validate_runtime_state_history <- function(statuses, module = NULL) {
  statuses <- as.character(statuses)
  label <- if (is.null(module)) "" else paste0(" for module: ", as.character(module)[1])
  if (!length(statuses) || anyNA(statuses) ||
      !all(statuses %in% runtime_execution_statuses())) {
    stop("runtime state history contains an unsupported status", label, call. = FALSE)
  }
  if (length(statuses) > 1L) {
    for (index in seq_len(length(statuses) - 1L)) {
      tryCatch(
        validate_runtime_state_transition(statuses[[index]], statuses[[index + 1L]]),
        error = function(error) {
          stop(conditionMessage(error), label, call. = FALSE)
        }
      )
    }
  }
  invisible(statuses)
}

#' Assemble a replay bundle from integrity envelopes
#'
#' Every legacy envelope is migrated explicitly before payload extraction.
#' Current envelopes produce deterministic no-op migration records. Mixed or
#' partially migrated inputs cannot bypass current-schema validation.
#'
#' @param envelopes List of runtime integrity envelopes.
#' @param migration_registry Runtime migration registry.
#' @return A `PopgenVCFRuntimeReplayAssembly` containing the verified bundle and
#'   ordered migration records.
#' @export
assemble_runtime_replay_from_envelopes <- function(envelopes, migration_registry) {
  if (!is.list(envelopes) || !length(envelopes)) {
    stop("runtime replay envelopes must be a non-empty list", call. = FALSE)
  }
  if (!inherits(migration_registry, "PopgenVCFRuntimeMigrationRegistry")) {
    stop("migration_registry must be a PopgenVCFRuntimeMigrationRegistry", call. = FALSE)
  }

  migrated <- lapply(envelopes, function(envelope) {
    migrate_runtime_integrity_envelope(envelope, migration_registry)
  })
  current <- lapply(migrated, function(result) {
    validate_runtime_integrity_envelope(result$envelope)
    result$envelope
  })
  kinds <- vapply(current, function(envelope) envelope$kind, character(1))
  singleton <- c("execution_ledger", "attempt_ledger")
  duplicates <- singleton[vapply(singleton, function(kind) sum(kinds == kind) > 1L, logical(1))]
  if (length(duplicates)) {
    stop("runtime replay assembly contains duplicate singleton kind: ",
         duplicates[[1]], call. = FALSE)
  }
  if (sum(kinds == "execution_ledger") != 1L) {
    stop("runtime replay assembly requires exactly one execution ledger", call. = FALSE)
  }
  supported <- c("execution_ledger", "attempt_ledger", "process_result", "process_workspace")
  unsupported <- setdiff(unique(kinds), supported)
  if (length(unsupported)) {
    stop("runtime replay assembly contains unsupported artifact kind: ",
         unsupported[[1]], call. = FALSE)
  }

  payloads <- lapply(current, runtime_integrity_payload)
  execution <- payloads[[which(kinds == "execution_ledger")]]
  attempt_index <- which(kinds == "attempt_ledger")
  attempts <- if (length(attempt_index)) payloads[[attempt_index]] else NULL
  bundle <- new_runtime_replay_bundle(
    execution_ledger = execution,
    attempt_ledger = attempts,
    process_results = payloads[kinds == "process_result"],
    process_workspaces = payloads[kinds == "process_workspace"]
  )
  records <- lapply(migrated, `[[`, "record")
  order_key <- paste(kinds, vapply(records, function(record) record$source_payload_digest,
                                   character(1)), sep = ":")
  records <- records[order(order_key)]
  structure(
    list(
      bundle = bundle,
      migration_records = records,
      assembly_fingerprint = runtime_payload_digest(list(
        replay_fingerprint = bundle$verification$replay_fingerprint,
        migration_fingerprints = vapply(
          records, function(record) record$migration_fingerprint, character(1)
        )
      ))
    ),
    class = "PopgenVCFRuntimeReplayAssembly"
  )
}
