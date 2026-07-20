# Deterministic concurrency stress invariants.

#' Validate scheduler concurrency invariants
#'
#' Validates the canonical scheduler properties that must hold independently of
#' worker completion timing or backend-specific telemetry.
#'
#' @param execution A scheduler execution ledger.
#' @param planned_order Character vector containing the canonical module order.
#' @return `TRUE`, invisibly, when all invariants hold.
#' @export
validate_concurrency_stress_invariants <- function(execution, planned_order) {
  required <- c(
    "module", "status", "dispatch_sequence", "completion_sequence",
    "merge_sequence"
  )
  if (!is.data.frame(execution) || !all(required %in% names(execution))) {
    stop("execution must contain canonical scheduler ledger fields", call. = FALSE)
  }
  planned_order <- as.character(planned_order)
  modules <- as.character(execution$module)
  if (!length(modules) || anyNA(modules) || any(!nzchar(modules))) {
    stop("execution modules must be non-empty strings", call. = FALSE)
  }
  if (anyDuplicated(modules)) {
    stop("execution contains duplicate modules", call. = FALSE)
  }
  if (!identical(modules, planned_order)) {
    stop("execution module order differs from planned order", call. = FALSE)
  }

  terminal <- c("success", "failed", "blocked", "cancelled", "skipped", "timed_out", "resource_unavailable")
  if (anyNA(execution$status) || any(!execution$status %in% terminal)) {
    stop("execution contains nonterminal or unsupported status values", call. = FALSE)
  }

  dispatched <- !is.na(execution$dispatch_sequence)
  completed <- !is.na(execution$completion_sequence)
  merged <- !is.na(execution$merge_sequence)
  if (any(completed & !dispatched)) {
    stop("execution records completion without dispatch", call. = FALSE)
  }
  if (any(merged & !completed)) {
    stop("execution records merge without completion", call. = FALSE)
  }
  for (field in c("dispatch_sequence", "completion_sequence", "merge_sequence")) {
    values <- execution[[field]][!is.na(execution[[field]])]
    if (anyDuplicated(values)) {
      stop(sprintf("execution contains duplicate %s values", field), call. = FALSE)
    }
    if (length(values) && !identical(sort(as.integer(values)), seq_along(values))) {
      stop(sprintf("execution %s values are not contiguous", field), call. = FALSE)
    }
  }

  successful <- execution$status == "success"
  if (any(successful & !merged)) {
    stop("successful modules must have a merge sequence", call. = FALSE)
  }
  merged_modules <- modules[order(execution$merge_sequence, na.last = NA)]
  expected_merged <- planned_order[planned_order %in% modules[successful]]
  if (!identical(merged_modules, expected_merged)) {
    stop("accepted results were not merged in planned order", call. = FALSE)
  }

  invisible(TRUE)
}
