recovery_analysis_projection <- function(analysis) {
  validate_analysis(analysis)
  projected <- analysis
  projected$results$execution_engine <- NULL
  projected$results$execution_ledger <- NULL
  projected
}

recovery_plan_projection <- function(plan) {
  if (!inherits(plan, "PopgenVCFExecutionPlan") || !is.list(plan)) {
    stop("plan must be a PopgenVCFExecutionPlan", call. = FALSE)
  }
  list(
    order = as.character(plan$order),
    waves = unname(as.integer(plan$waves[plan$order])),
    table = data.table::copy(plan$table[match(plan$order, plan$table$module)])
  )
}

recovery_execution_projection <- function(execution, order) {
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
    stop("recovery equivalence requires terminal execution states", call. = FALSE)
  }
  data.table::data.table(module = modules, status = statuses)
}

recovery_equivalence_components <- function(execution) {
  required <- c("analysis", "context", "order", "plan", "artifacts", "execution")
  if (!is.list(execution) || !all(required %in% names(execution))) {
    stop("execution must be a complete analysis execution result", call. = FALSE)
  }
  validate_artifact_manifest(execution$artifacts)
  order <- as.character(execution$order)
  if (anyDuplicated(order) || !identical(order, as.character(execution$plan$order))) {
    stop("execution order must uniquely match plan order", call. = FALSE)
  }
  list(
    analysis = recovery_analysis_projection(execution$analysis),
    context = execution$context,
    order = order,
    plan = recovery_plan_projection(execution$plan),
    artifacts = execution$artifacts,
    execution = recovery_execution_projection(execution$execution, order)
  )
}

#' Verify recovery equivalence against an uninterrupted execution
#'
#' Recovery-specific bookkeeping such as `checkpoint_reused` and execution-engine
#' resume metadata is intentionally excluded. Scientific results, context,
#' artifacts, plan identity, module order, and terminal outcomes must remain
#' byte-for-byte deterministic after canonical serialization.
#'
#' @param reference A complete uninterrupted analysis execution result.
#' @param recovered A complete execution result resumed from a checkpoint.
#' @return A `PopgenVCFRuntimeRecoveryEquivalence` verification report.
#' @export
verify_runtime_recovery_equivalence <- function(reference, recovered) {
  reference_components <- recovery_equivalence_components(reference)
  recovered_components <- recovery_equivalence_components(recovered)
  component_names <- names(reference_components)
  reference_digests <- vapply(
    reference_components, runtime_payload_digest, character(1), USE.NAMES = TRUE
  )
  recovered_digests <- vapply(
    recovered_components, runtime_payload_digest, character(1), USE.NAMES = TRUE
  )
  differences <- component_names[reference_digests != recovered_digests]
  if (length(differences)) {
    stop(
      "recovered execution is not equivalent to the uninterrupted execution: ",
      paste(differences, collapse = ", "),
      call. = FALSE
    )
  }
  structure(
    list(
      verified = TRUE,
      components = component_names,
      component_digests = reference_digests,
      recovery_fingerprint = runtime_payload_digest(reference_digests)
    ),
    class = "PopgenVCFRuntimeRecoveryEquivalence"
  )
}
