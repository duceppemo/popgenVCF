execution_plan_signature <- function(plan, registry) {
  contracts <- lapply(plan$order, function(name) {
    module <- registry$modules[[name]]
    list(
      name = name,
      requires = module$requires,
      outputs = module$outputs,
      resource_class = module$resource_class,
      parallel_safe = isTRUE(module$parallel_safe),
      contract_version = module$contract_version
    )
  })
  digest::digest(
    list(order = plan$order, waves = unname(plan$waves[plan$order]), contracts = contracts),
    algo = "sha256", serialize = TRUE
  )
}

checkpoint_payload_digest <- function(checkpoint) {
  payload <- checkpoint
  payload$checkpoint_digest <- NULL
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

checkpoint_remaining_plan <- function(full_plan, registry, remaining) {
  order <- full_plan$order[full_plan$order %in% remaining]
  waves <- execution_wave_map(registry, order)
  table <- data.table::copy(full_plan$table[match(order, full_plan$table$module)])
  table[, wave := unname(waves[module])]
  structure(list(order = order, waves = waves, table = table), class = "PopgenVCFExecutionPlan")
}

#' Create an execution checkpoint
#'
#' Capture validated execution state so unfinished modules can be resumed without
#' rerunning modules already recorded as successful.
#'
#' @param executed Result returned by [execute_analysis_plan()] or
#'   [execute_analysis_registry()].
#' @param registry The registry used for the execution.
#' @return A validated `PopgenVCFExecutionCheckpoint` object.
#' @export
new_execution_checkpoint <- function(executed, registry) {
  required <- c("analysis", "context", "order", "plan", "artifacts", "execution")
  if (!is.list(executed) || !all(required %in% names(executed))) {
    stop("executed must be a complete analysis execution result", call. = FALSE)
  }
  if (!inherits(registry, "PopgenVCFRegistry")) {
    stop("registry must be a PopgenVCFRegistry", call. = FALSE)
  }
  successful <- executed$execution$module[executed$execution$status == "success"]
  checkpoint <- structure(
    list(
      schema_version = "1.0",
      package_version = popgenvcf_version(),
      plan_order = executed$plan$order,
      plan_signature = execution_plan_signature(executed$plan, registry),
      completed = successful,
      analysis = executed$analysis,
      context = executed$context,
      artifacts = executed$artifacts,
      execution = data.table::copy(executed$execution),
      checkpoint_digest = NULL
    ),
    class = "PopgenVCFExecutionCheckpoint"
  )
  checkpoint$checkpoint_digest <- checkpoint_payload_digest(checkpoint)
  validate_execution_checkpoint(checkpoint, registry = registry)
  checkpoint
}

#' Validate an execution checkpoint
#'
#' @param checkpoint A `PopgenVCFExecutionCheckpoint` object.
#' @param registry Optional current registry used to detect plan or contract drift.
#' @return `checkpoint`, invisibly.
#' @export
validate_execution_checkpoint <- function(checkpoint, registry = NULL) {
  if (!inherits(checkpoint, "PopgenVCFExecutionCheckpoint")) {
    stop("checkpoint must be a PopgenVCFExecutionCheckpoint", call. = FALSE)
  }
  required <- c(
    "schema_version", "package_version", "plan_order", "plan_signature",
    "completed", "analysis", "context", "artifacts", "execution",
    "checkpoint_digest"
  )
  if (!all(required %in% names(checkpoint))) {
    stop("checkpoint is missing required fields", call. = FALSE)
  }
  if (!identical(checkpoint$checkpoint_digest, checkpoint_payload_digest(checkpoint))) {
    stop("execution checkpoint digest mismatch", call. = FALSE)
  }
  if (anyDuplicated(checkpoint$plan_order) || anyDuplicated(checkpoint$completed)) {
    stop("checkpoint module identities must be unique", call. = FALSE)
  }
  if (!all(checkpoint$completed %in% checkpoint$plan_order)) {
    stop("checkpoint completed modules are not part of the plan", call. = FALSE)
  }
  successful <- checkpoint$execution$module[checkpoint$execution$status == "success"]
  if (!setequal(checkpoint$completed, successful)) {
    stop("checkpoint completed modules disagree with the execution ledger", call. = FALSE)
  }
  validate_analysis(checkpoint$analysis)
  validate_artifact_manifest(checkpoint$artifacts)
  if (!is.null(registry)) {
    plan <- plan_analysis_execution(
      registry, checkpoint$analysis$config, selected = checkpoint$plan_order
    )
    if (!identical(checkpoint$plan_order, plan$order) ||
        !identical(checkpoint$plan_signature, execution_plan_signature(plan, registry))) {
      stop("execution checkpoint is incompatible with the current analysis plan", call. = FALSE)
    }
    for (name in checkpoint$completed) {
      outputs <- registry$modules[[name]]$outputs
      if (!all(outputs %in% names(checkpoint$analysis$results))) {
        stop("checkpoint is missing declared output(s) for module '", name, "'", call. = FALSE)
      }
    }
  }
  invisible(checkpoint)
}

#' Write an execution checkpoint
#'
#' The checkpoint is serialized inside a versioned runtime integrity envelope.
#' A whole-file SHA-256 sidecar protects the serialized bytes, while the envelope
#' and checkpoint digests independently protect schema compatibility and payload
#' semantics.
#'
#' @param checkpoint A validated execution checkpoint.
#' @param path Destination `.rds` path.
#' @param overwrite Whether an existing checkpoint may be replaced.
#' @return The normalized checkpoint path, invisibly.
#' @export
write_execution_checkpoint <- function(checkpoint, path, overwrite = FALSE) {
  validate_execution_checkpoint(checkpoint)
  envelope <- new_runtime_integrity_envelope("checkpoint", checkpoint)
  validate_runtime_integrity_envelope(envelope)
  path <- normalizePath(path, mustWork = FALSE)
  checksum_path <- paste0(path, ".sha256")
  if (!overwrite && (file.exists(path) || file.exists(checksum_path))) {
    stop("execution checkpoint already exists", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("execution-checkpoint-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(envelope, tmp, version = 3, compress = "xz")
  checksum <- digest::digest(file = tmp, algo = "sha256")
  if (!file.rename(tmp, path)) stop("unable to install execution checkpoint", call. = FALSE)
  writeLines(paste(checksum, basename(path)), checksum_path, useBytes = TRUE)
  invisible(path)
}

#' Read and verify an execution checkpoint
#'
#' Validation proceeds from the serialized bytes inward: the SHA-256 sidecar,
#' readable RDS structure, runtime envelope schema and digest, then checkpoint
#' invariants and optional registry compatibility.
#'
#' @param path Checkpoint `.rds` path.
#' @param registry Optional registry used for compatibility validation.
#' @return A validated `PopgenVCFExecutionCheckpoint` object.
#' @export
read_execution_checkpoint <- function(path, registry = NULL) {
  checksum_path <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(checksum_path)) {
    stop("checkpoint and SHA-256 sidecar are required", call. = FALSE)
  }
  sidecar <- readLines(checksum_path, warn = FALSE)
  if (!length(sidecar) || !nzchar(sidecar[[1]])) {
    stop("execution checkpoint SHA-256 sidecar is malformed", call. = FALSE)
  }
  expected <- strsplit(sidecar[[1]], "[[:space:]]+")[[1]][1]
  observed <- digest::digest(file = path, algo = "sha256")
  if (!identical(expected, observed)) {
    stop("execution checkpoint file checksum mismatch", call. = FALSE)
  }
  envelope <- tryCatch(
    readRDS(path),
    error = function(e) {
      stop("execution checkpoint is unreadable or truncated: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!inherits(envelope, "PopgenVCFRuntimeEnvelope")) {
    stop("legacy unwrapped execution checkpoint requires explicit migration", call. = FALSE)
  }
  checkpoint <- runtime_integrity_payload(envelope)
  validate_execution_checkpoint(checkpoint, registry = registry)
  checkpoint
}

#' Resume an analysis execution from a checkpoint
#'
#' Only modules not already recorded as successful are executed. Prior validated
#' outputs and artifacts are preserved, and the returned ledger identifies rows
#' reused from the checkpoint.
#'
#' @param checkpoint A validated execution checkpoint.
#' @param registry Current analysis registry.
#' @param engine Execution engine for unfinished modules.
#' @return A regular execution result with a combined ledger and `resumed_from_checkpoint` metadata.
#' @noRd
resume_analysis_execution <- function(checkpoint, registry,
                                      engine = new_execution_engine()) {
  validate_execution_checkpoint(checkpoint, registry = registry)
  full_plan <- plan_analysis_execution(
    registry, checkpoint$analysis$config, selected = checkpoint$plan_order
  )
  remaining <- setdiff(full_plan$order, checkpoint$completed)
  if (!length(remaining)) {
    execution <- data.table::copy(checkpoint$execution)
    execution[, checkpoint_reused := status == "success"]
    metadata <- checkpoint$analysis$results$execution_engine %||% list()
    metadata$resumed_from_checkpoint <- TRUE
    metadata$reused_modules <- checkpoint$completed
    analysis <- set_analysis_result(checkpoint$analysis, "execution_engine", metadata)
    analysis <- set_analysis_result(analysis, "execution_ledger", execution)
    return(list(
      analysis = analysis, context = checkpoint$context,
      order = checkpoint$completed, plan = full_plan,
      artifacts = checkpoint$artifacts, engine = metadata,
      execution = execution
    ))
  }
  remaining_plan <- checkpoint_remaining_plan(full_plan, registry, remaining)
  resumed <- execute_analysis_plan(
    checkpoint$analysis, checkpoint$context, registry, remaining_plan, engine
  )
  prior <- data.table::copy(checkpoint$execution)
  prior <- prior[module %in% checkpoint$completed]
  prior[, checkpoint_reused := TRUE]
  current <- data.table::copy(resumed$execution)
  current[, checkpoint_reused := FALSE]
  combined <- data.table::rbindlist(list(prior, current), use.names = TRUE, fill = TRUE)
  combined <- combined[match(full_plan$order, module)]
  artifacts <- append_artifact_manifest(checkpoint$artifacts, resumed$artifacts)
  metadata <- resumed$engine
  metadata$resumed_from_checkpoint <- TRUE
  metadata$reused_modules <- checkpoint$completed
  resumed$analysis <- set_analysis_result(resumed$analysis, "execution_engine", metadata)
  resumed$analysis <- set_analysis_result(resumed$analysis, "execution_ledger", combined)
  resumed$order <- c(checkpoint$completed, resumed$order)
  resumed$plan <- full_plan
  resumed$artifacts <- artifacts
  resumed$engine <- metadata
  resumed$execution <- combined
  resumed
}
