#' Print an execution checkpoint
#'
#' @param x A `PopgenVCFExecutionCheckpoint` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExecutionCheckpoint <- function(x, ...) {
  cat("<PopgenVCFExecutionCheckpoint>\n")
  cat("  schema version:", x$schema_version, "\n")
  cat("  planned modules:", length(x$plan_order), "\n")
  cat("  completed modules:", length(x$completed), "\n")
  cat("  digest:", x$checkpoint_digest, "\n")
  invisible(x)
}

checkpoint_remaining_plan <- function(full_plan, completed) {
  remaining <- setdiff(full_plan$order, completed)
  table <- full_plan$table[match(remaining, full_plan$table$module)]
  structure(
    list(
      order = remaining,
      waves = full_plan$waves[remaining],
      table = table
    ),
    class = "PopgenVCFExecutionPlan"
  )
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
#' @export
resume_analysis_execution <- function(checkpoint, registry,
                                      engine = new_execution_engine()) {
  validate_execution_checkpoint(checkpoint, registry = registry)
  full_plan <- plan_analysis_execution(
    registry, checkpoint$analysis$config, selected = checkpoint$plan_order
  )
  remaining_plan <- checkpoint_remaining_plan(full_plan, checkpoint$completed)
  if (!length(remaining_plan$order)) {
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
