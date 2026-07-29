# Late-load consistency guards for aggregate drift severity. These wrappers keep
# the core assessment tables unchanged while ensuring maxima are selected by
# severity value rather than by the row position returned by which.max().
.drift_maximum_classification <- function(classes) {
  severity <- c(stable = 0L, minor = 1L, moderate = 2L, major = 3L, breaking = 4L)
  if (!length(classes)) return("stable")
  names(severity)[max(unname(severity[classes])) + 1L]
}

#' Assess drift between two canonical baseline snapshots
#'
#' @param previous Earlier canonical baseline snapshot.
#' @param current Later canonical baseline snapshot.
#' @param profile Drift threshold profile.
#' @return A `PopgenVCFCanonicalDriftAssessment`.
#' @export
assess_canonical_baseline_drift <- function(previous, current,
                                            profile = new_canonical_drift_profile()) {
  assessment <- .assess_canonical_baseline_drift_core(previous, current, profile)
  assessment$classification <- .drift_maximum_classification(
    assessment$table$classification)
  assessment
}

#' Summarize canonical drift by dataset and analysis
#' @param assessment Canonical drift assessment or drift history.
#' @return Deterministic summary table.
#' @export
canonical_drift_summary <- function(assessment) {
  out <- .canonical_drift_summary_core(assessment)
  table <- if (inherits(assessment, "PopgenVCFCanonicalDriftAssessment"))
    canonical_drift_table(assessment) else if (inherits(assessment, "PopgenVCFCanonicalDriftHistory"))
    assessment$table else stop("assessment must be a drift assessment or history", call. = FALSE)
  if (!nrow(out)) return(out)
  out$maximum_classification <- vapply(seq_len(nrow(out)), function(i) {
    selected <- table$dataset_id == out$dataset_id[i] & table$analysis == out$analysis[i]
    .drift_maximum_classification(table$classification[selected])
  }, character(1))
  out
}
