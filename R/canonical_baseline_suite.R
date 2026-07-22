#' Create a canonical-suite baseline validation callback
#'
#' @param baseline_registry Canonical baseline registry.
#' @param observe Function accepting `(descriptor, directory)` and returning a
#'   named list keyed by baseline metric identifier.
#' @return A function compatible with [register_canonical_validation()].
#' @export
canonical_baseline_validation <- function(baseline_registry, observe) {
  validate_canonical_baseline_registry(baseline_registry)
  if (!is.function(observe)) stop("observe must be a function", call. = FALSE)
  force(baseline_registry)
  force(observe)
  function(descriptor, directory) {
    observations <- observe(descriptor, directory)
    result <- evaluate_canonical_baselines(
      baseline_registry, observations, dataset_id = descriptor$id)
    table <- canonical_baseline_table(result)
    data.frame(
      check = paste(table$analysis, table$metric_id, sep = ":"),
      passed = table$passed,
      expected = table$expected,
      observed = table$observed,
      deviation = table$deviation,
      tolerance = table$tolerance,
      comparator = table$comparator,
      baseline_version = table$baseline_version,
      stringsAsFactors = FALSE
    )
  }
}

#' Summarize baseline coverage
#'
#' @param registry Canonical baseline registry.
#' @return Deterministically ordered dataset-by-analysis metric counts.
#' @export
canonical_baseline_coverage <- function(registry) {
  validate_canonical_baseline_registry(registry)
  if (!length(registry$metrics)) return(data.frame(
    dataset_id = character(), analysis = character(), metrics = integer(),
    stringsAsFactors = FALSE))
  rows <- lapply(registry$metrics, function(metric) data.frame(
    dataset_id = metric$dataset_id, analysis = metric$analysis,
    metric_id = metric$id, stringsAsFactors = FALSE))
  raw <- do.call(rbind, rows)
  out <- stats::aggregate(metric_id ~ dataset_id + analysis, raw, length)
  names(out)[names(out) == "metric_id"] <- "metrics"
  out <- out[order(out$dataset_id, out$analysis), , drop = FALSE]
  rownames(out) <- NULL
  out
}
