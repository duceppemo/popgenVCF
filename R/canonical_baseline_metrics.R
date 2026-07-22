#' Create a canonical baseline metric
#'
#' @param id Stable metric identifier.
#' @param dataset_id Canonical dataset identifier.
#' @param analysis Analysis producing the metric.
#' @param expected Expected scalar or vector value.
#' @param comparator One of `exact`, `absolute`, `relative`, `set`, or `distribution`.
#' @param tolerance Non-negative numeric tolerance where applicable.
#' @param version Baseline version.
#' @param rationale Scientific justification for the comparator and tolerance.
#' @param provenance Optional named provenance metadata.
#' @return A validated `PopgenVCFCanonicalBaselineMetric`.
#' @export
new_canonical_baseline_metric <- function(id, dataset_id, analysis, expected,
                                          comparator = "exact", tolerance = 0,
                                          version = "1.0", rationale,
                                          provenance = list()) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  comparator <- match.arg(comparator, c("exact", "absolute", "relative", "set", "distribution"))
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance < 0)
    stop("tolerance must be one non-negative number", call. = FALSE)
  if (!is.atomic(expected) || !length(expected) || anyNA(expected))
    stop("expected must be a non-empty atomic value without missing values", call. = FALSE)
  if (comparator %in% c("absolute", "relative", "distribution") && !is.numeric(expected))
    stop(comparator, " baselines require numeric expected values", call. = FALSE)
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance))))
    stop("provenance must be a named list", call. = FALSE)
  validate_canonical_baseline_metric(structure(list(
    schema_version = "1.0", id = tolower(scalar(id, "id")),
    dataset_id = tolower(scalar(dataset_id, "dataset_id")),
    analysis = tolower(scalar(analysis, "analysis")), expected = expected,
    comparator = comparator, tolerance = as.numeric(tolerance),
    version = scalar(version, "version"), rationale = scalar(rationale, "rationale"),
    provenance = provenance
  ), class = "PopgenVCFCanonicalBaselineMetric"))
}

#' Validate a canonical baseline metric
#' @param metric Baseline metric.
#' @return `metric`, invisibly.
#' @export
validate_canonical_baseline_metric <- function(metric) {
  if (!inherits(metric, "PopgenVCFCanonicalBaselineMetric"))
    stop("metric must be a PopgenVCFCanonicalBaselineMetric", call. = FALSE)
  required <- c("schema_version", "id", "dataset_id", "analysis", "expected",
                "comparator", "tolerance", "version", "rationale", "provenance")
  if (!all(required %in% names(metric)) || !identical(metric$schema_version, "1.0"))
    stop("invalid canonical baseline metric schema", call. = FALSE)
  if (!metric$comparator %in% c("exact", "absolute", "relative", "set", "distribution"))
    stop("unsupported baseline comparator", call. = FALSE)
  invisible(metric)
}

#' Create a canonical baseline registry
#' @param metrics Optional list of baseline metrics.
#' @return A validated `PopgenVCFCanonicalBaselineRegistry`.
#' @export
new_canonical_baseline_registry <- function(metrics = list()) {
  registry <- structure(list(schema_version = "1.0", metrics = list()),
                        class = "PopgenVCFCanonicalBaselineRegistry")
  for (metric in metrics) registry <- register_canonical_baseline_metric(registry, metric)
  validate_canonical_baseline_registry(registry)
}

#' Register a canonical baseline metric
#' @param registry Baseline registry.
#' @param metric Baseline metric.
#' @param replace Permit replacement of an existing metric identifier.
#' @return Updated registry.
#' @export
register_canonical_baseline_metric <- function(registry, metric, replace = FALSE) {
  validate_canonical_baseline_registry(registry)
  validate_canonical_baseline_metric(metric)
  if (metric$id %in% names(registry$metrics) && !isTRUE(replace))
    stop("canonical baseline metric is already registered: ", metric$id, call. = FALSE)
  registry$metrics[[metric$id]] <- metric
  registry$metrics <- registry$metrics[sort(names(registry$metrics))]
  validate_canonical_baseline_registry(registry)
}

#' Validate a canonical baseline registry
#' @param registry Baseline registry.
#' @return `registry`, invisibly.
#' @export
validate_canonical_baseline_registry <- function(registry) {
  if (!inherits(registry, "PopgenVCFCanonicalBaselineRegistry"))
    stop("registry must be a PopgenVCFCanonicalBaselineRegistry", call. = FALSE)
  if (!identical(registry$schema_version, "1.0") || !is.list(registry$metrics))
    stop("invalid canonical baseline registry schema", call. = FALSE)
  if (length(registry$metrics) && is.null(names(registry$metrics)))
    stop("baseline metrics must be a named list", call. = FALSE)
  if (anyDuplicated(names(registry$metrics))) stop("baseline metric identifiers must be unique", call. = FALSE)
  for (id in names(registry$metrics)) {
    validate_canonical_baseline_metric(registry$metrics[[id]])
    if (!identical(id, registry$metrics[[id]]$id)) stop("baseline registry key mismatch", call. = FALSE)
  }
  invisible(registry)
}

.baseline_value_text <- function(x) {
  paste(format(x, digits = 17L, trim = TRUE, scientific = FALSE), collapse = "|")
}

.baseline_within_tolerance <- function(deviation, tolerance) {
  guard <- 8 * .Machine$double.eps * max(1, abs(deviation), abs(tolerance))
  isTRUE(deviation <= tolerance + guard)
}

#' Compare an observed value with a canonical baseline
#' @param metric Baseline metric.
#' @param observed Observed scalar or vector value.
#' @return One-row deterministic comparison table.
#' @export
compare_canonical_baseline_metric <- function(metric, observed) {
  validate_canonical_baseline_metric(metric)
  expected <- metric$expected
  comparator <- metric$comparator
  tolerance <- metric$tolerance
  passed <- FALSE
  deviation <- NA_real_
  detail <- NA_character_
  if (anyNA(observed) || !length(observed)) {
    detail <- "observed value is empty or contains missing values"
  } else if (comparator == "exact") {
    passed <- identical(expected, observed)
    detail <- if (passed) "identical" else "not identical"
  } else if (comparator == "absolute") {
    if (!is.numeric(observed) || length(observed) != length(expected)) {
      detail <- "numeric shape mismatch"
    } else {
      deviation <- max(abs(observed - expected))
      passed <- .baseline_within_tolerance(deviation, tolerance)
      detail <- "maximum absolute deviation"
    }
  } else if (comparator == "relative") {
    if (!is.numeric(observed) || length(observed) != length(expected)) {
      detail <- "numeric shape mismatch"
    } else {
      scale <- pmax(abs(expected), .Machine$double.eps)
      deviation <- max(abs(observed - expected) / scale)
      passed <- .baseline_within_tolerance(deviation, tolerance)
      detail <- "maximum relative deviation"
    }
  } else if (comparator == "set") {
    passed <- setequal(expected, observed)
    deviation <- length(setdiff(union(expected, observed), intersect(expected, observed)))
    detail <- "symmetric set difference size"
  } else {
    if (!is.numeric(observed) || length(observed) < 2L || length(expected) < 2L) {
      detail <- "distribution comparison requires numeric vectors of length at least two"
    } else {
      probabilities <- seq(0, 1, length.out = 11L)
      expected_q <- unname(stats::quantile(expected, probabilities, names = FALSE, type = 8))
      observed_q <- unname(stats::quantile(observed, probabilities, names = FALSE, type = 8))
      scale <- max(diff(range(expected)), .Machine$double.eps)
      deviation <- max(abs(observed_q - expected_q)) / scale
      passed <- .baseline_within_tolerance(deviation, tolerance)
      detail <- "normalized maximum decile deviation"
    }
  }
  data.frame(
    metric_id = metric$id, dataset_id = metric$dataset_id, analysis = metric$analysis,
    baseline_version = metric$version, comparator = comparator, tolerance = tolerance,
    expected = .baseline_value_text(expected), observed = .baseline_value_text(observed),
    deviation = deviation, passed = passed, detail = detail,
    rationale = metric$rationale, stringsAsFactors = FALSE
  )
}

#' Evaluate registered canonical baselines
#' @param registry Baseline registry.
#' @param observed Named list keyed by metric identifier.
#' @param dataset_id Optional dataset filter.
#' @return A `PopgenVCFCanonicalBaselineResult`.
#' @export
evaluate_canonical_baselines <- function(registry, observed, dataset_id = NULL) {
  validate_canonical_baseline_registry(registry)
  if (!is.list(observed) || is.null(names(observed)))
    stop("observed must be a named list keyed by metric identifier", call. = FALSE)
  ids <- sort(names(registry$metrics))
  if (!is.null(dataset_id)) {
    dataset_id <- tolower(as.character(dataset_id)[1L])
    ids <- ids[vapply(registry$metrics[ids], function(x) identical(x$dataset_id, dataset_id), logical(1))]
  }
  rows <- lapply(ids, function(id) {
    metric <- registry$metrics[[id]]
    if (!id %in% names(observed)) {
      out <- compare_canonical_baseline_metric(metric, numeric())
      out$detail <- "observed metric is missing"
      return(out)
    }
    compare_canonical_baseline_metric(metric, observed[[id]])
  })
  table <- if (length(rows)) do.call(rbind, rows) else data.frame(
    metric_id = character(), dataset_id = character(), analysis = character(),
    baseline_version = character(), comparator = character(), tolerance = numeric(),
    expected = character(), observed = character(), deviation = numeric(), passed = logical(),
    detail = character(), rationale = character(), stringsAsFactors = FALSE)
  rownames(table) <- NULL
  structure(list(schema_version = "1.0", table = table,
    passed = nrow(table) > 0L && all(table$passed)),
    class = "PopgenVCFCanonicalBaselineResult")
}

#' Return the canonical baseline comparison table
#' @param result Baseline result.
#' @return Deterministically ordered data frame.
#' @export
canonical_baseline_table <- function(result) {
  if (!inherits(result, "PopgenVCFCanonicalBaselineResult"))
    stop("result must be a PopgenVCFCanonicalBaselineResult", call. = FALSE)
  result$table[order(result$table$dataset_id, result$table$analysis, result$table$metric_id), , drop = FALSE]
}

#' Write canonical baseline evidence
#' @param result Baseline result.
#' @param output_dir Evidence directory.
#' @return Named normalized output paths.
#' @export
write_canonical_baseline_evidence <- function(result, output_dir) {
  table <- canonical_baseline_table(result)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tsv <- file.path(output_dir, "canonical_baseline_metrics.tsv")
  json <- file.path(output_dir, "canonical_baseline_metrics.json")
  methods <- file.path(output_dir, "canonical_baseline_methods.md")
  data.table::fwrite(table, tsv, sep = "\t", quote = FALSE, na = "NA")
  jsonlite::write_json(list(schema_version = "1.0", passed = result$passed,
    metrics = table), json, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 17)
  writeLines(paste0("Canonical baseline evaluation compared ", nrow(table),
    " versioned metric(s); ", sum(table$passed), " passed and ", sum(!table$passed),
    " failed. Comparators and tolerances are recorded per metric with scientific rationale."),
    methods, useBytes = TRUE)
  c(tsv = normalizePath(tsv), json = normalizePath(json), methods = normalizePath(methods))
}
