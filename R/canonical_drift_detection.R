#' Create a canonical drift threshold profile
#'
#' @param minor Maximum normalized drift classified as minor.
#' @param moderate Maximum normalized drift classified as moderate.
#' @param major Maximum normalized drift classified as major.
#' @return A validated `PopgenVCFCanonicalDriftProfile`.
#' @export
new_canonical_drift_profile <- function(minor = 1, moderate = 2, major = 5) {
  values <- c(minor = minor, moderate = moderate, major = major)
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) ||
      any(values < 0) || !identical(unname(sort(values)), unname(values)) ||
      anyDuplicated(values)) {
    stop("drift thresholds must be finite, non-negative, unique, and increasing", call. = FALSE)
  }
  structure(list(schema_version = "1.0", thresholds = values),
            class = "PopgenVCFCanonicalDriftProfile")
}

#' Create a canonical baseline snapshot
#'
#' @param id Stable release or snapshot identifier.
#' @param registry Canonical baseline registry.
#' @param recorded_at ISO-8601 date or datetime.
#' @param provenance Optional named provenance metadata.
#' @return A validated `PopgenVCFCanonicalBaselineSnapshot`.
#' @export
new_canonical_baseline_snapshot <- function(id, registry, recorded_at, provenance = list()) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  validate_canonical_baseline_registry(registry)
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance))))
    stop("provenance must be a named list", call. = FALSE)
  structure(list(schema_version = "1.0", id = scalar(id, "id"),
    recorded_at = scalar(recorded_at, "recorded_at"), registry = registry,
    provenance = provenance), class = "PopgenVCFCanonicalBaselineSnapshot")
}

.validate_drift_snapshot <- function(snapshot) {
  if (!inherits(snapshot, "PopgenVCFCanonicalBaselineSnapshot") ||
      !identical(snapshot$schema_version, "1.0"))
    stop("snapshot must be a canonical baseline snapshot", call. = FALSE)
  validate_canonical_baseline_registry(snapshot$registry)
  invisible(snapshot)
}

.drift_within <- function(x, limit) {
  guard <- 8 * .Machine$double.eps * max(1, abs(x), abs(limit))
  isTRUE(x <= limit + guard)
}

.drift_class <- function(normalized, profile, incompatible = FALSE) {
  if (isTRUE(incompatible) || is.na(normalized) || is.infinite(normalized)) return("breaking")
  thresholds <- profile$thresholds
  if (.drift_within(normalized, 0)) "stable"
  else if (.drift_within(normalized, thresholds[["minor"]])) "minor"
  else if (.drift_within(normalized, thresholds[["moderate"]])) "moderate"
  else if (.drift_within(normalized, thresholds[["major"]])) "major"
  else "breaking"
}

.metric_drift <- function(previous, current, profile) {
  compatible <- identical(previous$id, current$id) &&
    identical(previous$dataset_id, current$dataset_id) &&
    identical(previous$analysis, current$analysis) &&
    identical(previous$comparator, current$comparator)
  if (!compatible) {
    return(list(magnitude = NA_real_, normalized = Inf, classification = "breaking",
      detail = "metric identity or comparator changed"))
  }
  comparator <- previous$comparator
  old <- previous$expected
  new <- current$expected
  magnitude <- NA_real_
  if (comparator == "exact") {
    magnitude <- if (identical(old, new)) 0 else Inf
  } else if (comparator == "set") {
    magnitude <- length(setdiff(union(old, new), intersect(old, new)))
  } else if (!is.numeric(old) || !is.numeric(new) || length(old) != length(new)) {
    return(list(magnitude = NA_real_, normalized = Inf, classification = "breaking",
      detail = "numeric baseline shape changed"))
  } else if (comparator == "absolute") {
    magnitude <- max(abs(new - old))
  } else if (comparator == "relative") {
    magnitude <- max(abs(new - old) / pmax(abs(old), .Machine$double.eps))
  } else {
    if (length(old) < 2L || length(new) < 2L) {
      return(list(magnitude = NA_real_, normalized = Inf, classification = "breaking",
        detail = "distribution baseline requires at least two values"))
    }
    probabilities <- seq(0, 1, length.out = 11L)
    old_q <- unname(stats::quantile(old, probabilities, names = FALSE, type = 8))
    new_q <- unname(stats::quantile(new, probabilities, names = FALSE, type = 8))
    magnitude <- max(abs(new_q - old_q)) / max(diff(range(old)), .Machine$double.eps)
  }
  reference <- max(previous$tolerance, current$tolerance)
  normalized <- if (magnitude == 0) 0 else if (reference > 0) magnitude / reference else Inf
  list(magnitude = magnitude, normalized = normalized,
    classification = .drift_class(normalized, profile),
    detail = "normalized against the larger approved tolerance")
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
  .validate_drift_snapshot(previous)
  .validate_drift_snapshot(current)
  if (!inherits(profile, "PopgenVCFCanonicalDriftProfile"))
    stop("profile must be a canonical drift profile", call. = FALSE)
  old_ids <- names(previous$registry$metrics)
  new_ids <- names(current$registry$metrics)
  ids <- sort(union(old_ids, new_ids))
  rows <- lapply(ids, function(id) {
    old <- previous$registry$metrics[[id]]
    new <- current$registry$metrics[[id]]
    if (is.null(old) || is.null(new)) {
      present <- if (is.null(old)) new else old
      return(data.frame(metric_id = id, dataset_id = present$dataset_id,
        analysis = present$analysis, previous_version = if (is.null(old)) NA_character_ else old$version,
        current_version = if (is.null(new)) NA_character_ else new$version,
        magnitude = NA_real_, normalized_drift = Inf, classification = "breaking",
        detail = if (is.null(old)) "metric added without historical baseline" else
          "metric removed from current baseline", stringsAsFactors = FALSE))
    }
    drift <- .metric_drift(old, new, profile)
    data.frame(metric_id = id, dataset_id = old$dataset_id, analysis = old$analysis,
      previous_version = old$version, current_version = new$version,
      magnitude = drift$magnitude, normalized_drift = drift$normalized,
      classification = drift$classification, detail = drift$detail,
      stringsAsFactors = FALSE)
  })
  table <- if (length(rows)) do.call(rbind, rows) else data.frame(
    metric_id = character(), dataset_id = character(), analysis = character(),
    previous_version = character(), current_version = character(), magnitude = numeric(),
    normalized_drift = numeric(), classification = character(), detail = character())
  rownames(table) <- NULL
  severity <- c(stable = 0L, minor = 1L, moderate = 2L, major = 3L, breaking = 4L)
  overall <- if (!nrow(table)) "stable" else names(severity)[which.max(severity[table$classification])]
  structure(list(schema_version = "1.0", previous_id = previous$id, current_id = current$id,
    profile = profile, table = table, classification = overall),
    class = "PopgenVCFCanonicalDriftAssessment")
}

#' Return a canonical drift assessment table
#' @param assessment Canonical drift assessment.
#' @return Deterministically ordered data frame.
#' @export
canonical_drift_table <- function(assessment) {
  if (!inherits(assessment, "PopgenVCFCanonicalDriftAssessment"))
    stop("assessment must be a canonical drift assessment", call. = FALSE)
  out <- assessment$table[order(assessment$table$dataset_id,
    assessment$table$analysis, assessment$table$metric_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Assess drift across an ordered snapshot history
#'
#' @param snapshots Ordered list of at least two canonical baseline snapshots.
#' @param profile Drift threshold profile.
#' @return A `PopgenVCFCanonicalDriftHistory`.
#' @export
canonical_drift_history <- function(snapshots, profile = new_canonical_drift_profile()) {
  if (!is.list(snapshots) || length(snapshots) < 2L)
    stop("snapshots must contain at least two ordered snapshots", call. = FALSE)
  lapply(snapshots, .validate_drift_snapshot)
  ids <- vapply(snapshots, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("snapshot identifiers must be unique", call. = FALSE)
  assessments <- lapply(seq_len(length(snapshots) - 1L), function(i)
    assess_canonical_baseline_drift(snapshots[[i]], snapshots[[i + 1L]], profile))
  rows <- lapply(seq_along(assessments), function(i) {
    x <- canonical_drift_table(assessments[[i]])
    x$from_snapshot <- assessments[[i]]$previous_id
    x$to_snapshot <- assessments[[i]]$current_id
    x$transition <- i
    x
  })
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  finite <- is.finite(table$normalized_drift)
  cumulative <- aggregate(table$normalized_drift[finite],
    by = list(metric_id = table$metric_id[finite]), FUN = sum)
  names(cumulative)[2L] <- "cumulative_normalized_drift"
  structure(list(schema_version = "1.0", snapshot_ids = ids,
    assessments = assessments, table = table, cumulative = cumulative),
    class = "PopgenVCFCanonicalDriftHistory")
}

#' Summarize canonical drift by dataset and analysis
#' @param assessment Canonical drift assessment or drift history.
#' @return Deterministic summary table.
#' @export
canonical_drift_summary <- function(assessment) {
  table <- if (inherits(assessment, "PopgenVCFCanonicalDriftAssessment"))
    canonical_drift_table(assessment) else if (inherits(assessment, "PopgenVCFCanonicalDriftHistory"))
    assessment$table else stop("assessment must be a drift assessment or history", call. = FALSE)
  severity <- c(stable = 0L, minor = 1L, moderate = 2L, major = 3L, breaking = 4L)
  keys <- unique(table[c("dataset_id", "analysis")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    selected <- table$dataset_id == keys$dataset_id[i] & table$analysis == keys$analysis[i]
    classes <- table$classification[selected]
    data.frame(dataset_id = keys$dataset_id[i], analysis = keys$analysis[i],
      metrics = sum(selected), stable = sum(classes == "stable"), minor = sum(classes == "minor"),
      moderate = sum(classes == "moderate"), major = sum(classes == "major"),
      breaking = sum(classes == "breaking"),
      maximum_classification = names(severity)[which.max(severity[classes])],
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(out$dataset_id, out$analysis), , drop = FALSE]
}

#' Write deterministic canonical drift evidence
#' @param assessment Canonical drift assessment or history.
#' @param output_dir Evidence directory.
#' @return Named normalized paths.
#' @export
write_canonical_drift_evidence <- function(assessment, output_dir) {
  table <- if (inherits(assessment, "PopgenVCFCanonicalDriftAssessment"))
    canonical_drift_table(assessment) else if (inherits(assessment, "PopgenVCFCanonicalDriftHistory"))
    assessment$table else stop("assessment must be a drift assessment or history", call. = FALSE)
  summary <- canonical_drift_summary(assessment)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(metrics = file.path(output_dir, "canonical_drift_metrics.tsv"),
    summary = file.path(output_dir, "canonical_drift_summary.tsv"),
    json = file.path(output_dir, "canonical_drift.json"),
    methods = file.path(output_dir, "canonical_drift_methods.md"))
  data.table::fwrite(table, paths[["metrics"]], sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(summary, paths[["summary"]], sep = "\t", quote = FALSE, na = "NA")
  jsonlite::write_json(list(schema_version = "1.0", metrics = table, summary = summary),
    paths[["json"]], auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 17)
  writeLines(paste0("Canonical drift analysis assessed ", nrow(table),
    " metric transition(s). Classifications are stable, minor, moderate, major, or breaking ",
    "relative to versioned approved tolerances."), paths[["methods"]], useBytes = TRUE)
  vapply(paths, normalizePath, character(1))
}
