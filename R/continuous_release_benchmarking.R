#' Create a release performance budget
#'
#' @param id Stable budget identifier.
#' @param max_runtime_ratio,max_memory_ratio Maximum allowed ratios versus baseline.
#' @param min_throughput_ratio,min_scaling_efficiency Minimum allowed ratios.
#' @param minimum_repetitions Minimum repetitions required for gating evidence.
#' @return A validated `PopgenVCFReleasePerformanceBudget`.
#' @export
new_release_performance_budget <- function(
    id, max_runtime_ratio = 1.10, max_memory_ratio = 1.10,
    min_throughput_ratio = 0.90, min_scaling_efficiency = 0.70,
    minimum_repetitions = 5L) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  numeric_fields <- c(max_runtime_ratio, max_memory_ratio,
    min_throughput_ratio, min_scaling_efficiency)
  if (anyNA(numeric_fields) || any(!is.finite(numeric_fields)) || any(numeric_fields <= 0))
    stop("performance budget ratios must be positive finite values", call. = FALSE)
  if (length(minimum_repetitions) != 1L || is.na(minimum_repetitions) || minimum_repetitions < 1L)
    stop("minimum_repetitions must be a positive integer", call. = FALSE)
  structure(list(
    schema_version = "1.0", id = tolower(scalar(id, "id")),
    max_runtime_ratio = as.numeric(max_runtime_ratio),
    max_memory_ratio = as.numeric(max_memory_ratio),
    min_throughput_ratio = as.numeric(min_throughput_ratio),
    min_scaling_efficiency = as.numeric(min_scaling_efficiency),
    minimum_repetitions = as.integer(minimum_repetitions)
  ), class = "PopgenVCFReleasePerformanceBudget")
}

#' Create a continuous release benchmark observation
#'
#' @param benchmark_id,module,dataset_tier Stable identifiers.
#' @param release,git_sha Release and source identifiers.
#' @param runtime_seconds,peak_memory_mb,throughput,scaling_efficiency Metrics.
#' @param threads,repetitions Execution parameters.
#' @param environment Named environment fingerprint.
#' @return A `PopgenVCFContinuousBenchmarkObservation`.
#' @export
new_continuous_benchmark_observation <- function(
    benchmark_id, module, dataset_tier, release, git_sha,
    runtime_seconds, peak_memory_mb, throughput, scaling_efficiency,
    threads = 1L, repetitions = 5L, environment = list()) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  metrics <- c(runtime_seconds, peak_memory_mb, throughput, scaling_efficiency)
  if (anyNA(metrics) || any(!is.finite(metrics)) || any(metrics < 0))
    stop("benchmark metrics must be nonnegative finite values", call. = FALSE)
  if (!grepl("^[0-9a-f]{40}$", git_sha))
    stop("git_sha must be a full lowercase Git SHA", call. = FALSE)
  if (!is.list(environment) || (length(environment) && is.null(names(environment))))
    stop("environment must be a named list", call. = FALSE)
  x <- structure(list(
    schema_version = "1.0",
    benchmark_id = tolower(scalar(benchmark_id, "benchmark_id")),
    module = tolower(scalar(module, "module")),
    dataset_tier = match.arg(dataset_tier, c("synthetic", "canonical", "medium", "large")),
    release = scalar(release, "release"), git_sha = git_sha,
    runtime_seconds = as.numeric(runtime_seconds),
    peak_memory_mb = as.numeric(peak_memory_mb),
    throughput = as.numeric(throughput),
    scaling_efficiency = as.numeric(scaling_efficiency),
    threads = as.integer(threads), repetitions = as.integer(repetitions),
    environment = environment[sort(names(environment))]
  ), class = "PopgenVCFContinuousBenchmarkObservation")
  validate_continuous_benchmark_observation(x)
}

#' Validate a continuous benchmark observation
#' @param observation Observation object.
#' @return `observation`, invisibly.
#' @export
validate_continuous_benchmark_observation <- function(observation) {
  if (!inherits(observation, "PopgenVCFContinuousBenchmarkObservation"))
    stop("observation must be a PopgenVCFContinuousBenchmarkObservation", call. = FALSE)
  required <- c("schema_version", "benchmark_id", "module", "dataset_tier", "release",
    "git_sha", "runtime_seconds", "peak_memory_mb", "throughput",
    "scaling_efficiency", "threads", "repetitions", "environment")
  if (!all(required %in% names(observation)) || !identical(observation$schema_version, "1.0"))
    stop("invalid continuous benchmark observation schema", call. = FALSE)
  if (observation$threads < 1L || observation$repetitions < 1L)
    stop("threads and repetitions must be positive integers", call. = FALSE)
  invisible(observation)
}

#' Compare a release benchmark observation with an approved baseline
#'
#' @param current,baseline Benchmark observations.
#' @param budget Release performance budget.
#' @return A `PopgenVCFContinuousBenchmarkComparison`.
#' @export
compare_continuous_release_benchmark <- function(current, baseline, budget) {
  validate_continuous_benchmark_observation(current)
  validate_continuous_benchmark_observation(baseline)
  if (!inherits(budget, "PopgenVCFReleasePerformanceBudget"))
    stop("budget must be a PopgenVCFReleasePerformanceBudget", call. = FALSE)
  if (!identical(current$benchmark_id, baseline$benchmark_id))
    stop("benchmark identifiers differ", call. = FALSE)
  ratios <- c(
    runtime = current$runtime_seconds / max(baseline$runtime_seconds, .Machine$double.eps),
    memory = current$peak_memory_mb / max(baseline$peak_memory_mb, .Machine$double.eps),
    throughput = current$throughput / max(baseline$throughput, .Machine$double.eps),
    scaling_efficiency = current$scaling_efficiency
  )
  checks <- data.frame(
    metric = names(ratios), value = unname(ratios),
    threshold = c(budget$max_runtime_ratio, budget$max_memory_ratio,
      budget$min_throughput_ratio, budget$min_scaling_efficiency),
    comparator = c("<=", "<=", ">=", ">="), stringsAsFactors = FALSE
  )
  checks$passed <- c(
    ratios[["runtime"]] <= budget$max_runtime_ratio,
    ratios[["memory"]] <= budget$max_memory_ratio,
    ratios[["throughput"]] >= budget$min_throughput_ratio,
    ratios[["scaling_efficiency"]] >= budget$min_scaling_efficiency
  )
  enough_repetitions <- current$repetitions >= budget$minimum_repetitions
  status <- if (!enough_repetitions) "insufficient-evidence" else if (all(checks$passed)) "passed" else "failed"
  structure(list(schema_version = "1.0", benchmark_id = current$benchmark_id,
    current_release = current$release, baseline_release = baseline$release,
    budget_id = budget$id, checks = checks, status = status,
    release_ready = identical(status, "passed")),
    class = "PopgenVCFContinuousBenchmarkComparison")
}

#' Write deterministic continuous benchmark evidence
#'
#' @param observations List of observations.
#' @param comparisons Optional list of comparisons.
#' @param output_dir Destination directory.
#' @param require_release_ready Fail when a comparison is not release ready.
#' @return Named normalized evidence paths.
#' @export
write_continuous_benchmark_evidence <- function(observations, comparisons = list(),
    output_dir, require_release_ready = FALSE) {
  if (!is.list(observations) || !length(observations))
    stop("observations must be a non-empty list", call. = FALSE)
  lapply(observations, validate_continuous_benchmark_observation)
  if (isTRUE(require_release_ready) && length(comparisons) &&
      !all(vapply(comparisons, function(x) isTRUE(x$release_ready), logical(1))))
    stop("continuous benchmark evidence is not release ready", call. = FALSE)
  observations <- observations[order(vapply(observations, `[[`, character(1), "benchmark_id"))]
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  observation_table <- do.call(rbind, lapply(observations, function(x) data.frame(
    benchmark_id = x$benchmark_id, module = x$module, dataset_tier = x$dataset_tier,
    release = x$release, runtime_seconds = x$runtime_seconds,
    peak_memory_mb = x$peak_memory_mb, throughput = x$throughput,
    scaling_efficiency = x$scaling_efficiency, threads = x$threads,
    repetitions = x$repetitions, stringsAsFactors = FALSE)))
  tsv <- file.path(output_dir, "continuous_benchmarks.tsv")
  json <- file.path(output_dir, "continuous_benchmarks.json")
  report <- file.path(output_dir, "continuous_benchmark_summary.md")
  data.table::fwrite(observation_table, tsv, sep = "\t", quote = FALSE)
  jsonlite::write_json(list(schema_version = "1.0", observations = lapply(observations, unclass),
    comparisons = lapply(comparisons, unclass)), json, auto_unbox = TRUE,
    pretty = TRUE, digits = 17, na = "null")
  statuses <- if (length(comparisons)) vapply(comparisons, `[[`, character(1), "status") else "no-baseline"
  writeLines(c("# Continuous release benchmarking", "",
    paste("Observations:", nrow(observation_table)),
    paste("Comparison status:", paste(statuses, collapse = ", ")),
    "", "Evidence records runtime, peak memory, throughput, thread scaling,",
    "dataset tier, repetitions, release identity, Git SHA, and environment provenance."),
    report, useBytes = TRUE)
  c(tsv = normalizePath(tsv), json = normalizePath(json), report = normalizePath(report))
}
