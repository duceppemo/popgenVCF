continuous_benchmark_scalar <- function(x, label, lower = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  value <- trimws(x)
  if (isTRUE(lower)) tolower(value) else value
}

continuous_benchmark_positive_integer <- function(x, label) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop(label, " must be a positive whole number", call. = FALSE)
  }
  as.integer(x)
}

continuous_benchmark_observation_key <- function(observation) {
  paste(
    observation$benchmark_id,
    observation$module,
    observation$dataset_tier,
    observation$threads,
    sep = "::"
  )
}

continuous_benchmark_environment_digest <- function(environment) {
  digest::digest(environment, algo = "sha256", serialize = TRUE)
}

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
  ratios <- c(
    max_runtime_ratio = max_runtime_ratio,
    max_memory_ratio = max_memory_ratio,
    min_throughput_ratio = min_throughput_ratio,
    min_scaling_efficiency = min_scaling_efficiency
  )
  if (!is.numeric(ratios) || anyNA(ratios) || any(!is.finite(ratios)) || any(ratios <= 0)) {
    stop("performance budget ratios must be positive finite values", call. = FALSE)
  }
  budget <- structure(list(
    schema_version = "1.1",
    id = continuous_benchmark_scalar(id, "id", lower = TRUE),
    max_runtime_ratio = as.numeric(max_runtime_ratio),
    max_memory_ratio = as.numeric(max_memory_ratio),
    min_throughput_ratio = as.numeric(min_throughput_ratio),
    min_scaling_efficiency = as.numeric(min_scaling_efficiency),
    minimum_repetitions = continuous_benchmark_positive_integer(
      minimum_repetitions, "minimum_repetitions"
    )
  ), class = "PopgenVCFReleasePerformanceBudget")
  validate_release_performance_budget(budget)
  budget
}

#' Validate a release performance budget
#' @param budget Release performance budget.
#' @return `budget`, invisibly.
#' @export
validate_release_performance_budget <- function(budget) {
  if (!inherits(budget, "PopgenVCFReleasePerformanceBudget")) {
    stop("budget must be a PopgenVCFReleasePerformanceBudget", call. = FALSE)
  }
  required <- c("schema_version", "id", "max_runtime_ratio", "max_memory_ratio",
    "min_throughput_ratio", "min_scaling_efficiency", "minimum_repetitions")
  if (!all(required %in% names(budget)) || !budget$schema_version %in% c("1.0", "1.1")) {
    stop("invalid release performance budget schema", call. = FALSE)
  }
  continuous_benchmark_scalar(budget$id, "id")
  ratios <- unlist(budget[c("max_runtime_ratio", "max_memory_ratio",
    "min_throughput_ratio", "min_scaling_efficiency")], use.names = FALSE)
  if (!is.numeric(ratios) || anyNA(ratios) || any(!is.finite(ratios)) || any(ratios <= 0)) {
    stop("performance budget ratios must be positive finite values", call. = FALSE)
  }
  continuous_benchmark_positive_integer(budget$minimum_repetitions, "minimum_repetitions")
  invisible(budget)
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
  metrics <- c(
    runtime_seconds = runtime_seconds,
    peak_memory_mb = peak_memory_mb,
    throughput = throughput,
    scaling_efficiency = scaling_efficiency
  )
  if (!is.numeric(metrics) || anyNA(metrics) || any(!is.finite(metrics)) || any(metrics < 0)) {
    stop("benchmark metrics must be nonnegative finite values", call. = FALSE)
  }
  if (!is.character(git_sha) || length(git_sha) != 1L || is.na(git_sha) ||
      !grepl("^[0-9a-f]{40}$", git_sha)) {
    stop("git_sha must be a full lowercase Git SHA", call. = FALSE)
  }
  if (!is.list(environment) || (length(environment) &&
      (is.null(names(environment)) || anyNA(names(environment)) || any(!nzchar(names(environment)))))) {
    stop("environment must be a named list", call. = FALSE)
  }
  observation <- structure(list(
    schema_version = "1.1",
    benchmark_id = continuous_benchmark_scalar(benchmark_id, "benchmark_id", lower = TRUE),
    module = continuous_benchmark_scalar(module, "module", lower = TRUE),
    dataset_tier = match.arg(dataset_tier, c("synthetic", "canonical", "medium", "large")),
    release = continuous_benchmark_scalar(release, "release"),
    git_sha = git_sha,
    runtime_seconds = as.numeric(runtime_seconds),
    peak_memory_mb = as.numeric(peak_memory_mb),
    throughput = as.numeric(throughput),
    scaling_efficiency = as.numeric(scaling_efficiency),
    threads = continuous_benchmark_positive_integer(threads, "threads"),
    repetitions = continuous_benchmark_positive_integer(repetitions, "repetitions"),
    environment = environment[sort(names(environment))]
  ), class = "PopgenVCFContinuousBenchmarkObservation")
  validate_continuous_benchmark_observation(observation)
  observation
}

#' Validate a continuous benchmark observation
#' @param observation Observation object.
#' @return `observation`, invisibly.
#' @export
validate_continuous_benchmark_observation <- function(observation) {
  if (!inherits(observation, "PopgenVCFContinuousBenchmarkObservation")) {
    stop("observation must be a PopgenVCFContinuousBenchmarkObservation", call. = FALSE)
  }
  required <- c("schema_version", "benchmark_id", "module", "dataset_tier", "release",
    "git_sha", "runtime_seconds", "peak_memory_mb", "throughput",
    "scaling_efficiency", "threads", "repetitions", "environment")
  if (!all(required %in% names(observation)) || !observation$schema_version %in% c("1.0", "1.1")) {
    stop("invalid continuous benchmark observation schema", call. = FALSE)
  }
  continuous_benchmark_scalar(observation$benchmark_id, "benchmark_id")
  continuous_benchmark_scalar(observation$module, "module")
  continuous_benchmark_scalar(observation$release, "release")
  if (!observation$dataset_tier %in% c("synthetic", "canonical", "medium", "large")) {
    stop("invalid benchmark dataset tier", call. = FALSE)
  }
  if (!is.character(observation$git_sha) || length(observation$git_sha) != 1L ||
      is.na(observation$git_sha) || !grepl("^[0-9a-f]{40}$", observation$git_sha)) {
    stop("git_sha must be a full lowercase Git SHA", call. = FALSE)
  }
  metrics <- unlist(observation[c("runtime_seconds", "peak_memory_mb", "throughput",
    "scaling_efficiency")], use.names = FALSE)
  if (!is.numeric(metrics) || anyNA(metrics) || any(!is.finite(metrics)) || any(metrics < 0)) {
    stop("benchmark metrics must be nonnegative finite values", call. = FALSE)
  }
  continuous_benchmark_positive_integer(observation$threads, "threads")
  continuous_benchmark_positive_integer(observation$repetitions, "repetitions")
  if (!is.list(observation$environment) || (length(observation$environment) &&
      (is.null(names(observation$environment)) || anyNA(names(observation$environment)) ||
       any(!nzchar(names(observation$environment)))))) {
    stop("environment must be a named list", call. = FALSE)
  }
  if (!identical(names(observation$environment), sort(names(observation$environment)))) {
    stop("environment metadata must be deterministically ordered", call. = FALSE)
  }
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
  validate_release_performance_budget(budget)

  current_key <- continuous_benchmark_observation_key(current)
  baseline_key <- continuous_benchmark_observation_key(baseline)
  if (!identical(current_key, baseline_key)) {
    stop("benchmark observations are not identity-compatible", call. = FALSE)
  }

  safe_ratio <- function(numerator, denominator) {
    if (denominator <= 0) NA_real_ else numerator / denominator
  }
  ratios <- c(
    runtime = safe_ratio(current$runtime_seconds, baseline$runtime_seconds),
    memory = safe_ratio(current$peak_memory_mb, baseline$peak_memory_mb),
    throughput = safe_ratio(current$throughput, baseline$throughput),
    scaling_efficiency = current$scaling_efficiency
  )
  checks <- data.frame(
    metric = names(ratios),
    value = unname(ratios),
    threshold = c(
      budget$max_runtime_ratio,
      budget$max_memory_ratio,
      budget$min_throughput_ratio,
      budget$min_scaling_efficiency
    ),
    comparator = c("<=", "<=", ">=", ">="),
    stringsAsFactors = FALSE
  )
  checks$passed <- c(
    !is.na(ratios[["runtime"]]) && ratios[["runtime"]] <= budget$max_runtime_ratio,
    !is.na(ratios[["memory"]]) && ratios[["memory"]] <= budget$max_memory_ratio,
    !is.na(ratios[["throughput"]]) && ratios[["throughput"]] >= budget$min_throughput_ratio,
    ratios[["scaling_efficiency"]] >= budget$min_scaling_efficiency
  )

  repetitions_complete <- current$repetitions >= budget$minimum_repetitions &&
    baseline$repetitions >= budget$minimum_repetitions
  environment_compatible <- identical(current$environment, baseline$environment)
  metrics_comparable <- all(is.finite(ratios))
  evidence_complete <- repetitions_complete && environment_compatible && metrics_comparable
  status <- if (!evidence_complete) {
    "insufficient-evidence"
  } else if (all(checks$passed)) {
    "passed"
  } else {
    "failed"
  }

  comparison <- structure(list(
    schema_version = "1.1",
    observation_key = current_key,
    benchmark_id = current$benchmark_id,
    module = current$module,
    dataset_tier = current$dataset_tier,
    threads = current$threads,
    current_release = current$release,
    baseline_release = baseline$release,
    current_git_sha = current$git_sha,
    baseline_git_sha = baseline$git_sha,
    budget_id = budget$id,
    checks = checks,
    repetitions_complete = repetitions_complete,
    environment_compatible = environment_compatible,
    metrics_comparable = metrics_comparable,
    evidence_complete = evidence_complete,
    status = status,
    release_ready = identical(status, "passed")
  ), class = "PopgenVCFContinuousBenchmarkComparison")
  validate_continuous_benchmark_comparison(comparison)
  comparison
}

#' Validate a continuous benchmark comparison
#' @param comparison Benchmark comparison.
#' @return `comparison`, invisibly.
#' @export
validate_continuous_benchmark_comparison <- function(comparison) {
  if (!inherits(comparison, "PopgenVCFContinuousBenchmarkComparison")) {
    stop("comparison must be a PopgenVCFContinuousBenchmarkComparison", call. = FALSE)
  }
  required <- c("schema_version", "observation_key", "benchmark_id", "module",
    "dataset_tier", "threads", "current_release", "baseline_release",
    "current_git_sha", "baseline_git_sha", "budget_id", "checks",
    "repetitions_complete", "environment_compatible", "metrics_comparable",
    "evidence_complete", "status", "release_ready")
  if (!all(required %in% names(comparison)) || !identical(comparison$schema_version, "1.1")) {
    stop("invalid continuous benchmark comparison schema", call. = FALSE)
  }
  continuous_benchmark_scalar(comparison$benchmark_id, "benchmark_id")
  continuous_benchmark_scalar(comparison$module, "module")
  continuous_benchmark_scalar(comparison$current_release, "current_release")
  continuous_benchmark_scalar(comparison$baseline_release, "baseline_release")
  continuous_benchmark_scalar(comparison$budget_id, "budget_id")
  if (!comparison$dataset_tier %in% c("synthetic", "canonical", "medium", "large")) {
    stop("invalid benchmark comparison dataset tier", call. = FALSE)
  }
  threads <- continuous_benchmark_positive_integer(comparison$threads, "threads")
  expected_key <- paste(
    comparison$benchmark_id,
    comparison$module,
    comparison$dataset_tier,
    threads,
    sep = "::"
  )
  if (!identical(comparison$observation_key, expected_key)) {
    stop("continuous benchmark comparison identity is inconsistent", call. = FALSE)
  }
  for (field in c("current_git_sha", "baseline_git_sha")) {
    value <- comparison[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !grepl("^[0-9a-f]{40}$", value)) {
      stop(field, " must be a full lowercase Git SHA", call. = FALSE)
    }
  }
  if (!is.data.frame(comparison$checks) ||
      !all(c("metric", "value", "threshold", "comparator", "passed") %in% names(comparison$checks)) ||
      !identical(as.character(comparison$checks$metric),
        c("runtime", "memory", "throughput", "scaling_efficiency")) ||
      !is.logical(comparison$checks$passed) || length(comparison$checks$passed) != 4L ||
      anyNA(comparison$checks$passed)) {
    stop("invalid continuous benchmark comparison checks", call. = FALSE)
  }
  logical_fields <- c("repetitions_complete", "environment_compatible",
    "metrics_comparable", "evidence_complete", "release_ready")
  if (any(!vapply(comparison[logical_fields], function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }, logical(1)))) {
    stop("invalid continuous benchmark comparison state", call. = FALSE)
  }
  expected_complete <- comparison$repetitions_complete &&
    comparison$environment_compatible && comparison$metrics_comparable
  expected_status <- if (!expected_complete) {
    "insufficient-evidence"
  } else if (all(comparison$checks$passed)) {
    "passed"
  } else {
    "failed"
  }
  if (!comparison$status %in% c("passed", "failed", "insufficient-evidence") ||
      !identical(comparison$evidence_complete, expected_complete) ||
      !identical(comparison$status, expected_status) ||
      !identical(comparison$release_ready, identical(expected_status, "passed"))) {
    stop("continuous benchmark comparison state is inconsistent", call. = FALSE)
  }
  invisible(comparison)
}

#' Write deterministic continuous benchmark evidence
#'
#' @param observations List of observations.
#' @param comparisons Optional list of comparisons.
#' @param output_dir Destination directory.
#' @param require_release_ready Fail when comparison evidence is absent or not release ready.
#' @return Named normalized evidence paths.
#' @export
write_continuous_benchmark_evidence <- function(observations, comparisons = list(),
    output_dir, require_release_ready = FALSE) {
  if (!is.list(observations) || !length(observations)) {
    stop("observations must be a non-empty list", call. = FALSE)
  }
  lapply(observations, validate_continuous_benchmark_observation)
  observation_keys <- vapply(observations, continuous_benchmark_observation_key, character(1))
  if (anyDuplicated(observation_keys)) {
    stop("continuous benchmark observation identities must be unique", call. = FALSE)
  }
  observations <- observations[order(observation_keys)]
  observation_keys <- sort(observation_keys)

  if (!is.list(comparisons)) {
    stop("comparisons must be a list", call. = FALSE)
  }
  if (length(comparisons)) {
    lapply(comparisons, validate_continuous_benchmark_comparison)
    comparison_keys <- vapply(comparisons, `[[`, character(1), "observation_key")
    if (anyDuplicated(comparison_keys)) {
      stop("continuous benchmark comparison identities must be unique", call. = FALSE)
    }
    if (!all(comparison_keys %in% observation_keys)) {
      stop("every comparison must correspond to a supplied current observation", call. = FALSE)
    }
    for (index in seq_along(comparisons)) {
      comparison <- comparisons[[index]]
      observation <- observations[[match(comparison$observation_key, observation_keys)]]
      if (!identical(comparison$current_release, observation$release) ||
          !identical(comparison$current_git_sha, observation$git_sha)) {
        stop("every comparison must identify the exact supplied current observation", call. = FALSE)
      }
    }
    comparisons <- comparisons[order(comparison_keys)]
  }
  if (isTRUE(require_release_ready) &&
      (!length(comparisons) || !all(vapply(comparisons, `[[`, logical(1), "release_ready")))) {
    stop("continuous benchmark evidence is not release ready", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  observation_table <- do.call(rbind, lapply(observations, function(x) data.frame(
    observation_key = continuous_benchmark_observation_key(x),
    benchmark_id = x$benchmark_id,
    module = x$module,
    dataset_tier = x$dataset_tier,
    release = x$release,
    git_sha = x$git_sha,
    runtime_seconds = x$runtime_seconds,
    peak_memory_mb = x$peak_memory_mb,
    throughput = x$throughput,
    scaling_efficiency = x$scaling_efficiency,
    threads = x$threads,
    repetitions = x$repetitions,
    environment_sha256 = continuous_benchmark_environment_digest(x$environment),
    stringsAsFactors = FALSE
  )))
  tsv <- file.path(output_dir, "continuous_benchmarks.tsv")
  json <- file.path(output_dir, "continuous_benchmarks.json")
  report <- file.path(output_dir, "continuous_benchmark_summary.md")
  data.table::fwrite(observation_table, tsv, sep = "\t", quote = FALSE)
  jsonlite::write_json(list(
    schema_version = "1.1",
    release_ready = length(comparisons) > 0L &&
      all(vapply(comparisons, `[[`, logical(1), "release_ready")),
    observations = lapply(observations, unclass),
    comparisons = lapply(comparisons, unclass)
  ), json, auto_unbox = TRUE, pretty = TRUE, digits = 17, na = "null")
  statuses <- if (length(comparisons)) {
    vapply(comparisons, `[[`, character(1), "status")
  } else {
    "no-baseline"
  }
  writeLines(c(
    "# Continuous release benchmarking", "",
    paste("Observations:", nrow(observation_table)),
    paste("Comparisons:", length(comparisons)),
    paste("Comparison status:", paste(statuses, collapse = ", ")), "",
    "Evidence records runtime, peak memory, throughput, thread scaling,",
    "dataset tier, repetitions, release identity, Git SHA, and environment provenance."
  ), report, useBytes = TRUE)
  c(tsv = normalizePath(tsv), json = normalizePath(json), report = normalizePath(report))
}
