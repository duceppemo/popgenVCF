performance_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

#' Capture a performance benchmark environment fingerprint
#' @return A named list describing the current runtime and host.
#' @export
performance_environment_fingerprint <- function() {
  info <- Sys.info()
  physical <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  logical <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  list(
    schema_version = "1.0",
    os = unname(info[["sysname"]] %||% .Platform$OS.type),
    release = unname(info[["release"]] %||% NA_character_),
    machine = unname(info[["machine"]] %||% R.version$arch),
    r_version = as.character(getRversion()), platform = R.version$platform,
    physical_cores = as.integer(physical), logical_cores = as.integer(logical),
    blas = extSoftVersion()[["BLAS"]] %||% NA_character_
  )
}

performance_fingerprint_id <- function(x) {
  if (!is.list(x) || (length(x) && is.null(names(x)))) {
    stop("fingerprint must be a named list", call. = FALSE)
  }
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

#' Create a performance benchmark specification
#'
#' @param id Stable benchmark identifier.
#' @param runner Function accepting `threads` and returning any value. The
#'   temporary directory is exposed through `POPGENVCF_PERFORMANCE_TEMP`.
#' @param threads Integer thread counts to benchmark.
#' @param warmup Number of unrecorded warmup iterations.
#' @param iterations Number of measured iterations per thread count.
#' @param seed Deterministic base seed.
#' @param runtime_regression,memory_regression,disk_regression Allowed relative
#'   increases versus a baseline.
#' @param gating Whether detected regressions should fail the comparison.
#' @param metadata Additional named metadata.
#' @return A validated `PopgenVCFPerformanceSpec`.
#' @export
new_performance_benchmark_spec <- function(
    id, runner, threads = 1L, warmup = 1L, iterations = 5L, seed = 1L,
    runtime_regression = 0.20, memory_regression = 0.25,
    disk_regression = 0.25, gating = FALSE, metadata = list()) {
  id <- tolower(performance_scalar_string(id, "id"))
  if (!is.function(runner)) stop("runner must be a function", call. = FALSE)
  threads <- sort(unique(as.integer(threads)))
  if (!length(threads) || anyNA(threads) || any(threads < 1L)) {
    stop("threads must contain positive integers", call. = FALSE)
  }
  warmup <- as.integer(warmup); iterations <- as.integer(iterations); seed <- as.integer(seed)
  if (length(warmup) != 1L || is.na(warmup) || warmup < 0L) stop("warmup must be nonnegative", call. = FALSE)
  if (length(iterations) != 1L || is.na(iterations) || iterations < 1L) stop("iterations must be positive", call. = FALSE)
  thresholds <- c(runtime_seconds = runtime_regression,
                  peak_memory_mb = memory_regression,
                  temporary_disk_mb = disk_regression)
  if (anyNA(thresholds) || any(!is.finite(thresholds)) || any(thresholds < 0)) {
    stop("regression thresholds must be nonnegative finite values", call. = FALSE)
  }
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata)))) {
    stop("metadata must be a named list", call. = FALSE)
  }
  structure(list(
    schema_version = "1.0", id = id, runner = runner, threads = threads,
    warmup = warmup, iterations = iterations, seed = seed,
    thresholds = thresholds, gating = isTRUE(gating), metadata = metadata
  ), class = "PopgenVCFPerformanceSpec")
}

measure_performance_once <- function(runner, threads, seed) {
  set.seed(seed)
  temp <- tempfile("popgenvcf-performance-")
  dir.create(temp, recursive = TRUE)
  old_temp <- Sys.getenv("POPGENVCF_PERFORMANCE_TEMP", unset = NA_character_)
  Sys.setenv(POPGENVCF_PERFORMANCE_TEMP = temp)
  on.exit({
    if (is.na(old_temp)) Sys.unsetenv("POPGENVCF_PERFORMANCE_TEMP") else
      Sys.setenv(POPGENVCF_PERFORMANCE_TEMP = old_temp)
    unlink(temp, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  invisible(gc(reset = TRUE))
  started <- proc.time()[["elapsed"]]
  value <- runner(threads = threads)
  elapsed <- proc.time()[["elapsed"]] - started
  memory <- gc()
  mb_column <- if ("(Mb)" %in% colnames(memory)) "(Mb)" else colnames(memory)[ncol(memory)]
  memory_mb <- sum(memory[, mb_column], na.rm = TRUE)
  files <- list.files(temp, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  disk_mb <- if (length(files)) sum(file.info(files)$size, na.rm = TRUE) / 1024^2 else 0
  list(value = value, runtime_seconds = as.numeric(elapsed),
       peak_memory_mb = as.numeric(memory_mb), temporary_disk_mb = as.numeric(disk_mb))
}

performance_summary_stats <- function(values) {
  values <- as.numeric(values)
  c(median = stats::median(values), mad = stats::mad(values, constant = 1),
    q05 = unname(stats::quantile(values, 0.05, names = FALSE)),
    q95 = unname(stats::quantile(values, 0.95, names = FALSE)),
    min = min(values), max = max(values))
}

performance_scaling_metrics <- function(runtime, threads) {
  runtime <- as.numeric(runtime)
  threads <- as.integer(threads)
  base_threads <- min(threads)
  base_index <- which(threads == base_threads)[1L]
  base_runtime <- runtime[[base_index]]

  speedup <- rep(NA_real_, length(runtime))
  positive <- runtime > 0
  if (base_runtime > 0) speedup[positive] <- base_runtime / runtime[positive]
  speedup[[base_index]] <- 1

  efficiency <- speedup / (threads / base_threads)
  efficiency[[base_index]] <- 1
  list(speedup = speedup, scaling_efficiency = efficiency)
}

#' Run a performance benchmark
#' @param spec A `PopgenVCFPerformanceSpec`.
#' @param fingerprint Optional environment fingerprint.
#' @return A `PopgenVCFPerformanceResult`.
#' @export
run_performance_benchmark <- function(spec, fingerprint = performance_environment_fingerprint()) {
  if (!inherits(spec, "PopgenVCFPerformanceSpec")) stop("spec is invalid", call. = FALSE)
  rows <- list(); index <- 0L
  for (thread_count in spec$threads) {
    if (spec$warmup > 0L) for (i in seq_len(spec$warmup)) {
      invisible(measure_performance_once(spec$runner, thread_count, spec$seed + i - 1L))
    }
    for (i in seq_len(spec$iterations)) {
      index <- index + 1L
      measurement <- measure_performance_once(
        spec$runner, thread_count, spec$seed + spec$warmup + i - 1L
      )
      rows[[index]] <- data.table::data.table(
        threads = thread_count, iteration = i,
        runtime_seconds = measurement$runtime_seconds,
        peak_memory_mb = measurement$peak_memory_mb,
        temporary_disk_mb = measurement$temporary_disk_mb
      )
    }
  }
  measurements <- data.table::rbindlist(rows)
  summary <- measurements[, {
    runtime <- performance_summary_stats(runtime_seconds)
    memory <- performance_summary_stats(peak_memory_mb)
    disk <- performance_summary_stats(temporary_disk_mb)
    data.table::data.table(
      runtime_median = runtime[["median"]], runtime_mad = runtime[["mad"]],
      runtime_q05 = runtime[["q05"]], runtime_q95 = runtime[["q95"]],
      memory_median_mb = memory[["median"]], memory_mad_mb = memory[["mad"]],
      disk_median_mb = disk[["median"]], disk_mad_mb = disk[["mad"]]
    )
  }, by = threads]
  scaling <- performance_scaling_metrics(summary$runtime_median, summary$threads)
  summary[, `:=`(
    speedup = scaling$speedup,
    scaling_efficiency = scaling$scaling_efficiency
  )]
  structure(list(
    schema_version = "1.0", id = spec$id,
    fingerprint = fingerprint, fingerprint_id = performance_fingerprint_id(fingerprint),
    measurements = measurements, summary = summary,
    thresholds = spec$thresholds, gating = spec$gating, metadata = spec$metadata
  ), class = "PopgenVCFPerformanceResult")
}

#' Compare a performance result with a baseline
#' @param observed,baseline Performance results with matching identifiers.
#' @param allow_incompatible Permit comparison across different fingerprints.
#' @param gating Optional override for regression gating.
#' @return A `PopgenVCFPerformanceComparison`.
#' @export
compare_performance_baseline <- function(observed, baseline,
                                         allow_incompatible = FALSE,
                                         gating = observed$gating) {
  if (!inherits(observed, "PopgenVCFPerformanceResult") ||
      !inherits(baseline, "PopgenVCFPerformanceResult")) {
    stop("observed and baseline must be performance results", call. = FALSE)
  }
  if (!identical(observed$id, baseline$id)) stop("benchmark identifiers differ", call. = FALSE)
  compatible <- identical(observed$fingerprint_id, baseline$fingerprint_id)
  if (!compatible && !isTRUE(allow_incompatible)) {
    stop("performance fingerprints differ; cross-host comparison is disabled", call. = FALSE)
  }
  regression_thresholds <- observed$thresholds
  merged <- merge(observed$summary, baseline$summary, by = "threads",
                  suffixes = c("_observed", "_baseline"))
  metrics <- data.table::rbindlist(list(
    merged[, .(threads, metric = "runtime_seconds", observed = runtime_median_observed,
               baseline = runtime_median_baseline)],
    merged[, .(threads, metric = "peak_memory_mb", observed = memory_median_mb_observed,
               baseline = memory_median_mb_baseline)],
    merged[, .(threads, metric = "temporary_disk_mb", observed = disk_median_mb_observed,
               baseline = disk_median_mb_baseline)]
  ))
  metrics[, relative_change := data.table::fifelse(
    baseline == 0, data.table::fifelse(observed == 0, 0, Inf),
    (observed - baseline) / baseline
  )]
  metrics[, allowed_relative_change := unname(regression_thresholds[metric])]
  metrics[, regressed := relative_change > allowed_relative_change]
  status <- if (isTRUE(gating) && any(metrics$regressed)) "failed" else "passed"
  structure(list(
    schema_version = "1.0", id = observed$id, compatible = compatible,
    gating = isTRUE(gating), status = status, comparisons = metrics,
    observed_fingerprint = observed$fingerprint,
    baseline_fingerprint = baseline$fingerprint
  ), class = "PopgenVCFPerformanceComparison")
}

#' Convert performance objects to stable tables
#' @param x A performance result or comparison.
#' @return A data table.
#' @export
performance_benchmark_table <- function(x) {
  if (inherits(x, "PopgenVCFPerformanceResult")) {
    tab <- data.table::copy(x$summary)
    tab[, `:=`(id = x$id, fingerprint_id = x$fingerprint_id)]
    data.table::setcolorder(tab, c("id", "fingerprint_id",
                                  setdiff(names(tab), c("id", "fingerprint_id"))))
    return(tab[])
  }
  if (inherits(x, "PopgenVCFPerformanceComparison")) {
    tab <- data.table::copy(x$comparisons)
    tab[, `:=`(id = x$id, status = x$status,
               compatible = x$compatible, gating = x$gating)]
    return(tab[])
  }
  stop("x must be a performance result or comparison", call. = FALSE)
}

#' Save and read performance benchmark baselines
#' @param x A `PopgenVCFPerformanceResult`.
#' @param path RDS path.
#' @return `path` for saving, or the validated result for reading.
#' @export
save_performance_baseline <- function(x, path) {
  if (!inherits(x, "PopgenVCFPerformanceResult")) stop("x is invalid", call. = FALSE)
  saveRDS(x, path, version = 3)
  invisible(path)
}

#' @rdname save_performance_baseline
#' @export
read_performance_baseline <- function(path) {
  x <- readRDS(path)
  if (!inherits(x, "PopgenVCFPerformanceResult")) {
    stop("file does not contain a performance result", call. = FALSE)
  }
  x
}
