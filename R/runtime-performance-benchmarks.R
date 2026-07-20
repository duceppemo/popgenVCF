# Deterministic runtime performance and memory benchmark contracts.

runtime_benchmark_metric_names <- function() {
  c("elapsed_seconds", "allocated_bytes", "serialized_bytes")
}

validate_runtime_benchmark_metrics <- function(metrics) {
  required <- runtime_benchmark_metric_names()
  if (!is.list(metrics) || is.null(names(metrics)) || !all(required %in% names(metrics))) {
    stop(
      "runtime benchmark metrics must include elapsed_seconds, allocated_bytes, and serialized_bytes",
      call. = FALSE
    )
  }
  values <- unlist(metrics[required], use.names = FALSE)
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
    stop("runtime benchmark metrics must be finite non-negative numbers", call. = FALSE)
  }
  invisible(TRUE)
}

new_runtime_benchmark_record <- function(case, workload, metrics, environment = list()) {
  case <- as.character(case)[1]
  if (is.na(case) || !nzchar(case)) {
    stop("case must be a non-empty string", call. = FALSE)
  }
  if (!is.list(workload) || !length(workload)) {
    stop("workload must be a non-empty list", call. = FALSE)
  }
  validate_runtime_benchmark_metrics(metrics)
  if (!is.list(environment)) {
    stop("environment must be a list", call. = FALSE)
  }

  workload_fingerprint <- digest::digest(
    list(case = case, workload = workload),
    algo = "sha256",
    serialize = TRUE
  )

  structure(
    list(
      schema_version = 1L,
      case = case,
      workload = workload,
      workload_fingerprint = workload_fingerprint,
      metrics = metrics[runtime_benchmark_metric_names()],
      environment = environment
    ),
    class = "PopgenVCFRuntimeBenchmarkRecord"
  )
}

validate_runtime_benchmark_record <- function(x) {
  if (!inherits(x, "PopgenVCFRuntimeBenchmarkRecord")) {
    stop("x must be a PopgenVCFRuntimeBenchmarkRecord", call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("unsupported runtime benchmark record schema", call. = FALSE)
  }
  validate_runtime_benchmark_metrics(x$metrics)
  expected <- digest::digest(
    list(case = x$case, workload = x$workload),
    algo = "sha256",
    serialize = TRUE
  )
  if (!identical(x$workload_fingerprint, expected)) {
    stop("runtime benchmark workload fingerprint mismatch", call. = FALSE)
  }
  invisible(TRUE)
}
