benchmark_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

#' Create a canonical benchmark dataset
#'
#' @param id Stable dataset identifier.
#' @param scale Dataset scale: `tiny`, `small`, `medium`, `large`, or `real`.
#' @param loader Function returning the in-memory benchmark data.
#' @param source Human-readable source description.
#' @param checksum Optional content checksum.
#' @param metadata Named dataset metadata.
#' @return A validated `PopgenVCFBenchmarkDataset`.
#' @export
new_benchmark_dataset <- function(id, scale = "tiny", loader, source = "embedded",
                                  checksum = NA_character_, metadata = list()) {
  id <- tolower(benchmark_scalar_string(id, "id"))
  scale <- match.arg(scale, c("tiny", "small", "medium", "large", "real"))
  if (!is.function(loader)) stop("loader must be a function", call. = FALSE)
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata)))) {
    stop("metadata must be a named list", call. = FALSE)
  }
  x <- structure(list(
    schema_version = "1.0", id = id, scale = scale, loader = loader,
    source = as.character(source)[1L], checksum = as.character(checksum)[1L],
    metadata = metadata
  ), class = "PopgenVCFBenchmarkDataset")
  validate_benchmark_dataset(x)
}

#' Validate a benchmark dataset
#' @param x A `PopgenVCFBenchmarkDataset`.
#' @return `x`, invisibly.
#' @export
validate_benchmark_dataset <- function(x) {
  if (!inherits(x, "PopgenVCFBenchmarkDataset")) stop("x must be a PopgenVCFBenchmarkDataset", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported benchmark dataset schema", call. = FALSE)
  benchmark_scalar_string(x$id, "dataset id")
  if (!x$scale %in% c("tiny", "small", "medium", "large", "real")) stop("dataset scale is invalid", call. = FALSE)
  if (!is.function(x$loader)) stop("dataset loader must be a function", call. = FALSE)
  invisible(x)
}

#' Create a benchmark specification
#'
#' @param id Stable benchmark identifier.
#' @param category Benchmark category.
#' @param dataset A `PopgenVCFBenchmarkDataset`.
#' @param runner Function accepting loaded data and returning observed values or
#'   a list containing `observed`, optional `reference`, `memory_mb`, `disk_mb`,
#'   and `provenance`.
#' @param reference Optional fixed reference values.
#' @param comparator Optional comparator function.
#' @param absolute_tolerance,relative_tolerance Numerical tolerances.
#' @param runtime_budget_seconds,memory_budget_mb,disk_budget_mb Optional budgets.
#' @param requirements Optional function returning `TRUE`, or a character skip reason.
#' @param citations Character vector of scientific references.
#' @return A validated `PopgenVCFBenchmarkSpec`.
#' @export
new_benchmark_spec <- function(id, category, dataset, runner, reference = NULL,
                               comparator = NULL, absolute_tolerance = 1e-8,
                               relative_tolerance = 1e-6,
                               runtime_budget_seconds = Inf,
                               memory_budget_mb = Inf, disk_budget_mb = Inf,
                               requirements = NULL, citations = character()) {
  id <- tolower(benchmark_scalar_string(id, "id"))
  category <- match.arg(category, c("numerical", "scientific", "performance", "reproducibility", "external"))
  validate_benchmark_dataset(dataset)
  if (!is.function(runner)) stop("runner must be a function", call. = FALSE)
  if (!is.null(comparator) && !is.function(comparator)) stop("comparator must be NULL or a function", call. = FALSE)
  if (!is.null(requirements) && !is.function(requirements)) stop("requirements must be NULL or a function", call. = FALSE)
  numeric_fields <- c(absolute_tolerance, relative_tolerance, runtime_budget_seconds, memory_budget_mb, disk_budget_mb)
  if (anyNA(numeric_fields) || any(numeric_fields < 0)) stop("tolerances and budgets must be nonnegative", call. = FALSE)
  x <- structure(list(
    schema_version = "1.0", id = id, category = category, dataset = dataset,
    runner = runner, reference = reference, comparator = comparator,
    absolute_tolerance = absolute_tolerance,
    relative_tolerance = relative_tolerance,
    runtime_budget_seconds = runtime_budget_seconds,
    memory_budget_mb = memory_budget_mb, disk_budget_mb = disk_budget_mb,
    requirements = requirements, citations = as.character(citations)
  ), class = "PopgenVCFBenchmarkSpec")
  validate_benchmark_spec(x)
}

#' Validate a benchmark specification
#' @param x A `PopgenVCFBenchmarkSpec`.
#' @return `x`, invisibly.
#' @export
validate_benchmark_spec <- function(x) {
  if (!inherits(x, "PopgenVCFBenchmarkSpec")) stop("x must be a PopgenVCFBenchmarkSpec", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported benchmark spec schema", call. = FALSE)
  benchmark_scalar_string(x$id, "benchmark id")
  validate_benchmark_dataset(x$dataset)
  if (!is.function(x$runner)) stop("benchmark runner must be a function", call. = FALSE)
  invisible(x)
}

benchmark_default_compare <- function(observed, reference, absolute_tolerance, relative_tolerance) {
  if (is.null(reference)) {
    return(data.table::data.table(metric = names(observed) %||% "value", observed = as.numeric(observed),
                                  reference = NA_real_, absolute_error = NA_real_, relative_error = NA_real_, passed = TRUE))
  }
  observed <- unlist(observed, use.names = TRUE)
  reference <- unlist(reference, use.names = TRUE)
  if (!is.numeric(observed) || !is.numeric(reference)) stop("default comparator requires numeric observed and reference values", call. = FALSE)
  if (length(observed) != length(reference)) stop("observed and reference values have different lengths", call. = FALSE)
  if (!is.null(names(reference)) && length(names(reference))) observed <- observed[names(reference)]
  metric <- names(reference)
  if (is.null(metric) || any(!nzchar(metric))) metric <- paste0("metric_", seq_along(reference))
  absolute_error <- abs(observed - reference)
  denominator <- pmax(abs(reference), .Machine$double.eps)
  relative_error <- absolute_error / denominator
  data.table::data.table(
    metric = metric, observed = as.numeric(observed), reference = as.numeric(reference),
    absolute_error = absolute_error, relative_error = relative_error,
    passed = absolute_error <= absolute_tolerance | relative_error <= relative_tolerance
  )
}

new_benchmark_result <- function(spec, status, comparisons = data.table::data.table(),
                                 runtime_seconds = NA_real_, memory_mb = NA_real_,
                                 disk_mb = NA_real_, message = "", provenance = list()) {
  x <- structure(list(
    schema_version = "1.0", id = spec$id, category = spec$category,
    dataset_id = spec$dataset$id, dataset_scale = spec$dataset$scale,
    status = status, comparisons = data.table::as.data.table(comparisons),
    runtime_seconds = runtime_seconds, memory_mb = memory_mb, disk_mb = disk_mb,
    budgets = list(runtime_seconds = spec$runtime_budget_seconds,
                   memory_mb = spec$memory_budget_mb, disk_mb = spec$disk_budget_mb),
    message = as.character(message)[1L], provenance = provenance,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ), class = "PopgenVCFBenchmarkResult")
  validate_benchmark_result(x)
}

#' Validate a benchmark result
#' @param x A `PopgenVCFBenchmarkResult`.
#' @return `x`, invisibly.
#' @export
validate_benchmark_result <- function(x) {
  if (!inherits(x, "PopgenVCFBenchmarkResult")) stop("x must be a PopgenVCFBenchmarkResult", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported benchmark result schema", call. = FALSE)
  if (!x$status %in% c("passed", "failed", "skipped", "error")) stop("benchmark result status is invalid", call. = FALSE)
  if (!is.data.frame(x$comparisons)) stop("comparisons must be a data frame", call. = FALSE)
  invisible(x)
}

#' Create a benchmark registry
#' @param specs Optional list of benchmark specifications.
#' @return A `PopgenVCFBenchmarkRegistry`.
#' @export
new_benchmark_registry <- function(specs = list()) {
  x <- structure(list(specs = list()), class = "PopgenVCFBenchmarkRegistry")
  for (spec in specs) x <- register_benchmark(x, spec)
  x
}

#' Register a benchmark
#' @param registry A benchmark registry.
#' @param spec A benchmark specification.
#' @return Updated registry.
#' @export
register_benchmark <- function(registry, spec) {
  if (!inherits(registry, "PopgenVCFBenchmarkRegistry")) stop("registry must be a PopgenVCFBenchmarkRegistry", call. = FALSE)
  validate_benchmark_spec(spec)
  if (spec$id %in% names(registry$specs)) stop("duplicate benchmark id: ", spec$id, call. = FALSE)
  registry$specs[[spec$id]] <- spec
  registry
}

#' List registered benchmarks
#' @param registry A benchmark registry.
#' @return A data table.
#' @export
list_benchmarks <- function(registry) {
  if (!inherits(registry, "PopgenVCFBenchmarkRegistry")) stop("registry must be a PopgenVCFBenchmarkRegistry", call. = FALSE)
  data.table::rbindlist(lapply(registry$specs, function(x) data.table::data.table(
    id = x$id, category = x$category, dataset = x$dataset$id,
    scale = x$dataset$scale, absolute_tolerance = x$absolute_tolerance,
    relative_tolerance = x$relative_tolerance,
    runtime_budget_seconds = x$runtime_budget_seconds,
    memory_budget_mb = x$memory_budget_mb, disk_budget_mb = x$disk_budget_mb
  )), fill = TRUE)
}

benchmark_requirement_status <- function(spec) {
  if (is.null(spec$requirements)) return(list(available = TRUE, reason = ""))
  value <- spec$requirements()
  if (isTRUE(value)) return(list(available = TRUE, reason = ""))
  reason <- if (is.character(value) && length(value)) value[[1L]] else "benchmark requirements are unavailable"
  list(available = FALSE, reason = reason)
}

#' Execute one benchmark specification
#' @param spec A benchmark specification.
#' @return A `PopgenVCFBenchmarkResult`.
#' @export
run_benchmark <- function(spec) {
  validate_benchmark_spec(spec)
  requirement <- benchmark_requirement_status(spec)
  if (!requirement$available) return(new_benchmark_result(spec, "skipped", message = requirement$reason))
  data <- spec$dataset$loader()
  started <- proc.time()[["elapsed"]]
  raw <- tryCatch(spec$runner(data), error = identity)
  runtime <- proc.time()[["elapsed"]] - started
  if (inherits(raw, "error")) return(new_benchmark_result(spec, "error", runtime_seconds = runtime, message = conditionMessage(raw)))
  payload <- if (is.list(raw) && "observed" %in% names(raw)) raw else list(observed = raw)
  reference <- payload$reference %||% spec$reference
  comparator <- spec$comparator %||% benchmark_default_compare
  comparisons <- tryCatch(
    comparator(payload$observed, reference, spec$absolute_tolerance, spec$relative_tolerance),
    error = identity
  )
  if (inherits(comparisons, "error")) return(new_benchmark_result(spec, "error", runtime_seconds = runtime, message = conditionMessage(comparisons)))
  comparisons <- data.table::as.data.table(comparisons)
  if (!"passed" %in% names(comparisons)) stop("benchmark comparator must return a passed column", call. = FALSE)
  memory_mb <- as.numeric(payload$memory_mb %||% NA_real_)
  disk_mb <- as.numeric(payload$disk_mb %||% NA_real_)
  numerical_pass <- !nrow(comparisons) || all(comparisons$passed)
  runtime_pass <- runtime <= spec$runtime_budget_seconds
  memory_pass <- is.na(memory_mb) || memory_mb <= spec$memory_budget_mb
  disk_pass <- is.na(disk_mb) || disk_mb <= spec$disk_budget_mb
  status <- if (numerical_pass && runtime_pass && memory_pass && disk_pass) "passed" else "failed"
  reasons <- c(
    if (!numerical_pass) "numerical tolerance exceeded",
    if (!runtime_pass) "runtime budget exceeded",
    if (!memory_pass) "memory budget exceeded",
    if (!disk_pass) "disk budget exceeded"
  )
  new_benchmark_result(spec, status, comparisons, runtime, memory_mb, disk_mb,
                       paste(reasons, collapse = "; "), payload$provenance %||% list())
}

#' Execute a benchmark registry
#' @param registry A benchmark registry.
#' @param ids Optional benchmark identifiers.
#' @param categories Optional benchmark categories.
#' @return A validated `PopgenVCFBenchmarkSuite`.
#' @export
run_benchmark_suite <- function(registry, ids = NULL, categories = NULL) {
  if (!inherits(registry, "PopgenVCFBenchmarkRegistry")) stop("registry must be a PopgenVCFBenchmarkRegistry", call. = FALSE)
  specs <- registry$specs
  if (!is.null(ids)) specs <- specs[names(specs) %in% tolower(as.character(ids))]
  if (!is.null(categories)) specs <- specs[vapply(specs, function(x) x$category %in% categories, logical(1L))]
  if (!length(specs)) stop("no benchmarks selected", call. = FALSE)
  specs <- specs[order(names(specs))]
  results <- lapply(specs, run_benchmark)
  x <- structure(list(
    schema_version = "1.0", results = results,
    passed = all(vapply(results, function(z) z$status %in% c("passed", "skipped"), logical(1L))),
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    provenance = list(package_version = as.character(utils::packageVersion("popgenVCF")),
                      r_version = R.version.string, platform = R.version$platform)
  ), class = "PopgenVCFBenchmarkSuite")
  validate_benchmark_suite(x)
}

#' Validate a benchmark suite
#' @param x A `PopgenVCFBenchmarkSuite`.
#' @return `x`, invisibly.
#' @export
validate_benchmark_suite <- function(x) {
  if (!inherits(x, "PopgenVCFBenchmarkSuite")) stop("x must be a PopgenVCFBenchmarkSuite", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported benchmark suite schema", call. = FALSE)
  if (!is.list(x$results) || !length(x$results)) stop("benchmark suite must contain results", call. = FALSE)
  invisible(lapply(x$results, validate_benchmark_result))
  invisible(x)
}

#' Convert a benchmark suite to a summary table
#' @param x A benchmark suite.
#' @return A data table.
#' @export
benchmark_suite_table <- function(x) {
  validate_benchmark_suite(x)
  data.table::rbindlist(lapply(x$results, function(z) data.table::data.table(
    id = z$id, category = z$category, dataset = z$dataset_id, scale = z$dataset_scale,
    status = z$status, runtime_seconds = z$runtime_seconds,
    memory_mb = z$memory_mb, disk_mb = z$disk_mb,
    max_absolute_error = if (nrow(z$comparisons) && "absolute_error" %in% names(z$comparisons)) max(z$comparisons$absolute_error, na.rm = TRUE) else NA_real_,
    max_relative_error = if (nrow(z$comparisons) && "relative_error" %in% names(z$comparisons)) max(z$comparisons$relative_error, na.rm = TRUE) else NA_real_,
    message = z$message
  )), fill = TRUE)
}

#' Save and read benchmark suites
#' @param x A benchmark suite.
#' @param path File path.
#' @return `path` invisibly for save, or a validated suite for read.
#' @export
save_benchmark_suite <- function(x, path) {
  validate_benchmark_suite(x)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path, version = 3)
  invisible(path)
}

#' @export
read_benchmark_suite <- function(path) {
  if (!file.exists(path)) stop("benchmark suite file does not exist: ", path, call. = FALSE)
  x <- readRDS(path)
  validate_benchmark_suite(x)
  x
}

#' @export
print.PopgenVCFBenchmarkSuite <- function(x, ...) {
  tab <- benchmark_suite_table(x)
  cat("<PopgenVCFBenchmarkSuite>", nrow(tab), "benchmarks;",
      sum(tab$status == "passed"), "passed;", sum(tab$status == "skipped"), "skipped;",
      sum(tab$status %in% c("failed", "error")), "failed/error\n")
  invisible(x)
}
