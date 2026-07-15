external_reference_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

#' Create an external-reference comparison specification
#'
#' @param id Stable comparison identifier.
#' @param analysis Analysis identifier.
#' @param reference_tool Reference implementation name.
#' @param mode Comparison mode: `exact`, `numeric`, `matrix`, `subspace`, or
#'   `q_matrix`.
#' @param observed,reference Functions accepting benchmark data and returning
#'   values to compare.
#' @param absolute_tolerance,relative_tolerance Numerical tolerances.
#' @param role `equivalence` for gating comparisons or `diagnostic` for
#'   transparent non-gating cross-method comparisons.
#' @param requirements Optional availability function returning `TRUE` or a
#'   character skip reason.
#' @param reference_version Optional reference implementation version.
#' @param interpretation Scientific interpretation of the comparison.
#' @param citations Character vector of references.
#' @return A validated `PopgenVCFExternalReferenceSpec`.
#' @export
new_external_reference_spec <- function(
    id, analysis, reference_tool,
    mode = c("numeric", "exact", "matrix", "subspace", "q_matrix"),
    observed, reference,
    absolute_tolerance = 1e-8, relative_tolerance = 1e-6,
    role = c("equivalence", "diagnostic"), requirements = NULL,
    reference_version = NA_character_, interpretation = "",
    citations = character()) {
  mode <- match.arg(mode)
  role <- match.arg(role)
  if (!is.function(observed) || !is.function(reference)) {
    stop("observed and reference must be functions", call. = FALSE)
  }
  if (!is.null(requirements) && !is.function(requirements)) {
    stop("requirements must be NULL or a function", call. = FALSE)
  }
  tolerances <- c(absolute_tolerance, relative_tolerance)
  if (anyNA(tolerances) || any(!is.finite(tolerances)) || any(tolerances < 0)) {
    stop("tolerances must be nonnegative finite values", call. = FALSE)
  }
  x <- structure(list(
    schema_version = "1.0",
    id = tolower(external_reference_scalar_string(id, "id")),
    analysis = tolower(external_reference_scalar_string(analysis, "analysis")),
    reference_tool = external_reference_scalar_string(reference_tool, "reference_tool"),
    reference_version = as.character(reference_version)[1L],
    mode = mode, observed = observed, reference = reference,
    absolute_tolerance = as.numeric(absolute_tolerance),
    relative_tolerance = as.numeric(relative_tolerance),
    role = role, requirements = requirements,
    interpretation = as.character(interpretation)[1L],
    citations = as.character(citations)
  ), class = "PopgenVCFExternalReferenceSpec")
  validate_external_reference_spec(x)
}

#' Validate an external-reference specification
#' @param x A `PopgenVCFExternalReferenceSpec`.
#' @return `x`, invisibly.
#' @export
validate_external_reference_spec <- function(x) {
  if (!inherits(x, "PopgenVCFExternalReferenceSpec")) {
    stop("x must be a PopgenVCFExternalReferenceSpec", call. = FALSE)
  }
  if (!identical(x$schema_version, "1.0")) stop("unsupported external-reference schema", call. = FALSE)
  if (!x$mode %in% c("exact", "numeric", "matrix", "subspace", "q_matrix")) stop("invalid comparison mode", call. = FALSE)
  if (!x$role %in% c("equivalence", "diagnostic")) stop("invalid scientific role", call. = FALSE)
  if (!is.function(x$observed) || !is.function(x$reference)) stop("comparison functions are invalid", call. = FALSE)
  invisible(x)
}

external_reference_numeric_table <- function(observed, reference, absolute_tolerance, relative_tolerance) {
  observed <- unlist(observed, use.names = TRUE)
  reference <- unlist(reference, use.names = TRUE)
  if (!is.numeric(observed) || !is.numeric(reference)) stop("numeric comparison requires numeric values", call. = FALSE)
  if (length(observed) != length(reference)) stop("observed and reference lengths differ", call. = FALSE)
  if (!is.null(names(reference)) && all(nzchar(names(reference)))) observed <- observed[names(reference)]
  metric <- names(reference)
  if (is.null(metric) || any(!nzchar(metric))) metric <- paste0("metric_", seq_along(reference))
  absolute_error <- abs(as.numeric(observed) - as.numeric(reference))
  denominator <- pmax(abs(as.numeric(reference)), .Machine$double.eps)
  relative_error <- absolute_error / denominator
  data.table::data.table(
    metric = metric, observed = as.numeric(observed), reference = as.numeric(reference),
    absolute_error = absolute_error, relative_error = relative_error,
    passed = absolute_error <= absolute_tolerance | relative_error <= relative_tolerance
  )
}

external_reference_matrix_table <- function(observed, reference, absolute_tolerance, relative_tolerance) {
  observed <- as.matrix(observed)
  reference <- as.matrix(reference)
  if (!identical(dim(observed), dim(reference))) stop("observed and reference matrix dimensions differ", call. = FALSE)
  row_labels <- rownames(reference) %||% as.character(seq_len(nrow(reference)))
  col_labels <- colnames(reference) %||% as.character(seq_len(ncol(reference)))
  metric <- as.vector(outer(row_labels, col_labels, paste, sep = ":"))
  external_reference_numeric_table(
    stats::setNames(as.vector(observed), metric),
    stats::setNames(as.vector(reference), metric),
    absolute_tolerance, relative_tolerance
  )
}

external_reference_subspace_table <- function(observed, reference, absolute_tolerance) {
  observed <- as.matrix(observed)
  reference <- as.matrix(reference)
  if (nrow(observed) != nrow(reference)) stop("subspaces must have the same number of observations", call. = FALSE)
  dimensions <- min(ncol(observed), ncol(reference))
  if (dimensions < 1L) stop("subspaces must contain at least one component", call. = FALSE)
  observed_basis <- qr.Q(qr(observed))[, seq_len(dimensions), drop = FALSE]
  reference_basis <- qr.Q(qr(reference))[, seq_len(dimensions), drop = FALSE]
  correlations <- svd(crossprod(observed_basis, reference_basis), nu = 0L, nv = 0L)$d
  correlations <- pmin(1, pmax(0, correlations))
  errors <- 1 - correlations
  data.table::data.table(
    metric = paste0("canonical_correlation_", seq_along(correlations)),
    observed = correlations, reference = 1,
    absolute_error = errors, relative_error = errors,
    passed = errors <= absolute_tolerance
  )
}

#' Compare values from popgenVCF and a reference implementation
#' @param observed,reference Values to compare.
#' @param mode Comparison mode.
#' @param absolute_tolerance,relative_tolerance Numerical tolerances.
#' @return A long-form comparison table.
#' @export
compare_external_reference <- function(observed, reference,
                                       mode = c("numeric", "exact", "matrix", "subspace", "q_matrix"),
                                       absolute_tolerance = 1e-8,
                                       relative_tolerance = 1e-6) {
  mode <- match.arg(mode)
  if (mode == "exact") {
    passed <- identical(observed, reference)
    return(data.table::data.table(
      metric = "exact_identity", observed = as.character(passed), reference = "TRUE",
      absolute_error = as.numeric(!passed), relative_error = as.numeric(!passed), passed = passed
    ))
  }
  if (mode == "numeric") {
    return(external_reference_numeric_table(observed, reference, absolute_tolerance, relative_tolerance))
  }
  if (mode == "matrix") {
    return(external_reference_matrix_table(observed, reference, absolute_tolerance, relative_tolerance))
  }
  if (mode == "subspace") {
    return(external_reference_subspace_table(observed, reference, absolute_tolerance))
  }
  aligned <- align_cluster_labels(as.matrix(observed), as.matrix(reference))$aligned
  external_reference_matrix_table(aligned, reference, absolute_tolerance, relative_tolerance)
}

external_reference_requirement <- function(spec) {
  if (is.null(spec$requirements)) return(list(available = TRUE, reason = ""))
  value <- spec$requirements()
  if (isTRUE(value)) return(list(available = TRUE, reason = ""))
  list(available = FALSE, reason = if (is.character(value) && length(value)) value[[1L]] else "reference implementation unavailable")
}

#' Run an external-reference comparison
#' @param spec A comparison specification.
#' @param data Benchmark data passed to observed and reference adapters.
#' @return A `PopgenVCFExternalReferenceResult`.
#' @export
run_external_reference <- function(spec, data) {
  validate_external_reference_spec(spec)
  requirement <- external_reference_requirement(spec)
  if (!requirement$available) {
    return(structure(list(
      schema_version = "1.0", id = spec$id, analysis = spec$analysis,
      reference_tool = spec$reference_tool, reference_version = spec$reference_version,
      role = spec$role, mode = spec$mode, status = "skipped",
      comparisons = data.table::data.table(), message = requirement$reason,
      interpretation = spec$interpretation, citations = spec$citations
    ), class = "PopgenVCFExternalReferenceResult"))
  }
  observed <- tryCatch(spec$observed(data), error = identity)
  reference <- tryCatch(spec$reference(data), error = identity)
  failure <- if (inherits(observed, "error")) observed else if (inherits(reference, "error")) reference else NULL
  if (!is.null(failure)) {
    return(structure(list(
      schema_version = "1.0", id = spec$id, analysis = spec$analysis,
      reference_tool = spec$reference_tool, reference_version = spec$reference_version,
      role = spec$role, mode = spec$mode, status = "error",
      comparisons = data.table::data.table(), message = conditionMessage(failure),
      interpretation = spec$interpretation, citations = spec$citations
    ), class = "PopgenVCFExternalReferenceResult"))
  }
  comparisons <- tryCatch(compare_external_reference(
    observed, reference, spec$mode,
    spec$absolute_tolerance, spec$relative_tolerance
  ), error = identity)
  if (inherits(comparisons, "error")) {
    status <- "error"
    message <- conditionMessage(comparisons)
    comparisons <- data.table::data.table()
  } else {
    numerical_pass <- !nrow(comparisons) || all(comparisons$passed)
    status <- if (spec$role == "diagnostic" || numerical_pass) "passed" else "failed"
    message <- if (spec$role == "diagnostic" && !numerical_pass) {
      "diagnostic difference recorded; comparison is non-gating"
    } else if (!numerical_pass) "equivalence tolerance exceeded" else "within tolerance"
  }
  structure(list(
    schema_version = "1.0", id = spec$id, analysis = spec$analysis,
    reference_tool = spec$reference_tool, reference_version = spec$reference_version,
    role = spec$role, mode = spec$mode, status = status,
    comparisons = data.table::as.data.table(comparisons), message = message,
    interpretation = spec$interpretation, citations = spec$citations
  ), class = "PopgenVCFExternalReferenceResult")
}

#' Convert external-reference results to a long-form table
#' @param results A result or list of results.
#' @return A data table.
#' @export
external_reference_table <- function(results) {
  if (inherits(results, "PopgenVCFExternalReferenceResult")) results <- list(results)
  if (!is.list(results)) stop("results must be an external-reference result or list", call. = FALSE)
  data.table::rbindlist(lapply(results, function(x) {
    base <- data.table::data.table(
      id = x$id, analysis = x$analysis, reference_tool = x$reference_tool,
      reference_version = x$reference_version, role = x$role, mode = x$mode,
      status = x$status, message = x$message, interpretation = x$interpretation
    )
    if (!nrow(x$comparisons)) return(base)
    cbind(base[rep(1L, nrow(x$comparisons))], x$comparisons)
  }), fill = TRUE)
}

#' Convert an external-reference comparison to a benchmark specification
#' @param spec An external-reference specification.
#' @param dataset A canonical benchmark dataset.
#' @return A `PopgenVCFBenchmarkSpec`.
#' @export
external_reference_benchmark_spec <- function(spec, dataset) {
  validate_external_reference_spec(spec)
  validate_benchmark_dataset(dataset)
  new_benchmark_spec(
    id = spec$id, category = "external", dataset = dataset,
    runner = function(data) {
      result <- run_external_reference(spec, data)
      if (result$status == "skipped") stop(result$message, call. = FALSE)
      if (result$status == "error") stop(result$message, call. = FALSE)
      list(observed = result$comparisons, reference = result$comparisons)
    },
    comparator = function(observed, reference, absolute_tolerance, relative_tolerance) {
      observed[, .(metric, observed, reference, absolute_error, relative_error,
                   passed = if (spec$role == "diagnostic") TRUE else passed)]
    },
    absolute_tolerance = spec$absolute_tolerance,
    relative_tolerance = spec$relative_tolerance,
    requirements = spec$requirements, citations = spec$citations
  )
}
