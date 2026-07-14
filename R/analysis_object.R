#' Create a population-genomics analysis state
#'
#' `new_popgen_vcf_analysis()` creates the canonical state object passed between
#' popgenVCF pipeline stages. The object is an S3 list so it remains easy to
#' inspect, serialize, test, and extend without mutable-reference semantics.
#'
#' @param config A validated popgenVCF configuration list.
#' @param dirs Output directories returned by the internal directory builder.
#' @return An object of class `PopgenVCFAnalysis`.
#' @export
new_popgen_vcf_analysis <- function(config, dirs = NULL) {
  structure(
    list(
      schema_version = "1.0",
      package_name = "popgenVCF",
      package_version = popgenvcf_version(),
      started_at = Sys.time(),
      completed_at = NULL,
      status = "initialized",
      config = config,
      dirs = dirs,
      inputs = list(metadata = NULL, gds_path = NULL, ids = NULL),
      samples = list(ids = NULL, metadata = NULL, qc = NULL, participation = NULL),
      variants = list(audit = NULL, qc_ids = NULL, ld_ids = NULL, reports = NULL),
      results = list(),
      timings = list(),
      messages = data.table::data.table(
        timestamp = as.POSIXct(character()),
        level = character(),
        stage = character(),
        message = character()
      )
    ),
    class = "PopgenVCFAnalysis"
  )
}

#' Test whether an object is a popgenVCF analysis state
#' @param x Any R object.
#' @return A logical scalar.
#' @export
is_popgen_vcf_analysis <- function(x) inherits(x, "PopgenVCFAnalysis")

#' Validate a population-genomics analysis state
#'
#' @param x A `PopgenVCFAnalysis` object.
#' @param stage Optional stage name used to apply stronger stage-specific checks.
#' @return `x`, invisibly, or an error when invariants are violated.
#' @export
validate_analysis <- function(x, stage = NULL) {
  if (!is_popgen_vcf_analysis(x)) stop("Object is not a PopgenVCFAnalysis", call. = FALSE)
  required <- c("schema_version", "package_name", "config", "inputs", "samples", "variants", "results", "timings")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Analysis state is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(x$package_name, "popgenVCF")) stop("Analysis state has an invalid package identity", call. = FALSE)
  if (!is.list(x$config)) stop("Analysis config must be a list", call. = FALSE)
  if (!is.null(x$samples$ids) && anyDuplicated(x$samples$ids)) stop("Analysis state contains duplicate sample IDs", call. = FALSE)
  if (!is.null(x$samples$metadata) && !is.null(x$samples$ids)) {
    if (!identical(as.character(x$samples$metadata$sample), as.character(x$samples$ids))) {
      stop("Sample metadata order does not match analysis sample IDs", call. = FALSE)
    }
  }
  if (!is.null(x$variants$ld_ids) && !is.null(x$variants$qc_ids)) {
    outside <- setdiff(x$variants$ld_ids, x$variants$qc_ids)
    if (length(outside)) stop("LD-pruned SNP set contains variants outside the audited QC set", call. = FALSE)
  }
  if (!is.null(stage)) {
    stage <- as.character(stage)[1]
    if (stage %in% c("diversity", "ordination", "fst", "advanced") && !length(x$variants$qc_ids)) {
      stop("No QC-passing SNPs are available for stage: ", stage, call. = FALSE)
    }
    if (stage %in% c("ordination", "advanced") && !length(x$variants$ld_ids)) {
      stop("No LD-pruned SNPs are available for stage: ", stage, call. = FALSE)
    }
  }
  invisible(x)
}

#' Store a result in an analysis state
#' @param x A `PopgenVCFAnalysis` object.
#' @param name Result name.
#' @param value Result value.
#' @return Updated `PopgenVCFAnalysis` object.
#' @export
set_analysis_result <- function(x, name, value) {
  validate_analysis(x)
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) stop("Result name must be one non-empty string", call. = FALSE)
  x$results[[name]] <- value
  validate_analysis(x)
  x
}

#' Retrieve a result from an analysis state
#' @param x A `PopgenVCFAnalysis` object.
#' @param name Result name.
#' @param default Value returned when the result is absent.
#' @return The stored result or `default`.
#' @export
get_analysis_result <- function(x, name, default = NULL) {
  validate_analysis(x)
  if (!name %in% names(x$results)) return(default)
  x$results[[name]]
}

record_analysis_message <- function(x, level, stage, message) {
  row <- data.table::data.table(timestamp = Sys.time(), level = level, stage = stage, message = message)
  x$messages <- data.table::rbindlist(list(x$messages, row), use.names = TRUE, fill = TRUE)
  x
}

record_analysis_timing <- function(x, stage, elapsed_seconds) {
  x$timings[[stage]] <- as.numeric(elapsed_seconds)
  x
}

#' @export
print.PopgenVCFAnalysis <- function(x, ...) {
  cat("<PopgenVCFAnalysis>\n")
  cat("  status:       ", x$status, "\n", sep = "")
  cat("  package:      ", x$package_name, " ", x$package_version, "\n", sep = "")
  cat("  schema:       ", x$schema_version, "\n", sep = "")
  cat("  samples:      ", length(x$samples$ids %||% character()), "\n", sep = "")
  cat("  populations:  ", if (is.null(x$samples$metadata)) 0L else data.table::uniqueN(x$samples$metadata$population), "\n", sep = "")
  cat("  QC SNPs:      ", length(x$variants$qc_ids %||% integer()), "\n", sep = "")
  cat("  LD SNPs:      ", length(x$variants$ld_ids %||% integer()), "\n", sep = "")
  cat("  results:      ", if (length(x$results)) paste(names(x$results), collapse = ", ") else "none", "\n", sep = "")
  invisible(x)
}

#' @export
summary.PopgenVCFAnalysis <- function(object, ...) {
  data.table::data.table(
    package = object$package_name,
    package_version = object$package_version,
    status = object$status,
    samples = length(object$samples$ids %||% character()),
    populations = if (is.null(object$samples$metadata)) 0L else data.table::uniqueN(object$samples$metadata$population),
    qc_snps = length(object$variants$qc_ids %||% integer()),
    ld_pruned_snps = length(object$variants$ld_ids %||% integer()),
    completed_modules = paste(names(object$results), collapse = ", "),
    elapsed_seconds = sum(unlist(object$timings), na.rm = TRUE)
  )
}
