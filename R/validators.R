validation_result <- function(valid = TRUE, errors = character(), warnings = character(), metrics = list()) {
  structure(list(valid = isTRUE(valid), errors = as.character(errors), warnings = as.character(warnings), metrics = metrics),
            class = "PopgenVCFValidation")
}

#' Validate a module result
#'
#' @param result Module result stored in a `PopgenVCFAnalysis` object.
#' @param analysis Current analysis state.
#' @param context Runtime context.
#' @return A `PopgenVCFValidation` object.
#' @export
validate_module_result <- function(result, analysis = NULL, context = NULL) {
  validation_result(TRUE)
}

validate_finite_columns <- function(x, columns, allow_na = TRUE) {
  errors <- character()
  for (nm in intersect(columns, names(x))) {
    value <- x[[nm]]
    bad <- if (allow_na) !is.na(value) & !is.finite(value) else !is.finite(value)
    if (any(bad)) errors <- c(errors, sprintf("column '%s' contains non-finite values", nm))
  }
  errors
}

validate_diversity_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.list(result)) {
    errors <- c(errors, "diversity result is not a list")
  } else {
    required <- c("sample", "population", "locus")
    missing <- setdiff(required, names(result))
    if (length(missing)) {
      errors <- c(errors, sprintf("missing component '%s'", missing))
    }
  }
  if (!length(errors)) {
    errors <- c(errors,
      validate_finite_columns(result$sample, c("observed_heterozygosity", "missing_rate")),
      validate_finite_columns(result$population, c("observed_heterozygosity", "expected_heterozygosity", "inbreeding_coefficient")))
    hcols <- intersect(c("observed_heterozygosity", "expected_heterozygosity"), names(result$population))
    for (nm in hcols) if (any(result$population[[nm]] < 0 | result$population[[nm]] > 1, na.rm = TRUE)) {
      errors <- c(errors, sprintf("population %s is outside [0,1]", nm))
    }
  }
  validation_result(!length(errors), errors, warnings,
                    list(samples = if (is.list(result) && !is.null(result$sample)) nrow(result$sample) else NA_integer_,
                         populations = if (is.list(result) && !is.null(result$population)) nrow(result$population) else NA_integer_))
}

validate_pca_result <- function(result, analysis, context) {
  errors <- character()
  if (!is.list(result) || !all(c("scores", "variance") %in% names(result))) {
    errors <- c(errors, "PCA result requires scores and variance")
  } else {
    if (!"sample" %in% names(result$scores)) errors <- c(errors, "PCA scores lack sample IDs")
    pc <- grep("^PC[0-9]+$", names(result$scores), value = TRUE)
    if (length(pc) < 2L) errors <- c(errors, "PCA requires at least two components")
    errors <- c(errors, validate_finite_columns(result$scores, pc, allow_na = FALSE))
    if (any(result$variance$percent < 0, na.rm = TRUE)) errors <- c(errors, "negative PCA variance")
    if (sum(result$variance$proportion, na.rm = TRUE) > 1 + 1e-6) errors <- c(errors, "PCA variance proportions exceed one")
  }
  validation_result(!length(errors), errors,
                    metrics = list(components = if (is.list(result)) nrow(result$variance) else NA_integer_))
}

validate_ibs_result <- function(result, analysis, context) {
  errors <- character()
  if (!is.list(result) || !all(c("mds", "similarity_file", "distance_file") %in% names(result))) {
    errors <- c(errors, "IBS result is incomplete")
  } else {
    if (!all(c("sample", "MDS1", "MDS2") %in% names(result$mds))) errors <- c(errors, "IBS MDS coordinates are incomplete")
    errors <- c(errors, validate_finite_columns(result$mds, c("MDS1", "MDS2"), allow_na = FALSE))
  }
  validation_result(!length(errors), errors)
}

validate_fst_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.list(result) || !all(c("global", "long", "matrix") %in% names(result))) {
    errors <- c(errors, "FST result requires global, long, and matrix")
  } else {
    if (!is.matrix(result$matrix) || nrow(result$matrix) != ncol(result$matrix)) errors <- c(errors, "FST matrix must be square")
    if (is.matrix(result$matrix) && !isTRUE(all.equal(result$matrix, t(result$matrix), tolerance = 1e-10, check.attributes = FALSE))) {
      errors <- c(errors, "FST matrix is not symmetric")
    }
    if (is.matrix(result$matrix) && any(abs(diag(result$matrix)) > 1e-10, na.rm = TRUE)) errors <- c(errors, "FST diagonal is not zero")
    if ("fst" %in% names(result$long) && any(result$long$fst > 1, na.rm = TRUE)) warnings <- c(warnings, "pairwise FST values exceed one")
  }
  validation_result(!length(errors), errors, warnings,
                    list(comparisons = if (is.list(result)) nrow(result$long) else NA_integer_))
}

validate_dapc_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character(); metrics <- list()
  if (!is.list(result) || !all(c("models", "diagnostics") %in% names(result))) {
    errors <- c(errors, "DAPC result is incomplete")
  } else if (!length(result$models)) {
    warnings <- c(warnings, "DAPC produced no fitted models")
  } else {
    memberships <- lapply(result$models, `[[`, "membership")
    check <- validate_membership_collection(memberships)
    errors <- c(errors, check$errors); metrics <- check$metrics
    if ("assignment_accuracy" %in% names(result$diagnostics) &&
        any(result$diagnostics$assignment_accuracy < 0 | result$diagnostics$assignment_accuracy > 1, na.rm = TRUE)) {
      errors <- c(errors, "DAPC assignment accuracy is outside [0,1]")
    }
    if ("replicate_max_rmse" %in% names(result$diagnostics)) {
      max_rmse <- max(result$diagnostics$replicate_max_rmse, na.rm = TRUE)
      metrics$maximum_replicate_rmse <- max_rmse
      threshold <- context$cfg$analyses$structure$reproducibility_rmse %||% 0.05
      if (is.finite(max_rmse) && max_rmse > threshold) warnings <- c(warnings, "DAPC replicate membership exceeds configured RMSE threshold")
    }
  }
  validation_result(!length(errors), errors, warnings, metrics)
}

validate_amova_result <- function(result, analysis, context) {
  errors <- character()
  if (!is.list(result) || !all(c("components", "phi") %in% names(result))) errors <- c(errors, "AMOVA result is incomplete")
  validation_result(!length(errors), errors)
}

validate_ibd_result <- function(result, analysis, context) {
  if (is.null(result)) return(validation_result(TRUE, warnings = "IBD was skipped because geographic data were unavailable"))
  errors <- character()
  if (!is.list(result) || !all(c("summary", "pairs") %in% names(result))) errors <- c(errors, "IBD result is incomplete")
  validation_result(!length(errors), errors)
}

validate_tree_result <- function(result, analysis, context) {
  errors <- character()
  if (!inherits(result, "phylo")) errors <- c(errors, "tree result is not an ape phylo object")
  if (inherits(result, "phylo") && length(result$tip.label) != length(analysis$samples$ids)) {
    errors <- c(errors, "tree tip count does not match retained samples")
  }
  validation_result(!length(errors), errors,
                    metrics = list(tips = if (inherits(result, "phylo")) length(result$tip.label) else NA_integer_))
}

validate_admixture_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.data.frame(result) && !data.table::is.data.table(result)) errors <- c(errors, "ADMIXTURE CV result is not tabular")
  if (!length(errors) && !all(c("K", "cv_error") %in% names(result))) errors <- c(errors, "ADMIXTURE CV result lacks K or CV_error")
  if (!length(errors) && any(!is.finite(result$cv_error))) errors <- c(errors, "ADMIXTURE CV errors are non-finite")
  validation_result(!length(errors), errors, warnings,
                    metrics = list(k_values = if (!length(errors)) nrow(result) else NA_integer_))
}

validate_chromosome_result <- function(result, analysis, context) {
  errors <- if (!is.data.frame(result) && !data.table::is.data.table(result)) "chromosome summary is not tabular" else character()
  validation_result(!length(errors), errors)
}

assert_module_validation <- function(validation, module) {
  if (!inherits(validation, "PopgenVCFValidation")) stop("Validator for '", module, "' returned an invalid object", call. = FALSE)
  if (length(validation$warnings)) for (w in validation$warnings) log_msg("Module ", module, ": ", w, level = "WARNING")
  if (!isTRUE(validation$valid)) stop("Validation failed for module '", module, "': ", paste(validation$errors, collapse = "; "), call. = FALSE)
  invisible(validation)
}

validate_membership_collection <- function(collection, tolerance = 1e-6) {
  errors <- character(); metrics <- list()
  if (!is.list(collection) || !length(collection)) return(list(errors = "no membership matrices were produced", metrics = metrics))
  for (nm in names(collection)) {
    q <- collection[[nm]]
    if (data.table::is.data.table(q) || is.data.frame(q)) {
      cols <- grep("^cluster_", names(q), value = TRUE)
      q <- as.matrix(q[, ..cols])
    }
    z <- tryCatch(normalize_q_matrix(q), error = function(e) e)
    if (inherits(z, "error")) errors <- c(errors, sprintf("K=%s: %s", nm, conditionMessage(z)))
    else {
      metrics[[paste0("K", nm, "_samples")]] <- nrow(z)
      metrics[[paste0("K", nm, "_clusters")]] <- ncol(z)
      if (max(abs(rowSums(z) - 1)) > tolerance) errors <- c(errors, sprintf("K=%s rows do not sum to one", nm))
    }
  }
  list(errors = errors, metrics = metrics)
}

validate_population_structure_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character(); metrics <- list()
  if (!is.list(result)) return(validation_result(FALSE, "population-structure result is not a list"))
  q <- result$q %||% result$membership %||% list()
  check <- validate_membership_collection(q)
  errors <- c(errors, check$errors); metrics <- check$metrics
  if (!is.null(result$diagnostics) && nrow(result$diagnostics)) {
    if (!"K" %in% names(result$diagnostics)) errors <- c(errors, "diagnostics lack K")
  }
  validation_result(!length(errors), errors, warnings, metrics)
}
