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
    # Biallelic SNPs have at most 2 alleles, so rarefied allelic richness
    # cannot exceed 2 regardless of the rarefaction sample size.
    if ("allelic_richness" %in% names(result$locus) &&
        any(result$locus$allelic_richness > 2 + 1e-6, na.rm = TRUE)) {
      errors <- c(errors, "locus allelic_richness exceeds the biallelic maximum of 2")
    }
    # Unlike allelic richness (a rarefied count with a hard combinatorial
    # ceiling of 2 for biallelic data), Ae = 1/(1 - He_unbiased) is a
    # bias-corrected continuous estimator and can legitimately exceed 2 for
    # small per-population samples -- the same kind of expected sampling
    # noise already accepted for W&C84 FST going slightly negative elsewhere
    # in this codebase. The one universal bound that does always hold is
    # Ae >= 1 (He_unbiased is never negative, so 1/(1 - He_unbiased) >= 1 or
    # Inf).
    if ("effective_alleles" %in% names(result$locus)) {
      finite_ae <- result$locus$effective_alleles[is.finite(result$locus$effective_alleles)]
      if (any(finite_ae < 1 - 1e-6)) {
        errors <- c(errors, "locus effective_alleles is below the minimum of 1")
      }
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

validate_kinship_result <- function(result, analysis, context) {
  errors <- character()
  required <- c("close_relatives", "matrix_file", "ibs0_file", "pairs_file")
  if (!is.list(result) || !all(required %in% names(result))) {
    errors <- c(errors, "kinship result is incomplete")
  } else if (!is.null(result$close_relatives) && nrow(result$close_relatives)) {
    if (!all(c("sample_1", "sample_2", "kinship", "relationship_degree") %in% names(result$close_relatives))) {
      errors <- c(errors, "close relatives table is missing required columns")
    }
    # KING-robust's upper bound (0.5, self/duplicate) is a hard theoretical
    # ceiling, but real data spanning highly differentiated populations can
    # push the estimator well below the naive -0.5 lower bound -- confirmed
    # empirically against real chr22 1000 Genomes data -- so only the upper
    # bound is enforced here.
    if ("kinship" %in% names(result$close_relatives) &&
        any(result$close_relatives$kinship > 0.5 + 1e-6, na.rm = TRUE)) {
      errors <- c(errors, "kinship values exceed the theoretical maximum of 0.5")
    }
  }
  validation_result(!length(errors), errors,
                    metrics = list(close_relatives = if (is.list(result) && !is.null(result$close_relatives)) nrow(result$close_relatives) else NA_integer_))
}

validate_sex_check_result <- function(result, analysis, context) {
  if (is.null(result)) {
    return(validation_result(TRUE, metrics = list(n_x_snps = 0L, n_y_snps = 0L)))
  }
  errors <- character()
  if (!is.list(result) || !all(c("table", "n_x_snps", "n_y_snps") %in% names(result))) {
    errors <- c(errors, "sex-check result is incomplete")
  } else if (nrow(result$table)) {
    if (!all(c("sample", "x_heterozygosity_F", "y_call_rate", "inferred_sex") %in% names(result$table))) {
      errors <- c(errors, "sex-check table is missing required columns")
    }
    if (!all(result$table$inferred_sex[!is.na(result$table$inferred_sex)] %in%
             c("male", "female", "ambiguous", "discordant"))) {
      errors <- c(errors, "sex-check inferred_sex must be male, female, ambiguous, discordant, or NA")
    }
  }
  validation_result(!length(errors), errors,
                    metrics = list(
                      n_x_snps = if (is.list(result)) result$n_x_snps else 0L,
                      n_y_snps = if (is.list(result)) result$n_y_snps else 0L
                    ))
}

validate_roh_result <- function(result, analysis, context) {
  errors <- character()
  if (!is.list(result) || !all(c("runs", "sample_summary") %in% names(result))) {
    errors <- c(errors, "ROH result requires runs and sample_summary")
  } else {
    if (!is.data.frame(result$runs) || !is.data.frame(result$sample_summary)) {
      errors <- c(errors, "ROH runs and sample_summary must be tabular")
    } else {
      if ("length_bp" %in% names(result$runs) && any(result$runs$length_bp < 0, na.rm = TRUE)) {
        errors <- c(errors, "ROH run lengths are negative")
      }
      if ("n_runs" %in% names(result$sample_summary) && any(result$sample_summary$n_runs < 0, na.rm = TRUE)) {
        errors <- c(errors, "ROH sample summary has negative n_runs")
      }
      # froh cannot exceed one by construction: bcftools roh's Viterbi path
      # produces non-overlapping runs per sample per chromosome (unlike
      # kinship's KING estimator, which is not bounded below at -0.5 for
      # divergent pairs -- confirmed empirically this session for both).
      if ("froh" %in% names(result$sample_summary) &&
          any(result$sample_summary$froh < -1e-6 | result$sample_summary$froh > 1 + 1e-6, na.rm = TRUE)) {
        errors <- c(errors, "ROH froh is outside [0, 1]")
      }
      froh_class_cols <- c("froh_short", "froh_intermediate", "froh_long")
      if (all(froh_class_cols %in% names(result$sample_summary))) {
        s <- result$sample_summary
        if (any(s$froh_short < -1e-6 | s$froh_intermediate < -1e-6 | s$froh_long < -1e-6, na.rm = TRUE)) {
          errors <- c(errors, "ROH froh_<class> is negative")
        }
        # The three length classes partition every run exactly once, so
        # their froh values must sum to the overall froh -- a real
        # correctness invariant, not just a plausibility bound.
        class_sum <- s$froh_short + s$froh_intermediate + s$froh_long
        if ("froh" %in% names(s) && any(abs(class_sum - s$froh) > 1e-6, na.rm = TRUE)) {
          errors <- c(errors, "ROH froh_<class> values do not sum to froh")
        }
      }
    }
  }
  validation_result(!length(errors), errors,
                    metrics = list(runs = if (is.list(result) && !is.null(result$runs)) nrow(result$runs) else NA_integer_))
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

validate_genome_scan_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  required <- c("fst_windows", "diversity_windows", "outliers")
  if (!is.list(result) || !all(required %in% names(result))) {
    errors <- c(errors, "genome scan result requires fst_windows, diversity_windows, and outliers")
  } else {
    if (!is.data.frame(result$fst_windows) || !is.data.frame(result$diversity_windows)) {
      errors <- c(errors, "genome scan windows must be tabular")
    } else {
      # Matches validate_fst_result()'s exact existing FST tolerance: W&C84
      # can occasionally exceed 1 under small-sample noise, a deliberate,
      # already-established policy elsewhere in this codebase, not
      # something to tighten here.
      if ("segregating_sites" %in% names(result$diversity_windows) &&
          any(result$diversity_windows$segregating_sites < 0L, na.rm = TRUE)) {
        errors <- c(errors, "genome scan segregating_sites is negative")
      }
      if ("global_fst" %in% names(result$fst_windows) && any(result$fst_windows$global_fst > 1, na.rm = TRUE)) {
        warnings <- c(warnings, "some windowed FST values exceed one")
      }
    }
  }
  validation_result(!length(errors), errors, warnings,
                    metrics = list(windows = if (is.list(result) && !is.null(result$fst_windows)) nrow(result$fst_windows) else NA_integer_))
}

validate_ld_decay_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.list(result) || !all(c("binned", "n_snps", "n_pairs") %in% names(result))) {
    errors <- c(errors, "LD decay result requires binned, n_snps, and n_pairs")
  } else if (!is.data.frame(result$binned)) {
    errors <- c(errors, "LD decay binned result must be tabular")
  } else if (nrow(result$binned)) {
    if (any(result$binned$n_pairs < 0L, na.rm = TRUE)) errors <- c(errors, "LD decay n_pairs is negative")
    if (any(result$binned$mean_r2 < -1e-6, na.rm = TRUE)) errors <- c(errors, "LD decay mean_r2 is negative")
    # r is a correlation bounded in [-1, 1], so r^2 <= 1 mathematically;
    # matches validate_fst_result()'s and validate_genome_scan_result()'s
    # existing policy of warning rather than erroring on tiny floating-point
    # overshoot rather than tightening a bound none of them enforce as hard.
    if (any(result$binned$mean_r2 > 1 + 1e-6, na.rm = TRUE)) warnings <- c(warnings, "some LD decay mean_r2 values exceed one")
  }
  validation_result(!length(errors), errors, warnings,
                    metrics = list(
                      bins = if (is.list(result) && !is.null(result$binned)) nrow(result$binned) else NA_integer_,
                      n_pairs = if (is.list(result)) result$n_pairs else NA_integer_
                    ))
}

validate_population_tree_result <- function(result, analysis, context) {
  errors <- character()
  if (!is.list(result) || !all(c("distance", "n_snps", "populations") %in% names(result))) {
    errors <- c(errors, "population tree result requires distance, n_snps, and populations")
  } else if (is.matrix(result$distance) && nrow(result$distance)) {
    if (nrow(result$distance) != ncol(result$distance)) errors <- c(errors, "population distance matrix must be square")
    if (any(abs(diag(result$distance)) > 1e-8)) errors <- c(errors, "population distance diagonal must be zero")
    if (any(result$distance < -1e-8, na.rm = TRUE)) errors <- c(errors, "population genetic distance cannot be negative")
  }
  validation_result(!length(errors), errors,
                    metrics = list(populations = if (is.list(result)) length(result$populations) else NA_integer_))
}

validate_population_assignment_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.list(result) || !all(c("assignment", "populations") %in% names(result))) {
    errors <- c(errors, "population assignment result requires assignment and populations")
  } else if (nrow(result$assignment)) {
    a <- result$assignment
    required <- c("sample", "recorded_population", "assigned_population", "mismatch",
                  "log_likelihood", "likelihood_ratio", "posterior_probability", "n_loci_used")
    if (!all(required %in% names(a))) {
      errors <- c(errors, "population assignment table is missing required columns")
    } else {
      finite_posterior <- a$posterior_probability[is.finite(a$posterior_probability)]
      if (any(finite_posterior < -1e-8 | finite_posterior > 1 + 1e-8)) {
        errors <- c(errors, "assignment posterior probability outside [0, 1]")
      }
      if (any(a$n_loci_used < 0L, na.rm = TRUE)) errors <- c(errors, "assignment n_loci_used is negative")
      n_mismatch <- sum(a$mismatch, na.rm = TRUE)
      if (n_mismatch > 0L) {
        warnings <- c(warnings, sprintf(
          "%d sample(s) assign to a population other than their recorded label", n_mismatch
        ))
      }
    }
  }
  validation_result(!length(errors), errors, warnings,
                    metrics = list(samples = if (is.list(result)) nrow(result$assignment) else NA_integer_))
}

validate_bottleneck_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.list(result) || !all(c("spectrum", "summary", "n_bins") %in% names(result))) {
    errors <- c(errors, "bottleneck result requires spectrum, summary, and n_bins")
  } else {
    if (!all(c("population", "bin", "bin_lower", "bin_upper", "n_loci") %in% names(result$spectrum))) {
      errors <- c(errors, "bottleneck spectrum table is missing required columns")
    } else if (any(result$spectrum$n_loci < 0L)) {
      errors <- c(errors, "bottleneck spectrum n_loci is negative")
    } else if (any(result$spectrum$bin_lower >= result$spectrum$bin_upper)) {
      errors <- c(errors, "bottleneck spectrum bin bounds are not increasing")
    }
    if (!all(c("population", "n_polymorphic_loci", "mode_bin", "mode_shifted") %in% names(result$summary))) {
      errors <- c(errors, "bottleneck summary table is missing required columns")
    } else {
      n_shifted <- sum(result$summary$mode_shifted, na.rm = TRUE)
      if (n_shifted > 0L) {
        warnings <- c(warnings, sprintf(
          "%d population(s) show a mode-shifted site frequency spectrum, a possible recent-bottleneck signature",
          n_shifted
        ))
      }
    }
  }
  validation_result(!length(errors), errors, warnings,
                    metrics = list(populations = if (is.list(result)) nrow(result$summary) else NA_integer_))
}

validate_ne_ld_result <- function(result, analysis, context) {
  errors <- character(); warnings <- character()
  if (!is.data.frame(result) || !all(c("population", "ne", "ne_status") %in% names(result))) {
    errors <- c(errors, "Ne(LD) result requires population, ne, and ne_status columns")
  } else if (nrow(result)) {
    finite_ne <- result$ne[is.finite(result$ne)]
    if (any(finite_ne <= 0)) errors <- c(errors, "Ne(LD) estimates must be positive")
    if ("n_pairs" %in% names(result) && any(result$n_pairs < 0L, na.rm = TRUE)) {
      errors <- c(errors, "Ne(LD) n_pairs is negative")
    }
  }
  validation_result(!length(errors), errors, warnings,
                    metrics = list(populations = if (is.data.frame(result)) nrow(result) else NA_integer_))
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
      rmse <- suppressWarnings(as.numeric(
        result$diagnostics$replicate_max_rmse
      ))
      finite_rmse <- rmse[is.finite(rmse)]
      max_rmse <- if (length(finite_rmse)) max(finite_rmse) else NA_real_
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

validate_spatial_autocorrelation_result <- function(result, analysis, context) {
  if (is.null(result)) {
    return(validation_result(TRUE, warnings = "Spatial autocorrelation was skipped because geographic data were unavailable"))
  }
  errors <- character()
  required <- c("bin_upper", "n_pairs", "r", "p_value", "null_lower", "null_upper")
  if (!is.data.frame(result) || !all(required %in% names(result))) {
    errors <- c(errors, "spatial autocorrelation result is missing required columns")
  } else {
    if (any(result$n_pairs < 0L)) errors <- c(errors, "spatial autocorrelation n_pairs is negative")
    if (any(diff(result$bin_upper) <= 0)) errors <- c(errors, "spatial autocorrelation distance-class upper bounds are not increasing")
    finite_p <- result$p_value[is.finite(result$p_value)]
    if (any(finite_p < 0 | finite_p > 1)) errors <- c(errors, "spatial autocorrelation p_value outside [0, 1]")
  }
  validation_result(!length(errors), errors,
                    metrics = list(bins = if (is.data.frame(result)) nrow(result) else NA_integer_))
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
