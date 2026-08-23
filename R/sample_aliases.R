normalize_sample_aliases <- function(metadata) {
  x <- new_sample_identity(metadata)
  x[, display_sample := public_sample]
  x[]
}

public_sample_ids <- function(metadata, vcf_sample_ids) {
  resolve_sample_identity(metadata, vcf_sample_ids)
}

# Attaches a `population` column to an ancestry-backend Q-matrix table
# (ADMIXTURE/fastStructure/sNMF) when metadata provides one, for readability
# and population-ordered plotting only -- these are unsupervised ancestry
# inference methods that need no population metadata to run at all (unlike
# diversity/fst/amova/etc., which are population-gated and excluded entirely
# when metadata is absent, see analysis_capability_table()). When metadata
# has no `population` column, `qdt` is returned unchanged rather than
# stop()ing: plot_q_matrix() and downstream consumers already handle a
# missing `population` column gracefully, matching what a real production
# run with no population metadata surfaced (a crash here previously threw
# away an already-completed ADMIXTURE/fastStructure/sNMF result).
attach_q_population <- function(qdt, metadata) {
  if (!"population" %in% names(metadata)) return(qdt)
  metadata_samples <- as.character(metadata[["sample"]])
  if (anyDuplicated(metadata_samples)) {
    stop("Metadata contains duplicate sample identifiers", call. = FALSE)
  }
  population <- as.character(metadata[["population"]])[match(qdt$sample, metadata_samples)]
  if (anyNA(population)) {
    stop("Some ancestry samples are absent from retained metadata", call. = FALSE)
  }
  qdt[["population"]] <- population
  qdt
}

normalize_ld_window_bp <- function(x = Inf) {
  value <- suppressWarnings(as.numeric(x)[1L])
  if (is.na(value) || value <= 0) {
    stop("LD window must be a positive number or Inf", call. = FALSE)
  }
  if (!is.finite(value) || value > .Machine$integer.max) {
    return(.Machine$integer.max)
  }
  as.integer(value)
}
