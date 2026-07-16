normalize_sample_aliases <- function(metadata) {
  x <- new_sample_identity(metadata)
  x[, display_sample := public_sample]
  x[]
}

public_sample_ids <- function(metadata, vcf_sample_ids) {
  resolve_sample_identity(metadata, vcf_sample_ids)
}

relabel_sample_matrix <- function(x, metadata) {
  out <- as.matrix(x)
  if (!is.null(rownames(out))) rownames(out) <- public_sample_ids(metadata, rownames(out))
  if (!is.null(colnames(out))) colnames(out) <- public_sample_ids(metadata, colnames(out))
  out
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
