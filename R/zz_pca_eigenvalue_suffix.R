# SNPRelate's exact PCA backend may return one eigenvalue slot per retained
# sample even when a smaller eigen.cnt was requested. Unused trailing slots are
# represented as NA/NaN. This late-loaded compatibility definition keeps the
# finite leading eigensystem while remaining strict about infinities, internal
# missing-value gaps, and spectra with fewer than two usable components.
normalize_pca_eigenvalues <- function(eigenvalues,
                                      relative_tolerance = sqrt(.Machine$double.eps)) {
  eigenvalues <- as.numeric(eigenvalues)
  if (!length(eigenvalues)) {
    stop("SNPRelate PCA returned no eigenvalues", call. = FALSE)
  }
  if (any(is.infinite(eigenvalues))) {
    stop(
      sprintf(
        "SNPRelate PCA returned %d infinite eigenvalue(s)",
        sum(is.infinite(eigenvalues))
      ),
      call. = FALSE
    )
  }

  missing <- is.na(eigenvalues)
  discarded_nonfinite <- 0L
  if (any(missing)) {
    first_missing <- which(missing)[[1L]]
    prefix_length <- first_missing - 1L
    trailing <- seq.int(first_missing, length(eigenvalues))
    if (prefix_length < 2L || any(!missing[trailing])) {
      stop(
        sprintf(
          "SNPRelate PCA returned %d non-finite eigenvalue(s) within the requested eigensystem",
          sum(missing)
        ),
        call. = FALSE
      )
    }
    discarded_nonfinite <- length(eigenvalues) - prefix_length
    eigenvalues <- eigenvalues[seq_len(prefix_length)]
  }

  relative_tolerance <- as.numeric(relative_tolerance)[1L]
  if (!is.finite(relative_tolerance) || relative_tolerance < 0) {
    stop("relative_tolerance must be one finite nonnegative value", call. = FALSE)
  }
  scale <- max(abs(eigenvalues))
  tolerance <- max(
    .Machine$double.eps * max(1, scale),
    relative_tolerance * scale
  )
  materially_negative <- eigenvalues < -tolerance
  if (any(materially_negative)) {
    stop(
      sprintf(
        paste0(
          "SNPRelate PCA returned materially negative eigenvalues ",
          "(minimum=%.6g; tolerance=%.6g)"
        ),
        min(eigenvalues), tolerance
      ),
      call. = FALSE
    )
  }

  adjusted <- eigenvalues
  adjusted[abs(adjusted) <= tolerance] <- 0
  if (sum(adjusted) <= 0) {
    stop("SNPRelate PCA retained no positive genetic variance", call. = FALSE)
  }

  list(
    values = adjusted,
    adjusted_negative = sum(eigenvalues < 0),
    discarded_nonfinite = discarded_nonfinite,
    tolerance = tolerance
  )
}
