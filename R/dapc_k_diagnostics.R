dapc_cross_validation_success <- function(cross_validation) {
  if (is.null(cross_validation)) return(NA_real_)
  success <- cross_validation[[
    "Mean Successful Assignment by Number of PCs of PCA"
  ]]
  success <- suppressWarnings(as.numeric(success))
  success <- success[is.finite(success)]
  if (length(success)) max(success) else NA_real_
}

calinski_harabasz_score <- function(coordinates, groups) {
  coordinates <- as.matrix(coordinates)
  groups <- factor(groups)
  n <- nrow(coordinates)
  k <- nlevels(groups)
  if (n < 3L || k < 2L || k >= n || any(!is.finite(coordinates))) {
    return(NA_real_)
  }
  grand_mean <- colMeans(coordinates)
  within <- 0
  between <- 0
  for (level in levels(groups)) {
    block <- coordinates[groups == level, , drop = FALSE]
    center <- colMeans(block)
    within <- within + sum((block - rep(
      center, each = nrow(block)
    ))^2)
    between <- between + nrow(block) * sum((center - grand_mean)^2)
  }
  if (!is.finite(within) || within <= 0 || !is.finite(between)) {
    return(NA_real_)
  }
  (between / (k - 1L)) / (within / (n - k))
}

davies_bouldin_index <- function(coordinates, groups) {
  coordinates <- as.matrix(coordinates)
  groups <- factor(groups)
  levels <- levels(groups)
  if (length(levels) < 2L || any(!is.finite(coordinates))) {
    return(NA_real_)
  }
  centers <- matrix(NA_real_, nrow = length(levels), ncol = ncol(coordinates))
  scatter <- numeric(length(levels))
  for (i in seq_along(levels)) {
    block <- coordinates[groups == levels[[i]], , drop = FALSE]
    centers[i, ] <- colMeans(block)
    scatter[[i]] <- mean(sqrt(rowSums(sweep(block, 2L, centers[i, ], "-")^2)))
  }
  distances <- matrix(0, nrow = length(levels), ncol = length(levels))
  for (i in seq_along(levels)) {
    for (j in seq_along(levels)) {
      if (i == j) next
      distances[i, j] <- sqrt(sum((centers[i, ] - centers[j, ])^2))
    }
  }
  ratios <- numeric(length(levels))
  for (i in seq_along(levels)) {
    candidates <- (scatter[[i]] + scatter) / distances[i, ]
    candidates <- candidates[is.finite(candidates)]
    if (!length(candidates)) return(NA_real_)
    ratios[[i]] <- max(candidates)
  }
  if (any(!is.finite(ratios))) return(NA_real_)
  mean(ratios)
}
