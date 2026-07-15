#' Select ancestry K from backend fit metrics and replicate stability
#'
#' Summarizes replicate-level fit metrics by backend and K, detects stable
#' plateaus, and returns transparent backend-specific and cross-backend
#' recommendations. Metrics that represent error are minimized; likelihood-like
#' metrics are maximized.
#'
#' @param x A `PopgenVCFAncestryResult` or list of canonical ancestry
#'   replicates spanning one or more K values.
#' @param metric Optional metric name. When omitted, a backend-specific metric
#'   is selected from available values.
#' @param direction Optional optimization direction: `minimize` or `maximize`.
#' @param consensus Optional list of `PopgenVCFAncestryConsensus` objects used
#'   to incorporate replicate stability for each backend and K.
#' @param confidence Confidence level for replicate metric intervals.
#' @param plateau_fraction Relative improvement threshold used to identify the
#'   first K on the performance plateau.
#' @param stability_threshold Minimum stability considered strong.
#' @return A validated `PopgenVCFKSelection` object.
#' @export
select_ancestry_k <- function(x, metric = NULL, direction = NULL,
                              consensus = NULL, confidence = 0.95,
                              plateau_fraction = 0.01,
                              stability_threshold = 0.95) {
  reps <- if (inherits(x, "PopgenVCFAncestryResult")) {
    validate_ancestry_result(x)
    x$replicates
  } else x
  if (!is.list(reps) || !length(reps)) stop("x must contain ancestry replicates", call. = FALSE)
  invisible(lapply(reps, validate_ancestry_replicate))
  if (!is.numeric(confidence) || length(confidence) != 1L || confidence <= 0 || confidence >= 1) stop("confidence must lie strictly between zero and one", call. = FALSE)
  if (!is.numeric(plateau_fraction) || length(plateau_fraction) != 1L || !is.finite(plateau_fraction) || plateau_fraction < 0) stop("plateau_fraction must be nonnegative", call. = FALSE)
  if (!is.numeric(stability_threshold) || length(stability_threshold) != 1L || stability_threshold < 0 || stability_threshold > 1) stop("stability_threshold must lie in [0, 1]", call. = FALSE)

  stability <- ancestry_stability_lookup(consensus)
  backends <- sort(unique(vapply(reps, `[[`, character(1L), "backend")))
  summaries <- list()
  recommendations <- vector("list", length(backends))

  for (b in seq_along(backends)) {
    backend <- backends[[b]]
    backend_reps <- reps[vapply(reps, function(z) identical(z$backend, backend), logical(1L))]
    available <- unique(unlist(lapply(backend_reps, function(z) names(z$metrics)), use.names = FALSE))
    selected_metric <- if (is.null(metric)) ancestry_default_metric(backend, available) else as.character(metric)[1L]
    if (!nzchar(selected_metric) || !selected_metric %in% available) stop(sprintf("metric '%s' is unavailable for backend '%s'", selected_metric, backend), call. = FALSE)
    selected_direction <- if (is.null(direction)) ancestry_metric_direction(selected_metric) else match.arg(direction, c("minimize", "maximize"))

    rows <- lapply(backend_reps, function(z) {
      if (!selected_metric %in% names(z$metrics)) return(NULL)
      data.table::data.table(backend = backend, k = z$k, replicate = z$replicate, metric = selected_metric, value = unname(z$metrics[[selected_metric]]))
    })
    raw <- data.table::rbindlist(rows)
    if (!nrow(raw)) stop(sprintf("no finite values available for metric '%s'", selected_metric), call. = FALSE)
    alpha <- (1 - confidence) / 2
    tab <- raw[, .(
      n_replicates = .N,
      mean = mean(value),
      median = stats::median(value),
      sd = if (.N == 1L) 0 else stats::sd(value),
      se = if (.N == 1L) 0 else stats::sd(value) / sqrt(.N),
      lower = stats::quantile(value, alpha, names = FALSE, type = 8),
      upper = stats::quantile(value, 1 - alpha, names = FALSE, type = 8)
    ), by = .(backend, k, metric)]
    data.table::setorder(tab, k)
    tab[, direction := selected_direction]
    tab[, stability := vapply(k, function(kk) ancestry_lookup_stability(stability, backend, kk), numeric(1L))]
    tab[, objective := if (selected_direction == "minimize") -mean else mean]
    span <- diff(range(tab$mean))
    scale <- if (is.finite(span) && span > 0) span else max(abs(tab$mean), 1)
    tab[, improvement := c(NA_real_, diff(objective))]
    tab[, relative_improvement := improvement / scale]

    best_idx <- which.max(tab$objective)
    best_k <- tab$k[[best_idx]]
    plateau_k <- best_k
    if (best_idx > 1L) {
      first_small <- which(tab$k[-1L] <= best_k & tab$relative_improvement[-1L] < plateau_fraction)
      if (length(first_small)) plateau_k <- tab$k[first_small[[1L]] + 1L]
    }
    stable_candidates <- which(tab$k <= best_k & !is.na(tab$stability) & tab$stability >= stability_threshold)
    recommended_k <- if (length(stable_candidates)) {
      stable_on_plateau <- stable_candidates[tab$k[stable_candidates] >= plateau_k]
      if (length(stable_on_plateau)) min(tab$k[stable_on_plateau]) else tab$k[stable_candidates[[which.max(tab$objective[stable_candidates])]]]
    } else plateau_k
    rec_idx <- match(recommended_k, tab$k)
    stability_value <- tab$stability[[rec_idx]]
    gap <- abs(tab$objective[[best_idx]] - tab$objective[[rec_idx]]) / scale
    confidence_score <- max(0, min(1, 1 - gap))
    if (!is.na(stability_value)) confidence_score <- mean(c(confidence_score, stability_value))
    confidence_label <- if (confidence_score >= 0.85) "high" else if (confidence_score >= 0.65) "moderate" else "low"
    reasons <- c(
      sprintf("%s optimum at K=%d", selected_metric, best_k),
      sprintf("first stable plateau at K=%d using %.3g relative-improvement threshold", plateau_k, plateau_fraction)
    )
    if (!is.na(stability_value)) reasons <- c(reasons, sprintf("replicate stability at recommended K is %.3f", stability_value))
    if (recommended_k != best_k) reasons <- c(reasons, "selected the simpler stable model with negligible fit loss")

    tab[, recommended := k == recommended_k]
    summaries[[b]] <- tab
    recommendations[[b]] <- data.table::data.table(
      backend = backend, metric = selected_metric, direction = selected_direction,
      best_k = best_k, plateau_k = plateau_k, recommended_k = recommended_k,
      stability = stability_value, confidence_score = confidence_score,
      confidence = confidence_label, reasons = paste(reasons, collapse = "; ")
    )
  }

  summary_table <- data.table::rbindlist(summaries, fill = TRUE)
  recommendation_table <- data.table::rbindlist(recommendations, fill = TRUE)
  votes <- recommendation_table[, .N, by = recommended_k][order(-N, recommended_k)]
  overall_k <- votes$recommended_k[[1L]]
  agreement <- votes$N[[1L]] / nrow(recommendation_table)
  overall_confidence_score <- mean(c(agreement, recommendation_table[recommended_k == overall_k, confidence_score]))
  overall_confidence <- if (overall_confidence_score >= 0.85) "high" else if (overall_confidence_score >= 0.65) "moderate" else "low"
  overall_reason <- sprintf("K=%d received %d of %d backend recommendations (agreement %.1f%%)", overall_k, votes$N[[1L]], nrow(recommendation_table), 100 * agreement)

  out <- structure(list(
    summary = summary_table,
    recommendations = recommendation_table,
    overall_k = as.integer(overall_k),
    agreement = agreement,
    confidence_score = overall_confidence_score,
    confidence = overall_confidence,
    reason = overall_reason,
    confidence_level = confidence,
    plateau_fraction = plateau_fraction,
    stability_threshold = stability_threshold
  ), class = "PopgenVCFKSelection")
  validate_ancestry_k_selection(out)
}

ancestry_default_metric <- function(backend, available) {
  priorities <- switch(backend,
    admixture = c("cv_error", "cv", "cross_validation_error"),
    faststructure = c("marginal_likelihood", "likelihood", "complexity"),
    snmf = c("cross_entropy", "crossentropy"),
    character()
  )
  hit <- priorities[priorities %in% available]
  if (length(hit)) hit[[1L]] else if (length(available)) sort(available)[[1L]] else ""
}

ancestry_metric_direction <- function(metric) {
  if (grepl("error|entropy|loss|deviance|aic|bic|complexity", metric, ignore.case = TRUE)) "minimize" else "maximize"
}

ancestry_stability_lookup <- function(consensus) {
  if (is.null(consensus)) return(data.table::data.table(backend = character(), k = integer(), stability = numeric()))
  if (inherits(consensus, "PopgenVCFAncestryConsensus")) consensus <- list(consensus)
  if (!is.list(consensus)) stop("consensus must be a consensus object or list", call. = FALSE)
  data.table::rbindlist(lapply(consensus, function(z) {
    validate_ancestry_consensus(z)
    data.table::data.table(backend = z$backend, k = z$k, stability = z$global_stability)
  }))
}

ancestry_lookup_stability <- function(tab, backend_name, k_value) {
  if (!nrow(tab)) return(NA_real_)
  hit <- tab[tab$backend == backend_name & tab$k == k_value, stability]
  if (length(hit)) hit[[1L]] else NA_real_
}

#' Validate an ancestry K-selection result
#' @param x A `PopgenVCFKSelection` object.
#' @return `x`, invisibly, when valid.
#' @export
validate_ancestry_k_selection <- function(x) {
  if (!inherits(x, "PopgenVCFKSelection")) stop("x must be a PopgenVCFKSelection", call. = FALSE)
  if (!nrow(x$summary) || !nrow(x$recommendations)) stop("K selection tables must not be empty", call. = FALSE)
  if (!is.finite(x$overall_k) || x$overall_k < 1L) stop("overall_k must be a positive integer", call. = FALSE)
  if (!is.finite(x$agreement) || x$agreement < 0 || x$agreement > 1) stop("agreement must lie in [0, 1]", call. = FALSE)
  if (!is.finite(x$confidence_score) || x$confidence_score < 0 || x$confidence_score > 1) stop("confidence_score must lie in [0, 1]", call. = FALSE)
  if (!x$confidence %in% c("low", "moderate", "high")) stop("confidence label is invalid", call. = FALSE)
  invisible(x)
}

#' Generate manuscript-ready ancestry K-selection text
#' @param x A `PopgenVCFKSelection` object.
#' @return Named character vector containing methods and results text.
#' @export
ancestry_k_selection_text <- function(x) {
  validate_ancestry_k_selection(x)
  methods <- sprintf("Ancestry models were evaluated across K using replicate-level backend fit metrics, empirical %.1f%% intervals, plateau detection, and replicate stability. The first stable model with negligible fit loss was preferred over unnecessarily complex models.", 100 * x$confidence_level)
  results <- sprintf("The consensus ancestry model recommendation was K=%d with %s confidence. %s.", x$overall_k, x$confidence, x$reason)
  c(methods = methods, results = results)
}

#' @export
print.PopgenVCFKSelection <- function(x, ...) {
  cat("<PopgenVCFKSelection> K=", x$overall_k, " confidence=", x$confidence,
      " agreement=", format(x$agreement, digits = 3), "\n", sep = "")
  invisible(x)
}
