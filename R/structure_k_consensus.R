structure_k_metric_specifications <- function(diagnostics) {
  candidates <- data.table::data.table(
    metric = c(
      "cv_error", "BIC", "cross_entropy", "marginal_likelihood",
      "mean_success", "silhouette", "calinski_harabasz", "davies_bouldin",
      "replicate_max_rmse"
    ),
    direction = c(
      "minimize", "minimize", "minimize", "maximize",
      "maximize", "maximize", "maximize", "minimize", "minimize"
    ),
    label = c(
      "Cross-validation error", "Bayesian information criterion",
      "Cross-entropy", "Marginal likelihood",
      "Mean cross-validation success", "Mean silhouette width",
      "Calinski-Harabasz separation", "Davies-Bouldin index",
      "Replicate membership root mean squared error"
    )
  )
  candidates[metric %in% names(diagnostics)]
}

structure_k_elbow_index <- function(k, objective) {
  best <- which.max(objective)
  if (length(k) < 3L || best <= 2L) return(best)
  candidates <- seq_len(best)
  gain <- objective[candidates] - objective[candidates[[1L]]]
  maximum_gain <- max(gain)
  if (!is.finite(maximum_gain) || maximum_gain <= 0) return(best)
  x <- (k[candidates] - min(k[candidates])) /
    max(diff(range(k[candidates])), 1)
  y <- gain / maximum_gain
  deviation <- y - x
  if (max(deviation) <= sqrt(.Machine$double.eps)) return(best)
  candidates[[which.max(deviation)]]
}

structure_k_parsimonious_index <- function(k, values, direction, standard_error,
                                           tolerance_fraction) {
  objective <- if (identical(direction, "minimize")) -values else values
  best <- which.max(objective)
  metric_span <- diff(range(values))
  tolerance <- tolerance_fraction * if (is.finite(metric_span)) metric_span else 0
  if (length(standard_error) == length(values) &&
      is.finite(standard_error[[best]]) && standard_error[[best]] > 0) {
    tolerance <- max(tolerance, standard_error[[best]])
  }
  eligible <- if (identical(direction, "minimize")) {
    values <= values[[best]] + tolerance
  } else {
    values >= values[[best]] - tolerance
  }
  which(eligible)[[which.min(k[eligible])]]
}

normalize_structure_k_votes <- function(additional_votes, available_k) {
  if (is.null(additional_votes) || !length(additional_votes)) {
    return(data.table::data.table())
  }
  if (is.atomic(additional_votes) && !is.null(names(additional_votes))) {
    votes <- data.table::data.table(
      method = names(additional_votes),
      K = as.integer(additional_votes)
    )
  } else {
    votes <- data.table::as.data.table(additional_votes)
  }
  if (!all(c("method", "K") %in% names(votes))) {
    stop("Additional K-selection votes require method and K columns", call. = FALSE)
  }
  votes <- data.table::copy(votes)
  votes[, K := as.integer(K)]
  if (anyNA(votes$K) || any(!votes$K %in% available_k)) {
    stop("Additional K-selection votes must use evaluated K values", call. = FALSE)
  }
  if (!"metric" %in% names(votes)) votes[, metric := "native recommendation"]
  if (!"rule" %in% names(votes)) votes[, rule := "backend recommendation"]
  if (!"direction" %in% names(votes)) votes[, direction := NA_character_]
  votes[, .(method = as.character(method), metric = as.character(metric),
            rule = as.character(rule), direction = as.character(direction), K)]
}

select_structure_k_consensus <- function(diagnostics, additional_votes = NULL,
                                         tolerance_fraction = 0.02) {
  x <- data.table::copy(data.table::as.data.table(diagnostics))
  if (!"K" %in% names(x) || anyDuplicated(x$K)) {
    stop("Diagnostics require unique K values", call. = FALSE)
  }
  x[, K := as.integer(K)]
  if (anyNA(x$K) || any(x$K < 1L)) {
    stop("Diagnostics require positive integer K values", call. = FALSE)
  }
  if (!is.numeric(tolerance_fraction) || length(tolerance_fraction) != 1L ||
      !is.finite(tolerance_fraction) || tolerance_fraction < 0) {
    stop("tolerance_fraction must be one finite nonnegative value", call. = FALSE)
  }
  data.table::setorder(x, K)
  specifications <- structure_k_metric_specifications(x)
  score_rows <- list()
  vote_rows <- list()

  for (i in seq_len(nrow(specifications))) {
    metric <- specifications$metric[[i]]
    direction <- specifications$direction[[i]]
    label <- specifications$label[[i]]
    values <- suppressWarnings(as.numeric(x[[metric]]))
    finite <- is.finite(values)
    if (!any(finite)) next
    k <- x$K[finite]
    values <- values[finite]
    if (length(unique(signif(values, 15L))) < 2L) next
    objective <- if (identical(direction, "minimize")) -values else values
    objective_span <- diff(range(objective))
    desirability <- if (is.finite(objective_span) && objective_span > 0) {
      (objective - min(objective)) / objective_span
    } else {
      rep(1, length(objective))
    }
    score_rows[[length(score_rows) + 1L]] <- data.table::data.table(
      K = k, metric = metric, metric_label = label,
      direction = direction, value = values, desirability = desirability
    )

    optimum <- which.max(objective)
    elbow <- structure_k_elbow_index(k, objective)
    se_name <- paste0(metric, "_se")
    standard_error <- if (se_name %in% names(x)) {
      suppressWarnings(as.numeric(x[[se_name]][finite]))
    } else {
      rep(NA_real_, length(k))
    }
    has_standard_error <- is.finite(standard_error[[optimum]]) &&
      standard_error[[optimum]] > 0
    parsimonious <- structure_k_parsimonious_index(
      k, values, direction, standard_error, tolerance_fraction
    )
    rules <- c(
      "optimum", "elbow",
      if (has_standard_error) "one standard error" else "near-optimum plateau"
    )
    indices <- c(optimum, elbow, parsimonious)
    vote_rows[[length(vote_rows) + 1L]] <- data.table::data.table(
      method = paste(label, rules), metric = metric,
      rule = rules, direction = direction, K = k[indices]
    )
  }

  scores <- data.table::rbindlist(score_rows, fill = TRUE)
  votes <- data.table::rbindlist(vote_rows, fill = TRUE)
  votes <- data.table::rbindlist(
    list(votes, normalize_structure_k_votes(additional_votes, x$K)),
    fill = TRUE
  )
  if (!nrow(votes)) {
    stop("No finite K-selection metrics or backend recommendations were available",
         call. = FALSE)
  }

  vote_summary <- votes[, .(votes = .N), by = K]
  vote_summary <- merge(data.table::data.table(K = x$K), vote_summary,
                        by = "K", all.x = TRUE, sort = TRUE)
  vote_summary[is.na(votes), votes := 0L]
  total_votes <- nrow(votes)
  vote_summary[, vote_fraction := votes / total_votes]
  winning_votes <- max(vote_summary$votes)
  candidates <- vote_summary[votes == winning_votes, K]
  tie_break <- "majority vote"
  if (length(candidates) > 1L && nrow(scores)) {
    support <- scores[K %in% candidates, .(
      mean_desirability = mean(desirability, na.rm = TRUE)
    ), by = K]
    best_support <- max(support$mean_desirability)
    candidates <- support[mean_desirability == best_support, K]
    tie_break <- "majority vote, then mean normalized diagnostic support"
  }
  if (length(candidates) > 1L) {
    tie_break <- paste0(tie_break, ", then the simpler model")
  }
  consensus_k <- min(candidates)
  consensus <- data.table::data.table(
    consensus_k = as.integer(consensus_k),
    winning_votes = as.integer(winning_votes),
    total_votes = as.integer(total_votes),
    agreement = winning_votes / total_votes,
    tie_break = tie_break,
    tolerance_fraction = tolerance_fraction
  )

  structure(
    list(
      best_by_method = votes,
      scores = scores,
      vote_summary = vote_summary,
      consensus = consensus,
      consensus_k = as.integer(consensus_k)
    ),
    class = c("PopgenVCFStructureKSelection", "list")
  )
}

plot_structure_k_selection <- function(selection, cfg, dirs, stem, title) {
  if (is.null(selection)) return(invisible(NULL))
  scores <- data.table::copy(selection$scores)
  votes <- data.table::copy(selection$vote_summary)
  panel_order <- character()
  if (nrow(scores)) {
    scores[, panel := metric_label]
    panel_order <- unique(scores$panel)
  }
  votes[, `:=`(
    panel = "Method votes",
    desirability = vote_fraction,
    label = as.character(votes)
  )]
  panel_order <- c(panel_order, "Method votes")
  panel_data <- data.table::rbindlist(list(
    if (nrow(scores)) scores[, .(K, panel, desirability)] else NULL,
    votes[, .(K, panel, desirability)]
  ), fill = TRUE)
  panel_data[, panel := factor(panel, levels = panel_order)]
  vertical <- unique(panel_data[, .(panel)])
  vertical[, consensus_k := selection$consensus_k]

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = votes,
      ggplot2::aes(x = K, y = desirability),
      fill = "#EE6677", width = 0.7
    ) +
    ggplot2::geom_text(
      data = votes[votes > 0L],
      ggplot2::aes(x = K, y = desirability, label = label),
      vjust = -0.35, size = 3
    )
  if (nrow(scores)) {
    p <- p +
      ggplot2::geom_line(
        data = scores,
        ggplot2::aes(x = K, y = desirability, group = metric),
        colour = "#4477AA", linewidth = 0.7
      ) +
      ggplot2::geom_point(
        data = scores,
        ggplot2::aes(x = K, y = desirability),
        colour = "#4477AA", size = 2.2
      )
  }
  p <- p +
    ggplot2::geom_vline(
      data = vertical,
      ggplot2::aes(xintercept = consensus_k),
      colour = "grey20", linetype = 2, linewidth = 0.5
    ) +
    ggplot2::facet_wrap(~panel, ncol = 2) +
    ggplot2::scale_x_continuous(breaks = sort(unique(panel_data$K))) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.08), breaks = seq(0, 1, 0.25),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "Consensus number of clusters = %d; %d of %d method votes agree",
        selection$consensus_k, selection$consensus$winning_votes,
        selection$consensus$total_votes
      ),
      x = "Number of clusters (K)", y = "Relative support"
    ) +
    theme_publication()
  save_plot(p, stem, dirs, cfg$output$figure_formats, 10, 7, cfg$output$dpi)
  invisible(p)
}

write_structure_k_selection <- function(selection, dirs, stem) {
  if (is.null(selection)) return(invisible(NULL))
  write_tsv(selection$best_by_method,
            file.path(dirs$tables, paste0(stem, "_methods.tsv")))
  if (nrow(selection$scores)) {
    write_tsv(selection$scores,
              file.path(dirs$tables, paste0(stem, "_scores.tsv")))
  }
  write_tsv(selection$vote_summary,
            file.path(dirs$tables, paste0(stem, "_votes.tsv")))
  write_tsv(selection$consensus,
            file.path(dirs$tables, paste0(stem, "_consensus.tsv")))
  invisible(selection)
}
