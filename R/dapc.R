genlight_from_gds <- function(geno, sample_ids, metadata, snp_ids = NULL) {
  gl <- adegenet::as.genlight(geno)
  adegenet::indNames(gl) <- public_sample_ids(metadata, sample_ids)
  adegenet::pop(gl) <- factor(metadata[match(sample_ids, sample), population])
  if (!is.null(snp_ids)) adegenet::locNames(gl) <- as.character(snp_ids)
  gl
}

dapc_loading_table <- function(model, snp_ids, chromosome, position) {
  # adegenet::dapc()'s var.contr rownames are positional indices ("1", "2", ...),
  # not the genlight object's locNames -- rows are guaranteed to be in the same
  # per-locus order as snp_ids/chromosome/position (nothing reorders loci between
  # genlight_from_gds() and dapc()), so alignment must be positional, not by name.
  contr <- as.matrix(model$var.contr)
  snp_ids <- as.character(snp_ids)
  if (nrow(contr) != length(snp_ids)) {
    stop("var.contr row count does not match the supplied snp_ids.", call. = FALSE)
  }
  out <- data.table::rbindlist(lapply(colnames(contr), function(axis) {
    data.table::data.table(
      axis = axis, snp_id = snp_ids,
      chromosome = chromosome, position = position,
      contribution = as.numeric(contr[, axis])
    )
  }))
  out[, .axis_sort_key := natural_sort_key(axis)]
  data.table::setorder(out, .axis_sort_key, -contribution)
  out[, .axis_sort_key := NULL]
  out
}

classification_accuracy_permutation <- function(predicted, truth) {
  predicted <- factor(predicted); truth <- factor(truth)
  tab <- table(predicted, truth)
  k <- max(nrow(tab), ncol(tab))
  padded <- matrix(0, k, k); padded[seq_len(nrow(tab)), seq_len(ncol(tab))] <- tab
  assignment <- solve_cluster_assignment(padded)
  sum(padded[cbind(seq_len(k), assignment)]) / sum(padded)
}

extract_dapc_membership <- function(model, sample_ids) {
  post <- model$posterior
  if (is.null(post)) {
    grp <- factor(model$assign)
    post <- stats::model.matrix(~ grp - 1)
  }
  post <- normalize_q_matrix(post)
  rownames(post) <- sample_ids
  post
}

compute_dapc_shared_pca <- function(gl, max_pca) {
  adegenet::glPca(
    gl, center = TRUE, scale = FALSE, nf = max_pca,
    loadings = TRUE, returnDotProd = TRUE
  )
}


run_dapc_k_task <- function(k, gl, shared_pca, max_pca, sample_ids,
                            public_ids, metadata, truth, cross_validate,
                            replicate_seeds, chromosome = NULL, position = NULL) {
  reps <- list()
  primary <- NULL
  cv_success <- numeric()
  for (rep_seed in unique(as.integer(replicate_seeds))) {
    set.seed(rep_seed + k)
    cluster_fit <- adegenet::find.clusters(
      gl, n.pca = max_pca, n.clust = k, choose.n.clust = FALSE,
      glPca = shared_pca
    )
    grp <- cluster_fit$grp
    n_da <- max(1L, min(k - 1L, 10L))
    n_pca <- min(max_pca, max(2L, floor(length(sample_ids) * .8)))
    cv <- NULL
    if (cross_validate) {
      # adegenet::xvalDapc() does forward `...` down into boot::boot()'s own
      # native parallel = "multicore"/ncpus (via its internal .get.prop.pred()
      # helper) -- the mechanism the widely-used "Population Genetics in R"
      # DAPC tutorial demonstrates -- but deliberately NOT used here: this
      # call resamples with sim = "parametric", which per boot::boot()'s own
      # documentation resamples *inside* the worker processes, each choosing
      # its own separate, non-reproducible seed. Confirmed directly on this
      # package's real quickstart dataset: identical data/seed gave n.pca =
      # 10 (replicate RMSE ~0) serial vs. n.pca = 40 (RMSE 0.076, exceeding
      # the stability threshold) with parallel = "multicore" enabled -- a
      # materially different, less reproducible result, not just a faster
      # one. Every other parallel path in this codebase is verified
      # byte-identical regardless of thread count; this one cannot be
      # without reimplementing xvalDapc's own resampling loop, so it stays
      # serial.
      cv <- tryCatch(adegenet::xvalDapc(
        gl, grp, n.pca.max = n_pca, training.set = .9,
        result = "groupMean", center = TRUE, scale = FALSE,
        n.pca = NULL, n.rep = 30, xval.plot = FALSE
      ), error = function(e) NULL)
      if (!is.null(cv)) {
        selected <- suppressWarnings(as.integer(
          cv$`Number of PCs Achieving Highest Mean Success`
        ))
        if (length(selected) && is.finite(selected)) n_pca <- selected
      }
    }
    cv_success <- c(cv_success, dapc_cross_validation_success(cv))
    model <- adegenet::dapc(
      gl, pop = grp, n.pca = n_pca, n.da = n_da,
      glPca = shared_pca
    )
    membership <- extract_dapc_membership(model, public_ids)
    reps[[as.character(rep_seed)]] <- membership
    if (is.null(primary)) {
      primary <- list(
        model = model, groups = grp, cv = cv,
        n_pca = n_pca, n_da = n_da,
        membership = membership,
        bic = cluster_fit$Kstat %||% NA_real_
      )
    }
  }

  model <- primary$model
  grp <- primary$groups
  coord <- data.table::as.data.table(model$ind.coord, keep.rownames = "sample")
  coord[, vcf_sample := sample_ids[match(sample, public_ids)]]
  data.table::set(
    coord, j = "population",
    value = metadata$population[match(coord$vcf_sample, metadata$sample)]
  )
  data.table::set(coord, j = "cluster", value = as.character(grp))
  reproducibility <- if (length(reps) > 1L) {
    structure_reproducibility(reps)
  } else {
    NULL
  }
  assignment_accuracy <- classification_accuracy_permutation(grp, truth)
  loadings <- if (!is.null(chromosome)) {
    dapc_loading_table(model, adegenet::locNames(gl), chromosome, position)
  } else {
    NULL
  }

  list(
    key = as.character(k),
    model = list(
      model = model, coordinates = coord, groups = grp, cv = primary$cv,
      membership = primary$membership, replicate_membership = reps,
      reproducibility = reproducibility, loadings = loadings
    ),
    diagnostic = data.table::data.table(
      K = k, n_pca = primary$n_pca, n_da = primary$n_da,
      BIC = if (length(primary$bic)) as.numeric(primary$bic)[1] else NA_real_,
      assignment_accuracy = assignment_accuracy,
      mean_success = if (any(is.finite(cv_success))) {
        mean(cv_success[is.finite(cv_success)])
      } else {
        NA_real_
      },
      calinski_harabasz = calinski_harabasz_score(shared_pca$scores, grp),
      davies_bouldin = davies_bouldin_index(shared_pca$scores, grp),
      replicate_max_rmse = if (is.null(reproducibility)) {
        NA_real_
      } else {
        max(reproducibility$metrics$rmse)
      },
      replicate_min_cluster_correlation = if (is.null(reproducibility)) {
        NA_real_
      } else {
        min(reproducibility$metrics$minimum_cluster_correlation)
      }
    ),
    replicate_membership = reps
  )
}

execute_dapc_k_tasks <- function(k_values, task, workers) {
  if (workers <= 1L) return(lapply(k_values, task))
  results <- parallel::mclapply(
    k_values, task, mc.cores = workers,
    mc.preschedule = FALSE, mc.set.seed = FALSE
  )
  check_mclapply_results(results, paste0("K = ", k_values), "DAPC task(s)")
  results
}

run_dapc_analysis <- function(geno, sample_ids, metadata, k_values, seed,
                              cross_validate = TRUE, replicate_seeds = seed,
                              threads = 1L, snp_ids = NULL, chromosome = NULL,
                              position = NULL) {
  valid_k <- sort(unique(as.integer(
    k_values[k_values >= 2L & k_values < length(sample_ids)]
  )))
  if (!length(valid_k)) {
    return(list(
      models = list(), diagnostics = data.table::data.table(),
      k_selection = NULL, replicate_membership = list()
    ))
  }

  public_ids <- public_sample_ids(metadata, sample_ids)
  gl <- genlight_from_gds(geno, sample_ids, metadata, snp_ids = snp_ids)
  max_pca <- max(2L, min(nrow(geno) - 1L, 100L))
  shared_pca <- compute_dapc_shared_pca(gl, max_pca)
  truth <- metadata$population[match(sample_ids, metadata$sample)]
  workers <- fork_worker_count(length(valid_k), threads)
  log_msg(
    "DAPC fitting ", length(valid_k), " K value(s) with ", workers,
    " worker(s) and one shared genotype PCA"
  )
  task <- function(k) run_dapc_k_task(
    k, gl = gl, shared_pca = shared_pca, max_pca = max_pca,
    sample_ids = sample_ids, public_ids = public_ids,
    metadata = metadata, truth = truth,
    cross_validate = cross_validate,
    replicate_seeds = replicate_seeds,
    chromosome = chromosome, position = position
  )
  results <- execute_dapc_k_tasks(valid_k, task, workers)
  keys <- vapply(results, `[[`, character(1L), "key")
  models <- stats::setNames(lapply(results, `[[`, "model"), keys)
  diagnostics <- data.table::rbindlist(
    lapply(results, `[[`, "diagnostic"), fill = TRUE
  )
  replicate_membership <- stats::setNames(
    lapply(results, `[[`, "replicate_membership"), keys
  )
  k_selection <- select_structure_k_if_informative(diagnostics)
  list(
    models = models, diagnostics = diagnostics,
    k_selection = k_selection,
    replicate_membership = replicate_membership
  )
}

dapc_reproducibility_annotation <- function(dapc, k, cfg) {
  rmse_threshold <- cfg$analyses$structure$reproducibility_rmse %||% 0.05
  corr_threshold <- cfg$analyses$structure$minimum_cluster_correlation %||% 0.90
  diagnostics <- data.table::as.data.table(dapc$diagnostics)
  row <- diagnostics[as.character(K) == as.character(k)]
  rmse <- if (nrow(row) && "replicate_max_rmse" %in% names(row)) {
    suppressWarnings(as.numeric(row$replicate_max_rmse[[1L]]))
  } else {
    NA_real_
  }
  # minimum_cluster_correlation's own companion diagnostic (structure_reproducibility()'s
  # per-replicate worst-cluster correlation to the reference replicate, R/population_structure.R)
  # -- absent from a hand-built diagnostics table (e.g. an older cache or a test fixture)
  # is treated the same as "not estimated", not as a hard failure.
  min_corr <- if (nrow(row) && "replicate_min_cluster_correlation" %in% names(row)) {
    suppressWarnings(as.numeric(row$replicate_min_cluster_correlation[[1L]]))
  } else {
    NA_real_
  }
  n_pca <- if (nrow(row) && "n_pca" %in% names(row)) {
    suppressWarnings(as.integer(row$n_pca[[1L]]))
  } else {
    NA_integer_
  }
  n_pca_line <- if (length(n_pca) && is.finite(n_pca)) {
    sprintf("%d PCA axis/axes retained for this model.", n_pca)
  } else {
    NA_character_
  }
  estimated <- !is.null(dapc$models[[as.character(k)]]$reproducibility)

  if (!estimated || !length(rmse) || !is.finite(rmse)) {
    text <- sprintf(
      "Replicate membership RMSE not estimated (stability threshold = %.4g).",
      rmse_threshold
    )
    return(list(
      text = paste(stats::na.omit(c(text, n_pca_line)), collapse = "\n"),
      unstable = FALSE, n_pca = n_pca
    ))
  }
  rmse_unstable <- rmse > rmse_threshold
  corr_unstable <- length(min_corr) > 0L && is.finite(min_corr) && min_corr < corr_threshold
  if (rmse_unstable || corr_unstable) {
    detail <- if (rmse_unstable && corr_unstable) {
      sprintf(
        "RMSE = %.4g > %.4g, minimum cluster correlation = %.4g < %.4g",
        rmse, rmse_threshold, min_corr, corr_threshold
      )
    } else if (rmse_unstable) {
      sprintf("RMSE = %.4g > %.4g", rmse, rmse_threshold)
    } else {
      sprintf("minimum cluster correlation = %.4g < %.4g", min_corr, corr_threshold)
    }
    text <- sprintf(
      paste0(
        "WARNING: DAPC replicate membership is unstable ",
        "(%s).\nAvoid interpreting these assignments."
      ),
      detail
    )
    return(list(
      text = paste(stats::na.omit(c(text, n_pca_line)), collapse = "\n"),
      unstable = TRUE, n_pca = n_pca
    ))
  }
  text <- if (is.finite(min_corr)) {
    sprintf(
      "Replicate membership RMSE = %.4g (stability threshold = %.4g); minimum cluster correlation = %.4g (threshold = %.4g).",
      rmse, rmse_threshold, min_corr, corr_threshold
    )
  } else {
    sprintf(
      "Replicate membership RMSE = %.4g (stability threshold = %.4g).",
      rmse, rmse_threshold
    )
  }
  list(
    text = paste(stats::na.omit(c(text, n_pca_line)), collapse = "\n"),
    unstable = FALSE, n_pca = n_pca
  )
}

plot_dapc_loading_manhattan <- function(loadings, k, n_pca, cfg, dirs, profile) {
  layout <- manhattan_layout(loadings$chromosome, loadings$position)
  bp_breaks <- manhattan_bp_breaks(loadings$chromosome, loadings$position, layout$offset)
  loadings <- data.table::copy(loadings)
  loadings[, x := layout$x]
  loadings[, chrom_group := factor(match(chromosome, layout$ticks$chromosome) %% 2L)]
  loadings[, axis := factor(axis, levels = natural_sort_levels(axis))]
  colours <- expand_figure_palette(profile, 2L, "colours")
  base_size <- figure_base_size(cfg)
  p <- ggplot2::ggplot(loadings, ggplot2::aes(x = x, y = contribution, colour = chrom_group)) +
    ggplot2::geom_point(size = 1, alpha = .75, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
    ggplot2::facet_wrap(~axis, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = sprintf("Discriminant analysis SNP loadings (K = %s)", k),
      subtitle = if (length(n_pca) && is.finite(n_pca)) {
        sprintf("%d PCA axis/axes retained for this model.", n_pca)
      } else {
        NULL
      },
      x = "Chromosome position", y = "Contribution to discriminant function"
    ) + theme_publication(base_size) +
    ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines"))
  last_axis <- levels(loadings$axis)[length(levels(loadings$axis))]
  n_axes <- data.table::uniqueN(loadings$axis)
  p <- manhattan_chromosome_row(
    p, layout$ticks, range(loadings$contribution[loadings$axis == last_axis], na.rm = TRUE),
    base_size, facet_var = "axis", facet_last_level = last_axis, facet_levels = levels(loadings$axis),
    plot_width_in = 10
  )
  save_plot(
    p, sprintf("15_DAPC_loadings_manhattan_K%s", k), dirs,
    cfg$output$figure_formats, 10, max(4, 2.2 * n_axes), cfg$output$dpi
  )
  invisible(p)
}

plot_dapc_loading_ranked <- function(loadings, k, n_pca, cfg, dirs, profile) {
  ranked <- data.table::copy(loadings)
  ranked[, axis := factor(axis, levels = natural_sort_levels(axis))]
  data.table::setorder(ranked, axis, -contribution)
  ranked[, rank := seq_len(.N), by = axis]
  colour <- expand_figure_palette(profile, 1L, "colours")
  p <- ggplot2::ggplot(ranked, ggplot2::aes(x = rank, y = contribution)) +
    ggplot2::geom_point(size = 1, alpha = .75, colour = colour) +
    ggplot2::facet_wrap(~axis, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = sprintf("Discriminant analysis SNP loadings, ranked (K = %s)", k),
      subtitle = if (length(n_pca) && is.finite(n_pca)) {
        sprintf("%d PCA axis/axes retained for this model.", n_pca)
      } else {
        NULL
      },
      x = "SNP rank (descending contribution)", y = "Contribution to discriminant function"
    ) + theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines"))
  n_axes <- data.table::uniqueN(ranked$axis)
  save_plot(
    p, sprintf("16_DAPC_loadings_ranked_K%s", k), dirs,
    cfg$output$figure_formats, 8, max(4, 2.2 * n_axes), cfg$output$dpi
  )
  invisible(p)
}

# adegenet::xvalDapc() (called once per K in run_dapc_k_task(), gated behind
# cfg$analyses$dapc_cross_validation) already picks the number of retained
# PCs by cross-validated assignment success -- the full per-n.pca curve
# behind that choice was computed and kept on `cv` all along, just never
# rendered (xvalDapc(..., xval.plot = FALSE)). This draws the same
# diagnostic as adegenet's own built-in xval.plot = TRUE scatter (its own
# axis labels are literally "Number of PCA axes retained" and "Proportion
# of successful outcome prediction"), reusing the exact retained `cv`
# object rather than recomputing anything: every individual bootstrap
# replicate's outcome (not just the per-n.pca mean, which alone can look
# like a clean, confident peak even when the underlying replicates are
# highly variable -- exactly the case a user needs to see to judge whether
# the auto-selected PC count is trustworthy or an artifact of too few
# replicates/too small a training set on a real dataset), the full
# 2.5/50/97.5% random-chance reference band (not just the median), the
# mean curve on top, and the selected PC count marked directly on it.
plot_dapc_xval <- function(cv, k, cfg, dirs, profile) {
  if (is.null(cv)) return(invisible(NULL))
  success <- cv[["Mean Successful Assignment by Number of PCs of PCA"]]
  if (is.null(success) || !length(success)) return(invisible(NULL))
  df <- data.frame(
    n_pca = suppressWarnings(as.integer(names(success))),
    success = suppressWarnings(as.numeric(success))
  )
  df <- df[is.finite(df$n_pca) & is.finite(df$success), ]
  if (!nrow(df)) return(invisible(NULL))
  data.table::setorder(data.table::setDT(df), n_pca)

  raw <- cv[["Cross-Validation Results"]]
  raw_df <- NULL
  if (is.data.frame(raw) && all(c("n.pca", "success") %in% names(raw))) {
    raw_df <- data.frame(
      n_pca = suppressWarnings(as.numeric(raw$n.pca)),
      success = suppressWarnings(as.numeric(raw$success))
    )
    raw_df <- raw_df[is.finite(raw_df$n_pca) & is.finite(raw_df$success), ]
  }

  selected <- suppressWarnings(as.integer(
    cv[["Number of PCs Achieving Highest Mean Success"]]
  ))[1]
  chance <- suppressWarnings(as.numeric(
    cv[["Median and Confidence Interval for Random Chance"]]
  ))
  chance_names <- names(cv[["Median and Confidence Interval for Random Chance"]])
  chance_lo <- chance[chance_names == "2.5%"][1]
  chance_mid <- chance[chance_names == "50%"][1]
  chance_hi <- chance[chance_names == "97.5%"][1]

  colour <- expand_figure_palette(profile, 1L, "colours")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = n_pca, y = success))
  if (!is.null(raw_df) && nrow(raw_df)) {
    # Jittered so the discrete n.pca values a real n.rep = 30 replicates
    # tightly stack on don't render as one solid vertical smear. A fixed
    # seed keeps the figure itself reproducible across repeated report
    # generations against unchanged data -- ggplot2::position_jitter()
    # otherwise draws a fresh random offset every render (confirmed
    # directly: two renders from the identical retained `cv` object
    # produced visibly different point clouds without this).
    jitter_width <- max(diff(sort(unique(df$n_pca))), 1) * 0.12
    p <- p + ggplot2::geom_jitter(
      data = raw_df, colour = colour, alpha = 0.18, size = 1,
      position = ggplot2::position_jitter(width = jitter_width, height = 0, seed = 1L)
    )
  }
  if (length(chance_lo) && is.finite(chance_lo)) {
    p <- p + ggplot2::geom_hline(
      yintercept = chance_lo, linetype = "dashed", colour = "#999999"
    )
  }
  if (length(chance_hi) && is.finite(chance_hi)) {
    p <- p + ggplot2::geom_hline(
      yintercept = chance_hi, linetype = "dashed", colour = "#999999"
    )
  }
  if (length(chance_mid) && is.finite(chance_mid)) {
    p <- p + ggplot2::geom_hline(
      yintercept = chance_mid, linetype = "solid", colour = "#999999"
    )
  }
  p <- p +
    ggplot2::geom_line(colour = colour) +
    ggplot2::geom_point(colour = colour, size = 2)
  if (is.finite(selected)) {
    p <- p + ggplot2::geom_vline(
      xintercept = selected, linetype = "dotted", colour = "#B2182B"
    )
  }
  p <- p +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = sprintf("DAPC PC-count cross-validation (K = %s)", k),
      subtitle = if (is.finite(selected)) {
        sprintf("Selected %d PC(s) by highest mean assignment success", selected)
      } else {
        NULL
      },
      x = "Number of PCA axes retained", y = "Proportion of successful outcome prediction"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(
    p, sprintf("12b_DAPC_xval_K%s", k), dirs,
    cfg$output$figure_formats, 7, 5, cfg$output$dpi
  )
  invisible(p)
}

# The standard base-R DAPC diagnostic (barplot(dapc$eig, ...)) as a proper
# ggplot2 figure: how much between-group variance each retained
# discriminant axis explains. A steep drop after the first one or two axes
# is the usual signal that later axes (and the LD scatterplot panels built
# from them) carry little real separating power -- a second, independent
# way to sanity-check the retained axis count alongside plot_dapc_xval()'s
# PC-count curve above (that one validates n.pca going into dapc(), this
# one validates n.da coming out of it). Rendered once, for the highest K
# only (plot_dapc() below) -- the same shape of diagnostic repeated once per
# K added little beyond report length, since every K's eig vector already
# has exactly n.da = min(K - 1, 10) entries (adegenet::dapc() only ever
# returns the retained discriminant eigenvalues, not a larger discarded
# pool), so there is nothing K-specific to compare across panels.
plot_dapc_eigenvalues <- function(model, k, n_pca, cfg, dirs, profile) {
  eig <- model$eig
  if (is.null(eig) || !length(eig)) return(invisible(NULL))
  n_da <- length(eig)
  df <- data.frame(axis = factor(seq_along(eig)), eigenvalue = as.numeric(eig))
  df$contribution_pct <- 100 * df$eigenvalue / sum(df$eigenvalue)
  # Every bar shown is already retained (see comment above) -- the
  # right-most one is highlighted as a "this many, total" boundary marker,
  # the same role plot_dapc_xval()'s dotted vline plays for its own
  # selected-PC-count callout, not a claim that the other bars were not.
  df$retained_marker <- as.integer(df$axis) == n_da
  base_colour <- unname(expand_figure_palette(profile, 1L, "fills"))
  highlight_colour <- "#B2182B"
  p <- ggplot2::ggplot(df, ggplot2::aes(x = axis, y = eigenvalue, fill = retained_marker)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", contribution_pct)),
      vjust = -0.35, size = 3
    ) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = base_colour, `TRUE` = highlight_colour)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = sprintf("DA eigenvalues (K = %s)", k),
      subtitle = wrap_plot_subtitle(sprintf(
        "%d discriminant axis/axes retained (right-most bar highlighted); %s",
        n_da,
        if (length(n_pca) && is.finite(n_pca)) {
          sprintf("%d PCA axis/axes retained for this model.", n_pca)
        } else {
          "PCA axis/axes retained not available."
        }
      )),
      x = "Discriminant axis", y = "Eigenvalue"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(
    p, sprintf("12c_DAPC_eigenvalues_K%s", k), dirs,
    cfg$output$figure_formats, 7, 5, cfg$output$dpi
  )
  invisible(p)
}

plot_dapc <- function(dapc, cfg, dirs) {
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)
  for (k in names(dapc$models)) {
    d <- dapc$models[[k]]$coordinates
    annotation <- dapc_reproducibility_annotation(dapc, k, cfg)
    axes <- grep("^LD", names(d), value = TRUE)
    if (length(axes) >= 2L) {
      clusters <- sort(unique(as.character(d$cluster)))
      cluster_shapes <- stats::setNames(
        rep(profile$shapes, length.out = length(clusters)), clusters
      )
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[axes[1]]], y = .data[[axes[2]]], colour = population, shape = cluster)) +
        ggplot2::geom_hline(
          yintercept = 0, colour = "#D9D9D9", linewidth = 0.35
        ) +
        ggplot2::geom_vline(
          xintercept = 0, colour = "#D9D9D9", linewidth = 0.35
        ) +
        ggplot2::geom_point(size = 3, alpha = .9, stroke = 0.55) +
        ggplot2::scale_colour_manual(
          values = population_palette(d$population, style)
        ) +
        ggplot2::scale_shape_manual(values = cluster_shapes) +
        ggplot2::labs(
          title = sprintf("Discriminant analysis of principal components (K = %s)", k),
          subtitle = wrap_plot_subtitle(annotation$text), x = axes[1], y = axes[2],
          colour = "Population", shape = "DAPC cluster"
        ) + theme_publication(figure_base_size(cfg))
      if (isTRUE(annotation$unstable)) {
        p <- p + ggplot2::theme(
          plot.subtitle = ggplot2::element_text(colour = "#B2182B", face = "bold")
        )
      }
      save_plot(p, sprintf("11_DAPC_K%s", k), dirs, cfg$output$figure_formats, 8, 6, cfg$output$dpi)
    }
    plot_dapc_xval(dapc$models[[k]]$cv, k, cfg, dirs, profile)
    membership <- dapc$models[[k]]$membership
    q <- data.table::as.data.table(membership)
    q[, sample := rownames(membership)]
    q[, population := d$population[match(sample, d$sample)]]
    data.table::setcolorder(q, c("sample", "population", grep("^cluster_", names(q), value = TRUE)))
    plot_q_matrix_views(
      q, as.integer(k), cfg, dirs, prefix = "DAPC_membership",
      title = sprintf(
        "Discriminant analysis of principal components membership probabilities (K = %s)",
        k
      ),
      subtitle = wrap_plot_subtitle(annotation$text),
      subtitle_is_warning = annotation$unstable,
      y_label = "Posterior membership probability"
    )
    loadings <- dapc$models[[k]]$loadings
    if (!is.null(loadings) && nrow(loadings)) {
      plot_dapc_loading_manhattan(loadings, k, annotation$n_pca, cfg, dirs, profile)
      plot_dapc_loading_ranked(loadings, k, annotation$n_pca, cfg, dirs, profile)
    }
  }
  # plot_dapc_eigenvalues() only for the highest K: every K's eig vector
  # already has exactly n.da entries (see that function's own comment), so
  # there is nothing K-specific to compare across a full set of per-K
  # panels -- the highest K alone (the most discriminant axes fit) already
  # summarizes the diagnostic.
  if (length(dapc$models)) {
    max_k <- as.character(max(as.integer(names(dapc$models))))
    max_k_annotation <- dapc_reproducibility_annotation(dapc, max_k, cfg)
    plot_dapc_eigenvalues(
      dapc$models[[max_k]]$model, max_k, max_k_annotation$n_pca, cfg, dirs, profile
    )
  }
  plot_structure_k_selection(
    dapc$k_selection, cfg, dirs,
    stem = "12_DAPC_cluster_number_selection",
    title = paste("Discriminant analysis of principal components",
                  "cluster-number selection")
  )
}
