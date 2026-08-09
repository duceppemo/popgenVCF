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

dapc_worker_count <- function(k_values, threads,
                              fork_available = .Platform$OS.type != "windows") {
  threads <- suppressWarnings(as.integer(threads)[1L])
  if (is.na(threads) || threads < 1L || !isTRUE(fork_available)) return(1L)
  max(1L, min(threads, length(k_values)))
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
  failed <- vapply(results, inherits, logical(1L), what = "try-error")
  if (any(failed)) {
    stop(
      "Parallel DAPC task(s) failed for K = ",
      paste(k_values[failed], collapse = ", "),
      call. = FALSE
    )
  }
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
  workers <- dapc_worker_count(valid_k, threads)
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
  threshold <- cfg$analyses$structure$reproducibility_rmse %||% 0.05
  diagnostics <- data.table::as.data.table(dapc$diagnostics)
  row <- diagnostics[as.character(K) == as.character(k)]
  rmse <- if (nrow(row) && "replicate_max_rmse" %in% names(row)) {
    suppressWarnings(as.numeric(row$replicate_max_rmse[[1L]]))
  } else {
    NA_real_
  }
  estimated <- !is.null(dapc$models[[as.character(k)]]$reproducibility)

  if (!estimated || !length(rmse) || !is.finite(rmse)) {
    return(list(
      text = sprintf(
        "Replicate membership RMSE not estimated (stability threshold = %.4g).",
        threshold
      ),
      unstable = FALSE
    ))
  }
  if (rmse > threshold) {
    return(list(
      text = sprintf(
        paste0(
          "WARNING: DAPC replicate membership is unstable ",
          "(RMSE = %.4g > %.4g).\nAvoid interpreting these assignments."
        ),
        rmse, threshold
      ),
      unstable = TRUE
    ))
  }
  list(
    text = sprintf(
      "Replicate membership RMSE = %.4g (stability threshold = %.4g).",
      rmse, threshold
    ),
    unstable = FALSE
  )
}

plot_dapc_loading_manhattan <- function(loadings, k, cfg, dirs, profile) {
  layout <- manhattan_layout(loadings$chromosome, loadings$position)
  bp_breaks <- manhattan_bp_breaks(loadings$chromosome, loadings$position, layout$offset)
  loadings <- data.table::copy(loadings)
  loadings[, x := layout$x]
  loadings[, chrom_group := factor(match(chromosome, layout$ticks$chromosome) %% 2L)]
  loadings[, axis := factor(axis, levels = natural_sort_levels(axis))]
  colours <- expand_figure_palette(profile, 2L, "colours")
  p <- ggplot2::ggplot(loadings, ggplot2::aes(x = x, y = contribution, colour = chrom_group)) +
    ggplot2::geom_point(size = 1, alpha = .75, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
    ggplot2::facet_wrap(~axis, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = sprintf("Discriminant analysis SNP loadings (K = %s)", k),
      x = "Chromosome position", y = "Contribution to discriminant function"
    ) + theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1, "lines"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
  n_axes <- data.table::uniqueN(loadings$axis)
  save_plot(
    p, sprintf("15_DAPC_loadings_manhattan_K%s", k), dirs,
    cfg$output$figure_formats, 10, max(4, 2.2 * n_axes), cfg$output$dpi
  )
  invisible(p)
}

plot_dapc_loading_ranked <- function(loadings, k, cfg, dirs, profile) {
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
          subtitle = annotation$text, x = axes[1], y = axes[2],
          colour = "Population", shape = "DAPC cluster"
        ) + theme_publication(figure_base_size(cfg))
      if (isTRUE(annotation$unstable)) {
        p <- p + ggplot2::theme(
          plot.subtitle = ggplot2::element_text(colour = "#B2182B", face = "bold")
        )
      }
      save_plot(p, sprintf("11_DAPC_K%s", k), dirs, cfg$output$figure_formats, 8, 6, cfg$output$dpi)
    }
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
      subtitle = annotation$text,
      subtitle_is_warning = annotation$unstable,
      y_label = "Posterior membership probability"
    )
    loadings <- dapc$models[[k]]$loadings
    if (!is.null(loadings) && nrow(loadings)) {
      plot_dapc_loading_manhattan(loadings, k, cfg, dirs, profile)
      plot_dapc_loading_ranked(loadings, k, cfg, dirs, profile)
    }
  }
  plot_structure_k_selection(
    dapc$k_selection, cfg, dirs,
    stem = "12_DAPC_cluster_number_selection",
    title = paste("Discriminant analysis of principal components",
                  "cluster-number selection")
  )
}
