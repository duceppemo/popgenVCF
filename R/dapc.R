genlight_from_gds <- function(geno, sample_ids, metadata) {
  gl <- adegenet::as.genlight(geno)
  adegenet::indNames(gl) <- public_sample_ids(metadata, sample_ids)
  adegenet::pop(gl) <- factor(metadata[match(sample_ids, sample), population])
  gl
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

run_dapc_analysis <- function(geno, sample_ids, metadata, k_values, seed,
                              cross_validate = TRUE, replicate_seeds = seed) {
  public_ids <- public_sample_ids(metadata, sample_ids)
  gl <- genlight_from_gds(geno, sample_ids, metadata)
  max_pca <- max(2L, min(nrow(geno) - 1L, 100L))
  diagnostics <- list(); models <- list(); replicate_membership <- list()
  truth <- metadata$population[match(sample_ids, metadata$sample)]
  for (k in k_values[k_values >= 2L & k_values < length(sample_ids)]) {
    reps <- list(); primary <- NULL
    for (rep_seed in unique(as.integer(replicate_seeds))) {
      set.seed(rep_seed + k)
      cluster_fit <- adegenet::find.clusters(gl, n.pca = max_pca, n.clust = k,
                                            choose.n.clust = FALSE)
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
          selected <- suppressWarnings(as.integer(cv$`Number of PCs Achieving Highest Mean Success`))
          if (length(selected) && is.finite(selected)) n_pca <- selected
        }
      }
      model <- adegenet::dapc(gl, pop = grp, n.pca = n_pca, n.da = n_da)
      membership <- extract_dapc_membership(model, public_ids)
      reps[[as.character(rep_seed)]] <- membership
      if (is.null(primary)) primary <- list(model = model, groups = grp, cv = cv,
                                            n_pca = n_pca, n_da = n_da,
                                            membership = membership,
                                            bic = cluster_fit$Kstat %||% NA_real_)
    }
    model <- primary$model; grp <- primary$groups
    coord <- data.table::as.data.table(model$ind.coord, keep.rownames = "sample")
    coord[, vcf_sample := sample_ids[match(sample, public_ids)]]
    data.table::set(coord, j = "population", value = metadata$population[match(coord$vcf_sample, metadata$sample)])
    data.table::set(coord, j = "cluster", value = as.character(grp))
    reproducibility <- if (length(reps) > 1L) structure_reproducibility(reps) else NULL
    assignment_accuracy <- classification_accuracy_permutation(grp, truth)
    models[[as.character(k)]] <- list(
      model = model, coordinates = coord, groups = grp, cv = primary$cv,
      membership = primary$membership, replicate_membership = reps,
      reproducibility = reproducibility
    )
    diagnostics[[as.character(k)]] <- data.table::data.table(
      K = k, n_pca = primary$n_pca, n_da = primary$n_da,
      BIC = if (length(primary$bic)) as.numeric(primary$bic)[1] else NA_real_,
      assignment_accuracy = assignment_accuracy,
      replicate_max_rmse = if (is.null(reproducibility)) 0 else max(reproducibility$metrics$rmse)
    )
    replicate_membership[[as.character(k)]] <- reps
  }
  diag <- data.table::rbindlist(diagnostics, fill = TRUE)
  list(models = models, diagnostics = diag,
       k_selection = if (nrow(diag)) select_structure_k(diag) else NULL,
       replicate_membership = replicate_membership)
}

plot_dapc <- function(dapc, cfg, dirs) {
  for (k in names(dapc$models)) {
    d <- dapc$models[[k]]$coordinates
    axes <- grep("^LD", names(d), value = TRUE)
    if (length(axes) >= 2L) {
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[axes[1]]], y = .data[[axes[2]]], colour = population, shape = cluster)) +
        ggplot2::geom_point(size = 2.8, alpha = .85) + ggplot2::scale_colour_manual(values = population_palette(d$population)) +
        ggplot2::labs(title = sprintf("Discriminant analysis of principal components (K = %s)", k), x = axes[1], y = axes[2]) + theme_publication()
      save_plot(p, sprintf("11_DAPC_K%s", k), dirs, cfg$output$figure_formats, 8, 6, cfg$output$dpi)
    }
    membership <- dapc$models[[k]]$membership
    q <- data.table::as.data.table(membership)
    q[, sample := rownames(membership)]
    q[, population := d$population[match(sample, d$sample)]]
    data.table::setcolorder(q, c("sample", "population", grep("^cluster_", names(q), value = TRUE)))
    plot_q_matrix(q, as.integer(k), cfg, dirs, prefix = "DAPC_membership")
  }
}
