parse_admixture_cv <- function(text) {
  hit <- regmatches(text, regexpr("CV error \\(K=[0-9]+\\):[[:space:]]*[0-9.eE+-]+", text))
  if (!length(hit) || !nzchar(hit)) return(NULL)
  k <- as.integer(sub(".*K=([0-9]+).*", "\\1", hit))
  value <- as.numeric(sub(".*:[[:space:]]*", "", hit))
  data.table::data.table(K = k, cv_error = value)
}

#' Run ADMIXTURE cross-validation across K values
#'
#' @param executable ADMIXTURE executable name or path.
#' @param plink_prefix Prefix of the PLINK BED dataset.
#' @param k_values Integer ancestry-cluster values to evaluate.
#' @param threads Number of ADMIXTURE worker threads.
#' @param cv_folds Number of cross-validation folds.
#' @param output_dir Directory for ADMIXTURE logs and outputs.
#' @param seed Deterministic ADMIXTURE seed.
#' @return A data table of K values and cross-validation errors.
#' @export
run_admixture_cv <- function(executable, plink_prefix, k_values, threads = 1L, cv_folds = 5L,
                             output_dir = ".", seed = 42L) {
  bed <- paste0(plink_prefix, ".bed")
  if (!file.exists(bed)) stopf("ADMIXTURE requires PLINK BED files; missing %s", bed)
  exe <- Sys.which(executable)
  if (!nzchar(exe)) stopf("ADMIXTURE executable not found: %s", executable)
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(output_dir)
  results <- list()
  for (k in k_values) {
    log_file <- file.path(output_dir, sprintf("admixture_K%d.log", k))
    args <- c(sprintf("--cv=%d", cv_folds), sprintf("-j%d", max(1L, as.integer(threads))), normalizePath(bed), as.character(k))
    out <- system2(exe, args, stdout = TRUE, stderr = TRUE, env = sprintf("ADMIXTURE_SEED=%d", seed))
    writeLines(out, log_file)
    parsed <- parse_admixture_cv(paste(out, collapse = "\n"))
    if (!is.null(parsed)) results[[as.character(k)]] <- parsed
  }
  data.table::rbindlist(results, fill = TRUE)[order(K)]
}

read_admixture_q <- function(path, sample_file, metadata) {
  if (!file.exists(path)) stopf("Q matrix not found: %s", path)
  if (is.null(sample_file) || !file.exists(sample_file)) {
    stop("An explicit Q sample-order file is required", call. = FALSE)
  }
  required_metadata <- c("sample", "population")
  if (!all(required_metadata %in% names(metadata))) {
    stop("ADMIXTURE metadata requires sample and population columns", call. = FALSE)
  }

  ids <- data.table::fread(sample_file, header = FALSE)[[1L]] |> as.character()
  q <- data.table::fread(path, header = FALSE)
  if (nrow(q) != length(ids)) stop("Q rows do not match sample-order file", call. = FALSE)
  q[] <- lapply(q, as.numeric)
  if (any(!is.finite(as.matrix(q)))) stop("Q matrix contains nonnumeric values", call. = FALSE)
  rs <- rowSums(q)
  if (any(rs <= 0)) stop("Q matrix contains zero-sum rows", call. = FALSE)
  q <- q / rs
  data.table::setnames(q, paste0("cluster_", seq_len(ncol(q))))

  metadata_samples <- as.character(metadata[["sample"]])
  metadata_populations <- as.character(metadata[["population"]])
  if (anyDuplicated(metadata_samples)) {
    stop("ADMIXTURE metadata contains duplicate sample identifiers", call. = FALSE)
  }
  q[["sample"]] <- ids
  q[["population"]] <- metadata_populations[match(ids, metadata_samples)]
  if (anyNA(q[["population"]])) {
    stop("Some ADMIXTURE samples are absent from metadata", call. = FALSE)
  }
  data.table::setcolorder(q, c("sample", "population", grep("^cluster_", names(q), value = TRUE)))
  q
}

plot_admixture_cv <- function(cv, cfg, dirs) {
  if (!nrow(cv)) return(invisible(NULL))
  p <- ggplot2::ggplot(cv, ggplot2::aes(K, cv_error)) + ggplot2::geom_line() + ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(breaks = cv$K) +
    ggplot2::labs(
      title = "Admixture-model cross-validation", y = "Cross-validation error"
    ) + theme_publication()
  save_plot(p, "13_ADMIXTURE_CV", dirs, cfg$output$figure_formats, 7, 5, cfg$output$dpi)
}

plot_q_matrix <- function(q, k, cfg, dirs, prefix = "ADMIXTURE_Q",
                          title = NULL, subtitle = NULL,
                          subtitle_is_warning = FALSE, sample_labels = NULL,
                          order_mode = c("population", "data_driven"),
                          y_label = "Ancestry proportion") {
  order_mode <- match.arg(order_mode)
  clusters <- grep("^cluster_", names(q), value = TRUE)
  x <- data.table::copy(q)
  if (is.null(sample_labels)) sample_labels <- x$sample
  sample_labels <- as.character(sample_labels)
  if (length(sample_labels) != nrow(x) || anyNA(sample_labels) ||
      any(!nzchar(sample_labels)) || anyDuplicated(sample_labels)) {
    stop("Membership plot sample labels must be unique and non-empty", call. = FALSE)
  }
  data.table::set(x, j = "sample_label", value = sample_labels)
  membership <- as.matrix(x[, ..clusters])
  x[, dominant := max.col(membership, ties.method = "first")]
  x[, dominant_membership := apply(membership, 1L, max)]
  if (identical(order_mode, "population")) {
    if (!"population" %in% names(x)) {
      stop("Population-organized membership plots require a population column", call. = FALSE)
    }
    data.table::setorderv(
      x, c("population", "dominant", clusters, "sample_label"),
      c(1, 1, rep(-1, length(clusters)), 1)
    )
  } else {
    data.table::setorderv(
      x, c("dominant", "dominant_membership", clusters, "sample_label"),
      c(1, -1, rep(-1, length(clusters)), 1)
    )
  }
  x[, order := seq_len(.N)]
  cluster_boundaries <- numeric()
  if (identical(order_mode, "data_driven")) {
    cluster_ends <- x[, .(end = max(order)), by = dominant][order(dominant), end]
    if (length(cluster_ends) > 1L) {
      cluster_boundaries <- utils::head(cluster_ends, -1L) + 0.5
    }
  }
  plot_width <- min(48, max(10, nrow(x) * 0.08))
  points_per_sample <- plot_width * 72 / nrow(x)
  axis_label_size <- max(1, min(7, points_per_sample * 0.8))
  id_vars <- c("sample", "sample_label", "order")
  if ("population" %in% names(x)) id_vars <- c(id_vars, "population")
  long <- data.table::melt(x, id.vars = id_vars, measure.vars = clusters,
                           variable.name = "cluster", value.name = "ancestry")
  default_title <- sprintf(
    "%s (K = %d)",
    switch(
      prefix,
      fastStructure_Q = "fastStructure ancestry proportions",
      sNMF_Q = "Sparse non-negative matrix factorization ancestry coefficients",
      STRUCTURE_Q = "STRUCTURE membership proportions",
      DAPC_membership = "Discriminant analysis of principal components membership probabilities",
      "Admixture-model ancestry proportions"
    ),
    k
  )
  plot_title <- title %||% default_title
  if (identical(order_mode, "data_driven")) {
    plot_title <- paste0(plot_title, " - data-driven cluster order")
  }
  p <- ggplot2::ggplot(long, ggplot2::aes(order, ancestry, fill = cluster)) + ggplot2::geom_col(width = 1) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    ggplot2::scale_x_continuous(breaks = x$order, labels = x$sample_label, expand = c(0,0)) +
    ggplot2::labs(
      title = plot_title,
      subtitle = subtitle, x = NULL, y = y_label
    ) +
    theme_publication() + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = axis_label_size),
      axis.ticks.x = ggplot2::element_line(colour = "grey40")
    )
  if (length(cluster_boundaries)) {
    p <- p + ggplot2::geom_vline(
      xintercept = cluster_boundaries, colour = "grey20",
      linewidth = 0.45
    )
  }
  if (identical(order_mode, "population")) {
    p <- p + ggplot2::facet_grid(~population, scales = "free_x", space = "free_x")
  }
  if (isTRUE(subtitle_is_warning)) {
    p <- p + ggplot2::theme(
      plot.subtitle = ggplot2::element_text(colour = "#B2182B", face = "bold")
    )
  }
  label_height <- min(8, max(nchar(sample_labels)) * 0.08)
  stem <- if (identical(order_mode, "data_driven")) {
    sprintf("14_%s_data_driven_K%d", prefix, k)
  } else {
    sprintf("14_%s_K%d", prefix, k)
  }
  save_plot(p, stem, dirs, cfg$output$figure_formats,
            plot_width, 6 + label_height, cfg$output$dpi)
}

plot_q_matrix_views <- function(q, k, cfg, dirs, prefix = "ADMIXTURE_Q",
                                title = NULL, subtitle = NULL,
                                subtitle_is_warning = FALSE,
                                sample_labels = NULL,
                                y_label = "Ancestry proportion") {
  common <- list(
    q = q, k = k, cfg = cfg, dirs = dirs, prefix = prefix,
    title = title, subtitle = subtitle,
    subtitle_is_warning = subtitle_is_warning,
    sample_labels = sample_labels, y_label = y_label
  )
  do.call(plot_q_matrix, c(common, list(order_mode = "population")))
  do.call(plot_q_matrix, c(common, list(order_mode = "data_driven")))
  invisible(NULL)
}
