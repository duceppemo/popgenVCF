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
    ggplot2::scale_x_continuous(breaks = cv$K) + ggplot2::labs(title = "ADMIXTURE cross-validation", y = "Cross-validation error") + theme_publication()
  save_plot(p, "13_ADMIXTURE_CV", dirs, cfg$output$figure_formats, 7, 5, cfg$output$dpi)
}

plot_q_matrix <- function(q, k, cfg, dirs, prefix = "ADMIXTURE_Q") {
  clusters <- grep("^cluster_", names(q), value = TRUE)
  x <- data.table::copy(q)
  x[, dominant := max.col(as.matrix(.SD), ties.method = "first"), .SDcols = clusters]
  data.table::setorderv(x, c("population", "dominant", clusters), c(1, 1, rep(-1, length(clusters))))
  x[, order := seq_len(.N)]
  long <- data.table::melt(x, id.vars = c("sample", "population", "order"), measure.vars = clusters,
                           variable.name = "cluster", value.name = "ancestry")
  p <- ggplot2::ggplot(long, ggplot2::aes(order, ancestry, fill = cluster)) + ggplot2::geom_col(width = 1) +
    ggplot2::facet_grid(~population, scales = "free_x", space = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0,0)) + ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::labs(title = sprintf("ADMIXTURE ancestry proportions (K = %d)", k), x = NULL, y = "Ancestry proportion") +
    theme_publication() + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  save_plot(p, sprintf("14_%s_K%d", prefix, k), dirs, cfg$output$figure_formats, max(10, nrow(q) * .08), 6, cfg$output$dpi)
}
