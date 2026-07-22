module_result <- function(analysis, context) list(analysis = analysis, context = context)

run_module_diversity <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  div <- compute_diversity(context$gds, context$sample_ids, context$qc_snps,
                           context$metadata, context$ids)
  ci <- if (isTRUE(cfg$analyses$bootstrap$enabled)) {
    bootstrap_diversity(div$locus, cfg$analyses$bootstrap$replicates,
                        cfg$compute$seed, cfg$analyses$bootstrap$unit)
  } else data.table::data.table()
  context$diversity_full <- div
  stored <- div; stored$genotype <- NULL
  analysis <- set_analysis_result(analysis, "diversity", stored)
  analysis <- set_analysis_result(analysis, "diversity_ci", ci)
  write_tsv(div$sample, file.path(dirs$tables, "08_sample_diversity.tsv"))
  write_tsv(div$population, file.path(dirs$tables, "09_population_diversity.tsv"))
  write_tsv(div$locus, file.path(dirs$tables, "10_population_locus_diversity.tsv"))
  if (nrow(ci)) write_tsv(ci, file.path(dirs$tables, "11_diversity_bootstrap_CI.tsv"))
  plot_diversity(div, ci, cfg, dirs)
  module_result(analysis, context)
}

run_module_pca <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  pca <- run_pca(context$gds, context$sample_ids, context$final_snps,
                 context$metadata, cfg$analyses$n_pcs, cfg$compute$threads)
  context$pca <- pca
  analysis <- set_analysis_result(analysis, "pca", pca[c("scores", "variance")])
  write_tsv(pca$scores, file.path(dirs$tables, "12_PCA_scores.tsv"))
  write_tsv(pca$variance, file.path(dirs$tables, "13_PCA_variance.tsv"))
  plot_pca(pca, cfg, dirs)
  module_result(analysis, context)
}

run_module_ibs <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  ibs <- run_ibs(context$gds, context$sample_ids, context$final_snps,
                 context$metadata, cfg$compute$threads)
  context$ibs <- ibs
  analysis <- set_analysis_result(analysis, "ibs", list(
    mds = ibs$mds, eigenvalues = ibs$eig,
    similarity_file = file.path(dirs$tables, "14_IBS_similarity.tsv"),
    distance_file = file.path(dirs$tables, "15_IBS_distance.tsv")
  ))
  write_matrix_tsv(ibs$similarity, file.path(dirs$tables, "14_IBS_similarity.tsv"), "sample")
  write_matrix_tsv(ibs$distance, file.path(dirs$tables, "15_IBS_distance.tsv"), "sample")
  write_tsv(ibs$mds, file.path(dirs$tables, "16_IBS_MDS.tsv"))
  plot_ibs(ibs, cfg, dirs)
  module_result(analysis, context)
}

run_module_tree <- function(analysis, context) {
  tree <- build_nj_tree(context$ibs, context$metadata, context$cfg, context$dirs)
  analysis <- set_analysis_result(analysis, "tree", tree)
  module_result(analysis, context)
}

run_module_fst <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  fst <- run_fst(context$gds, context$qc_snps, context$metadata)
  fst_ci <- if (isTRUE(cfg$analyses$bootstrap$enabled)) {
    bootstrap_fst(context$gds, context$qc_snps, context$ids, context$metadata,
                  cfg$analyses$bootstrap$replicates, cfg$compute$seed)
  } else data.table::data.table()
  analysis <- set_analysis_result(analysis, "fst", fst)
  analysis <- set_analysis_result(analysis, "fst_ci", fst_ci)
  write_tsv(data.table::data.table(global_fst = fst$global), file.path(dirs$tables, "17_global_FST.tsv"))
  write_tsv(fst$long, file.path(dirs$tables, "18_pairwise_FST.tsv"))
  write_matrix_tsv(fst$matrix, file.path(dirs$tables, "19_pairwise_FST_matrix.tsv"), "population")
  if (nrow(fst_ci)) write_tsv(fst_ci, file.path(dirs$tables, "20_pairwise_FST_bootstrap_CI.tsv"))
  plot_fst(fst, cfg, dirs)
  module_result(analysis, context)
}

run_module_dapc <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; div <- context$diversity_full
  structure_cfg <- cfg$analyses$structure
  seeds <- structure_cfg$seeds
  if (is.null(seeds)) seeds <- cfg$compute$seed + seq_len(structure_cfg$replicates) - 1L
  dapc <- run_dapc_analysis(div$genotype, context$sample_ids, context$metadata,
                            parse_int_range(cfg$analyses$dapc_k), cfg$compute$seed,
                            cfg$analyses$dapc_cross_validation,
                            replicate_seeds = seeds)
  analysis <- set_analysis_result(analysis, "dapc", dapc)
  write_tsv(dapc$diagnostics, file.path(dirs$tables, "21_DAPC_diagnostics.tsv"))
  for (k in names(dapc$models)) {
    write_tsv(dapc$models[[k]]$coordinates,
              file.path(dirs$tables, sprintf("22_DAPC_coordinates_K%s.tsv", k)))
    membership <- data.table::as.data.table(dapc$models[[k]]$membership)
    membership[, sample := rownames(dapc$models[[k]]$membership)]
    data.table::setcolorder(membership, c("sample", grep("^cluster_", names(membership), value = TRUE)))
    write_tsv(membership, file.path(dirs$tables, sprintf("22b_DAPC_membership_K%s.tsv", k)))
    if (!is.null(dapc$models[[k]]$reproducibility)) {
      write_tsv(dapc$models[[k]]$reproducibility$metrics,
                file.path(dirs$tables, sprintf("22c_DAPC_reproducibility_K%s.tsv", k)))
    }
  }
  if (!is.null(dapc$k_selection)) {
    write_tsv(dapc$k_selection$best_by_method, file.path(dirs$tables, "22d_DAPC_K_selection.tsv"))
  }
  plot_dapc(dapc, cfg, dirs)
  module_result(analysis, context)
}

run_module_amova <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; div <- context$diversity_full
  amova <- run_amova_analysis(div$genotype, context$sample_ids, context$metadata,
                              999L, cfg$compute$seed)
  analysis <- set_analysis_result(analysis, "amova", amova)
  write_tsv(amova$components, file.path(dirs$tables, "23_AMOVA_components.tsv"))
  write_tsv(amova$phi, file.path(dirs$tables, "24_AMOVA_phi_statistics.tsv"))
  module_result(analysis, context)
}

run_module_ibd <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  ibd <- run_mantel_ibd(context$ibs$distance, context$metadata,
                        cfg$input$geographic_columns, 999L, cfg$compute$seed)
  analysis <- set_analysis_result(analysis, "ibd", ibd)
  if (!is.null(ibd)) {
    write_tsv(ibd$summary, file.path(dirs$tables, "25_Mantel_IBD_summary.tsv"))
    write_tsv(ibd$pairs, file.path(dirs$tables, "26_IBD_pairs.tsv"))
    plot_ibd(ibd, cfg, dirs)
  } else {
    log_msg("Skipping Mantel/IBD because valid latitude/longitude columns were unavailable", level = "WARNING")
  }
  module_result(analysis, context)
}

run_module_admixture <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; ac <- cfg$analyses$admixture
  cv <- run_admixture_cv(ac$executable, ac$plink_prefix, parse_int_range(ac$k),
                         ac$threads, ac$cv_folds, dirs$admixture, cfg$compute$seed)
  analysis <- set_analysis_result(analysis, "admixture_cv", cv)
  write_tsv(cv, file.path(dirs$tables, "27_ADMIXTURE_CV.tsv"))
  plot_admixture_cv(cv, cfg, dirs)
  for (k in cv$K) {
    qpath <- file.path(dirs$admixture, sprintf("%s.%d.Q", basename(ac$plink_prefix), k))
    if (file.exists(qpath)) {
      q <- read_admixture_q(qpath, ac$q_sample_file, context$metadata)
      write_tsv(q, file.path(dirs$tables, sprintf("28_ADMIXTURE_Q_K%d.tsv", k)))
      plot_q_matrix(q, k, cfg, dirs)
    }
  }
  module_result(analysis, context)
}

run_module_faststructure <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; fc <- cfg$analyses$faststructure
  result <- run_faststructure(fc$structure_executable, fc$choosek_executable,
                              fc$plink_prefix, parse_int_range(fc$k),
                              dirs$structure, cfg$compute$seed)
  for (k in names(result$q)) {
    q <- result$q[[k]]
    ids <- data.table::fread(fc$q_sample_file, header = FALSE)[[1]] |> as.character()
    if (nrow(q) != length(ids)) stop("fastStructure Q rows do not match sample-order file", call. = FALSE)
    qdt <- data.table::as.data.table(q); qdt[, sample := ids]
    qdt[, population := context$metadata$population[match(sample, context$metadata$sample)]]
    data.table::setcolorder(qdt, c("sample", "population", grep("^cluster_", names(qdt), value = TRUE)))
    result$q[[k]] <- qdt
    write_tsv(qdt, file.path(dirs$tables, sprintf("29_fastStructure_Q_K%s.tsv", k)))
    plot_q_matrix(qdt, as.integer(k), cfg, dirs, prefix = "fastStructure_Q")
  }
  write_tsv(result$runs, file.path(dirs$tables, "29_fastStructure_runs.tsv"))
  analysis <- set_analysis_result(analysis, "faststructure", result)
  module_result(analysis, context)
}

run_module_snmf <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; sc <- cfg$analyses$snmf
  result <- run_snmf(sc$geno_file, parse_int_range(sc$k), sc$repetitions,
                     sc$entropy, cfg$compute$seed)
  write_tsv(result$diagnostics, file.path(dirs$tables, "30_sNMF_cross_entropy.tsv"))
  sample_order <- data.table::fread(sc$q_sample_file, header = FALSE)[[1]] |> as.character()
  for (k in names(result$q)) {
    q <- result$q[[k]]
    if (nrow(q) != length(sample_order)) stop("sNMF Q rows do not match sample-order file", call. = FALSE)
    qdt <- data.table::as.data.table(q); qdt[, sample := sample_order]
    qdt[, population := context$metadata$population[match(sample, context$metadata$sample)]]
    if (anyNA(qdt$population)) stop("Some sNMF samples are absent from retained metadata", call. = FALSE)
    data.table::setcolorder(qdt, c("sample", "population", grep("^cluster_", names(qdt), value = TRUE)))
    result$q[[k]] <- qdt
    write_tsv(qdt, file.path(dirs$tables, sprintf("30_sNMF_Q_K%s.tsv", k)))
    plot_q_matrix(qdt, as.integer(k), cfg, dirs, prefix = "sNMF_Q")
  }
  analysis <- set_analysis_result(analysis, "snmf", result)
  module_result(analysis, context)
}

run_module_chromosome <- function(analysis, context) {
  chromosome <- run_chromosome_analyses(
    context$gds, context$qc_snps, context$final_snps, context$ids,
    context$sample_ids, context$metadata, context$cfg
  )
  summary <- write_chromosome_results(chromosome, context$dirs)
  analysis <- set_analysis_result(analysis, "chromosome_summary", summary)
  module_result(analysis, context)
}

#' Construct the built-in analysis registry
#' @return A populated `PopgenVCFRegistry`.
#' @noRd
default_analysis_registry <- function() {
  r <- new_analysis_registry()
  r <- register_analysis(r, "diversity", run_module_diversity,
                         description = "Sample, population, and locus diversity",
                         validate = validate_diversity_result,
                         outputs = c("diversity", "diversity_ci"),
                         references = "Nei 1987", resource_class = "heavy")
  r <- register_analysis(r, "pca", run_module_pca,
                         description = "Principal component analysis",
                         validate = validate_pca_result,
                         references = "Patterson et al. 2006", resource_class = "heavy")
  r <- register_analysis(r, "ibs", run_module_ibs,
                         description = "IBS matrices and multidimensional scaling",
                         validate = validate_ibs_result,
                         references = "Zheng et al. 2012", resource_class = "heavy")
  r <- register_analysis(r, "tree", run_module_tree, requires = "ibs",
                         description = "Neighbour-joining tree from IBS distance",
                         validate = validate_tree_result,
                         outputs = "tree", references = "Saitou and Nei 1987")
  r <- register_analysis(r, "fst", run_module_fst,
                         description = "Global and pairwise Weir-Cockerham FST",
                         validate = validate_fst_result, outputs = c("fst", "fst_ci"),
                         references = "Weir and Cockerham 1984", resource_class = "heavy")
  r <- register_analysis(r, "dapc", run_module_dapc, requires = "diversity",
                         enabled = function(cfg) isTRUE(cfg$analyses$dapc),
                         description = "Discriminant analysis of principal components",
                         validate = validate_dapc_result,
                         references = "Jombart et al. 2010", resource_class = "heavy")
  r <- register_analysis(r, "amova", run_module_amova, requires = "diversity",
                         enabled = function(cfg) isTRUE(cfg$analyses$amova),
                         description = "Analysis of molecular variance",
                         validate = validate_amova_result,
                         references = "Excoffier et al. 1992", resource_class = "heavy")
  r <- register_analysis(r, "ibd", run_module_ibd, requires = "ibs",
                         enabled = function(cfg) isTRUE(cfg$analyses$mantel) || isTRUE(cfg$analyses$isolation_by_distance),
                         description = "Mantel test and isolation by distance",
                         validate = validate_ibd_result,
                         references = c("Mantel 1967", "Rousset 1997"))
  r <- register_analysis(r, "admixture", run_module_admixture,
                         enabled = function(cfg) isTRUE(cfg$analyses$admixture$enabled),
                         description = "External ADMIXTURE cross-validation",
                         validate = validate_admixture_result,
                         outputs = "admixture_cv", references = "Alexander et al. 2009",
                         resource_class = "external")
  r <- register_analysis(r, "faststructure", run_module_faststructure,
                         enabled = function(cfg) isTRUE(cfg$analyses$faststructure$enabled),
                         description = "External fastStructure ancestry inference",
                         validate = validate_population_structure_result,
                         outputs = "faststructure", references = "Raj et al. 2014",
                         resource_class = "external")
  r <- register_analysis(r, "snmf", run_module_snmf,
                         enabled = function(cfg) isTRUE(cfg$analyses$snmf$enabled),
                         description = "LEA sNMF ancestry inference",
                         validate = validate_population_structure_result,
                         outputs = "snmf", references = "Frichot et al. 2014",
                         resource_class = "external")
  r <- register_analysis(r, "chromosome", run_module_chromosome,
                         enabled = function(cfg) isTRUE(cfg$analyses$chromosome_specific),
                         description = "Chromosome-specific PCA and FST",
                         validate = validate_chromosome_result,
                         outputs = "chromosome_summary", resource_class = "heavy")
  r
}
