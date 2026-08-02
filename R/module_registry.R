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
  chromosome <- context$ids$chromosome[match(context$qc_snps, context$ids$snp)]
  position <- context$ids$position[match(context$qc_snps, context$ids$snp)]
  dapc <- run_dapc_analysis(div$genotype, context$sample_ids, context$metadata,
                            parse_int_range(cfg$analyses$dapc_k), cfg$compute$seed,
                            cfg$analyses$dapc_cross_validation,
                            replicate_seeds = seeds,
                            threads = cfg$compute$threads,
                            snp_ids = context$qc_snps,
                            chromosome = chromosome, position = position)
  analysis <- set_analysis_result(analysis, "dapc", dapc)
  write_tsv(dapc$diagnostics, file.path(dirs$tables, "21_DAPC_diagnostics.tsv"))
  top_n <- cfg$analyses$dapc_loading_top_n
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
    loadings <- dapc$models[[k]]$loadings
    if (!is.null(loadings) && nrow(loadings)) {
      # loadings is already sorted axis, -contribution by dapc_loading_table().
      top <- loadings[, .SD[seq_len(min(.N, top_n))], by = axis]
      top[, rank := seq_len(.N), by = axis]
      data.table::setcolorder(top, c("axis", "rank", "snp_id", "chromosome", "position", "contribution"))
      write_tsv(top, file.path(dirs$tables, sprintf("22f_DAPC_loadings_K%s.tsv", k)))
    }
  }
  if (!is.null(dapc$k_selection)) {
    write_tsv(dapc$k_selection$best_by_method, file.path(dirs$tables, "22d_DAPC_K_selection.tsv"))
    write_structure_k_selection(
      dapc$k_selection, dirs, "22e_DAPC_K_selection"
    )
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

run_module_snmf <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; sc <- cfg$analyses$snmf
  snmf_input <- prepare_snmf_input(
    context$gds, context$sample_ids, context$final_snps,
    preferred_geno_file = sc$geno_file,
    preferred_sample_file = sc$q_sample_file,
    cache_dir = dirs$cache
  )
  context$snmf_input <- snmf_input
  result <- run_snmf(
    snmf_input$geno_file, parse_int_range(sc$k), sc$repetitions,
    sc$entropy, cfg$compute$seed, threads = sc$threads
  )
  result$diagnostics <- data.table::as.data.table(result$diagnostics)
  selection_diagnostics <- summarize_snmf_k_diagnostics(result$diagnostics)
  finite_cross_entropy <- selection_diagnostics$cross_entropy[
    is.finite(selection_diagnostics$cross_entropy)
  ]
  k_selection <- if (length(unique(finite_cross_entropy)) > 1L) {
    select_structure_k(selection_diagnostics)
  } else {
    NULL
  }
  result$k_selection <- k_selection
  write_tsv(
    result$diagnostics,
    file.path(dirs$tables, "30_sNMF_cross_entropy.tsv")
  )
  if (!is.null(k_selection)) {
    write_structure_k_selection(k_selection, dirs, "30b_sNMF_K_selection")
    plot_structure_k_selection(
      k_selection, cfg, dirs,
      stem = "13c_sNMF_cluster_number_selection",
      title = paste("Sparse non-negative matrix factorization",
                    "cluster-number selection")
    )
  }
  sample_order <- readLines(snmf_input$sample_file, warn = FALSE)
  for (k in names(result$q)) {
    q <- result$q[[k]]
    if (nrow(q) != length(sample_order)) {
      stop("sNMF Q rows do not match sample-order file", call. = FALSE)
    }
    qdt <- data.table::as.data.table(q); qdt[, sample := sample_order]
    qdt[, population := context$metadata$population[match(sample, context$metadata$sample)]]
    if (anyNA(qdt$population)) stop("Some sNMF samples are absent from retained metadata", call. = FALSE)
    data.table::setcolorder(qdt, c("sample", "population", grep("^cluster_", names(qdt), value = TRUE)))
    result$q[[k]] <- qdt
    write_tsv(qdt, file.path(dirs$tables, sprintf("30_sNMF_Q_K%s.tsv", k)))
    plot_q_matrix_views(
      qdt, as.integer(k), cfg, dirs, prefix = "sNMF_Q",
      sample_labels = public_sample_ids(context$metadata, qdt$sample)
    )
  }
  analysis <- set_analysis_result(analysis, "snmf", result)
  if (is.null(k_selection)) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "snmf",
      paste(
        "Cross-entropy values were unavailable or did not vary;",
        "cluster-number consensus was skipped"
      )
    )
  }
  analysis <- record_analysis_message(
    analysis, "INFO", "snmf",
    paste("sNMF input", snmf_input$source, "with", snmf_input$n_samples,
          "samples and", snmf_input$n_snps, "SNPs")
  )
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
