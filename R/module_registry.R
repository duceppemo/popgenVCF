module_result <- function(analysis, context) list(analysis = analysis, context = context)

run_module_diversity <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  div <- compute_diversity(context$gds, context$sample_ids, context$qc_snps,
                           context$metadata, context$ids, cfg$analyses$hwe_alpha)
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
  if (sum(div$locus$private_allele != "none", na.rm = TRUE) > 0L) {
    private_loci <- div$locus[
      private_allele != "none",
      .(population, snp_id, chromosome, position, private_allele,
        alternate_allele_count, reference_allele_count)
    ]
    write_tsv(private_loci, file.path(dirs$tables, "32_private_alleles.tsv"))
  }
  if (!isTRUE(div$allelic_richness_available)) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "diversity",
      "Allelic richness was skipped: the optional hierfstat package is not installed"
    )
  }
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

run_module_kinship <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  result <- run_kinship(context$gds, context$sample_ids, context$final_snps,
                        context$metadata, cfg$compute$threads)
  threshold <- cfg$analyses$kinship_close_relative_threshold
  close_relatives <- result$pairs[is.finite(kinship) & kinship > threshold]
  analysis <- set_analysis_result(analysis, "kinship", list(
    close_relatives = close_relatives,
    matrix_file = file.path(dirs$tables, "33_kinship_matrix.tsv"),
    ibs0_file = file.path(dirs$tables, "34_kinship_IBS0_matrix.tsv"),
    pairs_file = file.path(dirs$tables, "35_kinship_pairs.tsv")
  ))
  write_matrix_tsv(result$kinship, file.path(dirs$tables, "33_kinship_matrix.tsv"), "sample")
  write_matrix_tsv(result$ibs0, file.path(dirs$tables, "34_kinship_IBS0_matrix.tsv"), "sample")
  write_tsv(result$pairs, file.path(dirs$tables, "35_kinship_pairs.tsv"))
  if (nrow(close_relatives)) {
    write_tsv(close_relatives, file.path(dirs$tables, "36_close_relatives.tsv"))
  }
  plot_kinship(result, cfg, dirs)
  module_result(analysis, context)
}

run_module_sex_check <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  result <- run_sex_check(
    context$gds, context$sample_ids, context$qc_snps_all, context$ids, context$metadata,
    cfg$analyses$sex_check_x_chromosome_names,
    cfg$analyses$sex_check_male_f_threshold, cfg$analyses$sex_check_female_f_threshold,
    cfg$analyses$sex_check_y_chromosome_names,
    cfg$analyses$sex_check_y_male_call_rate_threshold, cfg$analyses$sex_check_y_female_call_rate_threshold
  )
  analysis <- set_analysis_result(analysis, "sex_check", result)
  if (!is.null(result) && nrow(result$table)) {
    write_tsv(result$table, file.path(dirs$tables, "42_sex_check.tsv"))
    plot_sex_check(result, cfg, dirs)
  }
  module_result(analysis, context)
}

run_module_roh <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  non_autosomal <- if (isTRUE(cfg$qc$autosome_only)) cfg$qc$non_autosomal_chromosome_names else character()
  result <- run_roh(context$vcf_path, context$sample_ids, context$metadata,
                    cfg$qc$max_variant_missing, cfg$analyses$roh_gt_error_phred,
                    cfg$compute$threads, non_autosomal,
                    cfg$analyses$roh_length_class_short_max_bp,
                    cfg$analyses$roh_length_class_long_min_bp)
  analysis <- set_analysis_result(analysis, "roh", result)
  write_tsv(result$runs, file.path(dirs$tables, "37_ROH_runs.tsv"))
  write_tsv(result$sample_summary, file.path(dirs$tables, "38_ROH_sample_summary.tsv"))
  plot_roh(result, cfg, dirs)
  plot_roh_length_class(result, cfg, dirs)
  module_result(analysis, context)
}

run_module_tree <- function(analysis, context) {
  tree <- build_nj_tree(
    context$ibs, context$metadata, context$cfg, context$dirs,
    gds = context$gds, sample_ids = context$sample_ids, snp_ids = context$final_snps
  )
  analysis <- set_analysis_result(analysis, "tree", tree)
  plot_nj_tree(tree, context$metadata, context$cfg, context$dirs, "52_IBS_tree", "Neighbor-joining tree (identity-by-state)")
  module_result(analysis, context)
}

run_module_ml_tree <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  genotype <- SNPRelate::snpgdsGetGeno(
    context$gds, sample.id = context$sample_ids, snp.id = context$final_snps, verbose = FALSE
  )
  alleles <- context$ids$allele[match(context$final_snps, context$ids$snp)]
  ref <- sub("/.*", "", alleles); alt <- sub(".*/", "", alleles)
  result <- run_ml_tree(
    genotype, ref, alt, public_sample_ids(context$metadata, context$sample_ids),
    seed = cfg$compute$seed, threads = cfg$compute$threads,
    bootstrap_replicates = cfg$analyses$ml_tree$bootstrap_replicates
  )
  ape::write.tree(result$tree, file.path(dirs$trees, "ML_GTR_gamma_ASC.nwk"))
  write_tsv(
    data.table::data.table(
      model = result$model, log_likelihood = result$log_likelihood,
      n_snps_used = result$n_snps_used, n_snps_dropped = result$n_snps_dropped,
      bootstrap_replicates = attr(result$tree, "bootstrap_replicates")
    ),
    file.path(dirs$tables, "54_ML_tree_summary.tsv")
  )
  analysis <- set_analysis_result(analysis, "ml_tree", result)
  plot_nj_tree(
    result$tree, context$metadata, cfg, dirs, "55_ML_tree",
    "Maximum-likelihood tree (GTR+Gamma, ascertainment-bias corrected)"
  )
  if (result$n_snps_dropped > 0L) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "ml_tree",
      paste0(
        result$n_snps_dropped, " of ", result$n_snps_dropped + result$n_snps_used,
        " retained SNP(s) were excluded from the maximum-likelihood tree ",
        "(non-single-base or non-distinct reference/alternate alleles)"
      )
    )
  }
  if (isTRUE(result$bootstrap_failed)) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "ml_tree",
      "Bootstrap support could not be computed for the maximum-likelihood tree (a resampled locus set was too uninformative to fit); the tree itself is unaffected"
    )
  }
  module_result(analysis, context)
}

run_module_fst <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  fst <- run_fst(context$gds, context$qc_snps, context$metadata)
  jost <- compute_jost_d(context$diversity_full$locus)
  fst$global_jost_d <- jost$global
  fst$jost_d_matrix <- jost$matrix
  fst$long[, `:=`(jost_d = NA_real_, jost_d_n_snps = NA_integer_)]
  if (nrow(jost$long)) {
    fst$long[jost$long, `:=`(jost_d = i.jost_d, jost_d_n_snps = i.jost_d_n_snps),
             on = c("population_1", "population_2")]
  }
  beta <- compute_population_specific_fst(
    context$diversity_full$genotype, context$diversity_full$sample$population, context$qc_snps
  )
  fst$global_beta_fst <- beta$overall
  fst$population_specific_fst <- beta$table
  fst_ci <- if (isTRUE(cfg$analyses$bootstrap$enabled)) {
    bootstrap_fst(context$gds, context$qc_snps, context$ids, context$metadata,
                  cfg$analyses$bootstrap$replicates, cfg$compute$seed)
  } else data.table::data.table()
  analysis <- set_analysis_result(analysis, "fst", fst)
  analysis <- set_analysis_result(analysis, "fst_ci", fst_ci)
  write_tsv(data.table::data.table(
    global_fst = fst$global, global_nm = fst$global_nm, global_jost_d = fst$global_jost_d,
    global_beta_fst = fst$global_beta_fst
  ), file.path(dirs$tables, "17_global_FST.tsv"))
  write_tsv(fst$long, file.path(dirs$tables, "18_pairwise_FST.tsv"))
  write_matrix_tsv(fst$matrix, file.path(dirs$tables, "19_pairwise_FST_matrix.tsv"), "population")
  if (nrow(jost$matrix)) write_matrix_tsv(jost$matrix, file.path(dirs$tables, "19b_pairwise_jost_d_matrix.tsv"), "population")
  if (nrow(fst_ci)) write_tsv(fst_ci, file.path(dirs$tables, "20_pairwise_FST_bootstrap_CI.tsv"))
  if (beta$available) {
    write_tsv(beta$table, file.path(dirs$tables, "51_population_specific_fst.tsv"))
    plot_population_specific_fst(beta, cfg, dirs)
  } else {
    analysis <- record_analysis_message(
      analysis, "WARNING", "fst",
      "Population-specific FST (Weir and Goudet 2017) skipped: hierfstat is not installed"
    )
  }
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

run_module_genome_scan <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  window_bp <- cfg$analyses$genome_scan_window_bp
  step_bp <- cfg$analyses$genome_scan_step_bp
  min_snps <- cfg$analyses$genome_scan_min_snps
  fst_windows <- run_genome_scan_fst(
    context$gds, context$qc_snps, context$ids, context$metadata,
    window_bp, step_bp, min_snps
  )
  population_n <- context$metadata[, .N, by = population][, stats::setNames(N, population)]
  diversity_windows <- run_genome_scan_diversity(
    context$diversity_full$locus, window_bp, step_bp, min_snps, population_n
  )
  outliers <- fst_windows[is.finite(global_fst)]
  data.table::setorder(outliers, -global_fst)
  outliers <- head(outliers, 20L)
  analysis <- set_analysis_result(analysis, "genome_scan", list(
    fst_windows = fst_windows, diversity_windows = diversity_windows, outliers = outliers
  ))
  write_tsv(fst_windows, file.path(dirs$tables, "39_genome_scan_fst.tsv"))
  write_tsv(diversity_windows, file.path(dirs$tables, "40_genome_scan_diversity.tsv"))
  if (nrow(outliers)) write_tsv(outliers, file.path(dirs$tables, "41_genome_scan_FST_outliers.tsv"))
  plot_genome_scan(fst_windows, diversity_windows, cfg, dirs)
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

run_module_clonality <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; div <- context$diversity_full
  result <- run_clonality(
    div$genotype, context$sample_ids, context$metadata, cfg$compute$seed,
    curve_replicates = cfg$analyses$clonality_genotype_curve_replicates,
    ia_permutations = cfg$analyses$clonality_ia_permutations
  )
  analysis <- set_analysis_result(analysis, "clonality", result)
  write_tsv(result$summary, file.path(dirs$tables, "56_MLG_diversity_summary.tsv"))
  write_tsv(result$groups, file.path(dirs$tables, "57_MLG_groups.tsv"))
  if (nrow(result$curve)) write_tsv(result$curve, file.path(dirs$tables, "58_genotype_accumulation_curve.tsv"))
  plot_clonality(result, cfg, dirs)
  if (nrow(result$groups)) {
    n_cross <- sum(result$groups$cross_population)
    msg <- sprintf(
      "%d sample(s) share an identical multilocus genotype with at least one other sample (%d group(s)%s)",
      sum(result$groups$n_members), nrow(result$groups),
      if (n_cross > 0L) sprintf(", %d spanning more than one recorded population", n_cross) else ""
    )
    analysis <- record_analysis_message(analysis, "WARNING", "clonality", msg)
  }
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

run_module_spatial_autocorrelation <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  geno <- SNPRelate::snpgdsGetGeno(
    context$gds, sample.id = context$sample_ids, snp.id = context$final_snps,
    snpfirstdim = FALSE, verbose = FALSE
  )
  rownames(geno) <- context$sample_ids
  result <- run_spatial_autocorrelation(
    geno, context$sample_ids, context$metadata, cfg$input$geographic_columns,
    cfg$analyses$spatial_autocorrelation_bins,
    cfg$analyses$spatial_autocorrelation_permutations, cfg$compute$seed
  )
  analysis <- set_analysis_result(analysis, "spatial_autocorrelation", result)
  if (!is.null(result)) {
    write_tsv(result, file.path(dirs$tables, "50_spatial_autocorrelation.tsv"))
    plot_spatial_autocorrelation(result, cfg, dirs)
  } else {
    log_msg("Skipping spatial autocorrelation because valid latitude/longitude columns were unavailable", level = "WARNING")
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

run_module_ld_decay <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  result <- compute_ld_decay(
    context$gds, context$sample_ids, context$qc_snps, context$ids,
    cfg$analyses$ld_decay_max_distance_bp, cfg$analyses$ld_decay_bin_bp,
    cfg$analyses$ld_decay_slide
  )
  analysis <- set_analysis_result(analysis, "ld_decay", result)
  write_tsv(result$binned, file.path(dirs$tables, "43_LD_decay.tsv"))
  plot_ld_decay(result, cfg, dirs)
  module_result(analysis, context)
}

run_module_ne_ld <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  result <- compute_ne_ld(
    context$gds, context$sample_ids, context$qc_snps, context$ids, context$metadata,
    cfg$analyses$ne_ld_max_snps, cfg$compute$seed
  )
  analysis <- set_analysis_result(analysis, "ne_ld", result)
  write_tsv(result, file.path(dirs$tables, "45_Ne_LD.tsv"))
  if (nrow(result) && all(result$ne_status == "fewer_than_two_chromosomes")) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "ne_ld",
      paste(
        "LD-based Ne estimation requires unlinked (cross-chromosome) marker pairs;",
        "the retained autosomal marker set spans only one chromosome"
      )
    )
  }
  plot_ne_ld(result, cfg, dirs)
  module_result(analysis, context)
}

run_module_population_tree <- function(analysis, context) {
  dirs <- context$dirs; cfg <- context$cfg
  result <- compute_population_genetic_distance(context$diversity_full$locus)
  if (nrow(result$distance)) {
    write_matrix_tsv(result$distance, file.path(dirs$tables, "46_population_genetic_distance.tsv"), "population")
    result$tree <- build_population_tree(result$distance, dirs, cfg, context$diversity_full$locus)
    if (!is.null(result$tree)) {
      plot_nj_tree(result$tree, NULL, cfg, dirs, "53_population_tree", "Neighbor-joining tree (Nei's genetic distance)")
    }
  }
  analysis <- set_analysis_result(analysis, "population_tree", result)
  module_result(analysis, context)
}

run_module_population_assignment <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  div <- compute_diversity(
    context$gds, context$sample_ids, context$final_snps, context$metadata,
    context$ids, cfg$analyses$hwe_alpha
  )
  result <- run_population_assignment(div$genotype, div$sample, div$locus, context$final_snps)
  analysis <- set_analysis_result(analysis, "population_assignment", result)
  write_tsv(result$assignment, file.path(dirs$tables, "47_population_assignment.tsv"))
  plot_population_assignment(result, cfg, dirs)
  n_mismatch <- sum(result$assignment$mismatch, na.rm = TRUE)
  if (n_mismatch > 0L) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "population_assignment",
      sprintf("%d sample(s) assign to a population other than their recorded label", n_mismatch)
    )
  }
  module_result(analysis, context)
}

run_module_bottleneck <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  result <- run_bottleneck_analysis(context$diversity_full$locus, cfg$analyses$bottleneck_n_bins)
  analysis <- set_analysis_result(analysis, "bottleneck", result)
  write_tsv(result$spectrum, file.path(dirs$tables, "48_site_frequency_spectrum.tsv"))
  write_tsv(result$summary, file.path(dirs$tables, "49_bottleneck_mode_shift.tsv"))
  plot_bottleneck(result, cfg, dirs)
  n_shifted <- sum(result$summary$mode_shifted, na.rm = TRUE)
  if (n_shifted > 0L) {
    analysis <- record_analysis_message(
      analysis, "WARNING", "bottleneck",
      sprintf(
        "%d population(s) show a mode-shifted site frequency spectrum, a possible recent-bottleneck signature",
        n_shifted
      )
    )
  }
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
