test_that("run_pipeline executes end to end and writes the documented artifacts", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  paths <- popgenVCF:::validation_fixture_paths()
  root <- withr::local_tempdir()

  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- paths$vcf
  cfg$input$metadata <- paths$metadata
  cfg$output$directory <- root
  cfg$compute$threads <- 1L
  cfg$analyses$n_pcs <- 3L
  # Default genome_scan_min_snps (5) is too strict for this tiny fixture's
  # ~500bp position span; lowering it exercises the real figure-drawing
  # path too, not just the table-writing path.
  cfg$analyses$genome_scan_min_snps <- 2L
  cfg$analyses$dapc_k <- "2:3"
  cfg$analyses$dapc_cross_validation <- FALSE
  cfg$analyses$bootstrap$enabled <- FALSE
  cfg$analyses$structure$replicates <- 1L
  cfg$report$enabled <- FALSE

  analysis <- popgenVCF::run_pipeline(cfg)

  expect_s3_class(analysis, "PopgenVCFAnalysis")
  expect_identical(analysis$status, "complete")

  results_rds <- file.path(root, "analysis_results.rds")
  expect_true(file.exists(results_rds))
  reloaded <- readRDS(results_rds)
  expect_identical(reloaded$status, "complete")

  for (rel in c(
    "run_manifest.tsv", "analysis_summary.tsv",
    "tables/05_variant_QC.tsv", "tables/08_LD_pruned_SNPs.tsv",
    "tables/12_PCA_scores.tsv", "tables/17_global_FST.tsv",
    "tables/21_DAPC_diagnostics.tsv", "tables/22f_DAPC_loadings_K2.tsv",
    "tables/22f_DAPC_loadings_K3.tsv",
    "figures/15_DAPC_loadings_manhattan_K2.pdf",
    "figures/16_DAPC_loadings_ranked_K2.pdf",
    "tables/31_PCA_loadings.tsv",
    "figures/17_PCA_loadings_manhattan.pdf",
    "figures/18_PCA_loadings_ranked.pdf",
    "figures/19_HWE_pvalues.pdf",
    "tables/32_private_alleles.tsv",
    "figures/20_private_alleles.pdf",
    "tables/33_kinship_matrix.tsv",
    "tables/34_kinship_IBS0_matrix.tsv",
    "tables/35_kinship_pairs.tsv",
    "figures/21_kinship_heatmap.pdf",
    "figures/22_kinship_IBS0_vs_kinship.pdf",
    "tables/37_ROH_runs.tsv",
    "tables/38_ROH_sample_summary.tsv",
    "figures/23_ROH_length_distribution.pdf",
    "figures/24_ROH_FROH_by_sample.pdf",
    "tables/39_genome_scan_fst.tsv",
    "tables/40_genome_scan_diversity.tsv",
    "figures/25_genome_scan_FST_manhattan.pdf",
    "figures/26_genome_scan_diversity_manhattan.pdf",
    "tables/23_AMOVA_components.tsv",
    "tables/25_Mantel_IBD_summary.tsv", "trees/IBS_neighbor_joining.nwk"
  )) {
    expect_true(file.exists(file.path(root, rel)), info = rel)
  }

  manifest <- data.table::fread(file.path(root, "run_manifest.tsv"))
  expect_identical(manifest$value[manifest$field == "samples"], "8")

  pca_scores <- data.table::fread(file.path(root, "tables/12_PCA_scores.tsv"))
  expect_identical(nrow(pca_scores), 8L)

  loadings <- data.table::fread(file.path(root, "tables/22f_DAPC_loadings_K2.tsv"))
  expect_setequal(
    names(loadings), c("axis", "rank", "snp_id", "chromosome", "position", "contribution")
  )
  expect_true(all(loadings[, .N, by = axis]$N <= cfg$analyses$dapc_loading_top_n))
  expect_true(all(loadings[, min(rank) == 1L && all(diff(rank) == 1L), by = axis]$V1))

  pca_loadings <- data.table::fread(file.path(root, "tables/31_PCA_loadings.tsv"))
  expect_setequal(
    names(pca_loadings),
    c("axis", "rank", "snp_id", "chromosome", "position", "contribution", "magnitude")
  )
  expect_true(all(pca_loadings[, .N, by = axis]$N <= cfg$analyses$pca_loading_top_n))
  expect_true(all(pca_loadings[, min(rank) == 1L && all(diff(rank) == 1L), by = axis]$V1))
  expect_equal(pca_loadings$magnitude, abs(pca_loadings$contribution))

  expect_false(is.null(reloaded$results$pca$loadings))
  expect_true(nrow(reloaded$results$pca$loadings) > 0L)

  population_diversity <- data.table::fread(file.path(root, "tables/09_population_diversity.tsv"))
  expect_true(all(c(
    "hwe_tested_loci", "hwe_significant_loci", "hwe_significant_loci_fdr", "private_allele_loci"
  ) %in% names(population_diversity)))
  expect_true(sum(population_diversity$hwe_tested_loci) > 0L)
  expect_true(sum(population_diversity$private_allele_loci) > 0L)

  locus_diversity <- data.table::fread(file.path(root, "tables/10_population_locus_diversity.tsv"))
  expect_true(all(c("hwe_pvalue", "private_allele", "reference_allele_count") %in% names(locus_diversity)))

  private_alleles <- data.table::fread(file.path(root, "tables/32_private_alleles.tsv"))
  expect_setequal(
    names(private_alleles),
    c("population", "snp_id", "chromosome", "position", "private_allele",
      "alternate_allele_count", "reference_allele_count")
  )
  expect_true(all(private_alleles$private_allele %in% c("ref", "alt", "both")))
  expect_identical(nrow(private_alleles), sum(population_diversity$private_allele_loci))

  kinship_matrix <- data.table::fread(file.path(root, "tables/33_kinship_matrix.tsv"))
  expect_identical(nrow(kinship_matrix), 8L)
  kinship_pairs <- data.table::fread(file.path(root, "tables/35_kinship_pairs.tsv"))
  expect_setequal(
    names(kinship_pairs),
    c("sample_1", "sample_2", "population_1", "population_2", "IBS0", "kinship", "relationship_degree")
  )
  expect_identical(nrow(kinship_pairs), 28L)
  # The bundled 9-SNP validation fixture is too small for KING-robust to be
  # scientifically meaningful (established elsewhere this session for HWE and
  # kinship alike); some/all pairs can legitimately come out non-finite when
  # LD pruning leaves very few markers, so only structural bounds are checked.
  expect_true(all(kinship_pairs$kinship <= 0.5 + 1e-6, na.rm = TRUE))
  expect_true(all(
    kinship_pairs$relationship_degree %in% c(
      "duplicate/MZ twin", "1st-degree", "2nd-degree", "3rd-degree", "unrelated"
    ) | is.na(kinship_pairs$relationship_degree)
  ))
  expect_false(is.null(reloaded$results$kinship$close_relatives))

  roh_runs <- data.table::fread(file.path(root, "tables/37_ROH_runs.tsv"))
  expect_setequal(
    names(roh_runs),
    c("sample", "population", "chromosome", "start", "end", "length_bp", "n_markers", "quality")
  )
  roh_summary <- data.table::fread(file.path(root, "tables/38_ROH_sample_summary.tsv"))
  expect_identical(nrow(roh_summary), 8L)
  expect_setequal(
    names(roh_summary),
    c("sample", "population", "n_runs", "total_length_bp", "mean_length_bp", "longest_run_bp", "froh")
  )
  # Values are not scientifically meaningful at 9 SNPs (established elsewhere
  # this session for HWE/kinship); only structural bounds are checked.
  expect_true(all(roh_summary$froh >= -1e-6 & roh_summary$froh <= 1 + 1e-6))
  expect_false(is.null(reloaded$results$roh$sample_summary))

  fst_windows <- data.table::fread(file.path(root, "tables/39_genome_scan_fst.tsv"))
  expect_setequal(names(fst_windows), c("chromosome", "window_start", "window_end", "n_snps", "global_fst"))
  expect_identical(data.table::uniqueN(fst_windows$chromosome), 2L)
  diversity_windows <- data.table::fread(file.path(root, "tables/40_genome_scan_diversity.tsv"))
  expect_setequal(
    names(diversity_windows),
    c("chromosome", "window_start", "window_end", "population", "n_snps",
      "mean_observed_heterozygosity", "mean_expected_heterozygosity",
      "segregating_sites", "tajima_d")
  )
  expect_false(is.null(reloaded$results$genome_scan$outliers))
})

test_that("run_pipeline completes without a metadata file and records no metadata hash", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  paths <- popgenVCF:::validation_fixture_paths()
  root <- withr::local_tempdir()

  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- paths$vcf
  cfg$output$directory <- root
  cfg$compute$threads <- 1L
  cfg$analyses$n_pcs <- 3L
  cfg$report$enabled <- FALSE

  expect_warning(
    analysis <- popgenVCF::run_pipeline(cfg), "Skipping unavailable analysis module"
  )

  expect_identical(analysis$status, "complete")

  manifest <- data.table::fread(file.path(root, "run_manifest.tsv"), na.strings = "NA")
  expect_true(is.na(manifest$value[manifest$field == "metadata"]))
  expect_true(is.na(manifest$value[manifest$field == "metadata_sha256"]))
})
