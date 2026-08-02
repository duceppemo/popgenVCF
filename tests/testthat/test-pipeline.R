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
