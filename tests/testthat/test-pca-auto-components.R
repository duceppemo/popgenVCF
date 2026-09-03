test_that("pca_component_count(\"auto\", ...) requests the generous 100-component cap, not a fixed small default", {
  # n_samples - 1 must itself exceed 100 for the cap to bind -- 20 samples
  # alone would cap available at 19, testing "capped by the data" instead.
  many_sample_ids <- paste0("s", 1:200)
  snp_ids <- 1:1000
  expect_identical(popgenVCF:::pca_component_count("auto", many_sample_ids, snp_ids), 100L)

  # Capped by what the data can actually support when fewer are available.
  sample_ids <- paste0("s", 1:20)
  small_snp_ids <- 1:5
  expect_identical(
    popgenVCF:::pca_component_count("auto", sample_ids, small_snp_ids), 5L
  )
})

test_that("pca_component_count still rejects a non-\"auto\", non-numeric n_pcs and values below 2", {
  sample_ids <- paste0("s", 1:20)
  snp_ids <- 1:300
  expect_error(
    suppressWarnings(popgenVCF:::pca_component_count("bogus", sample_ids, snp_ids)),
    "auto"
  )
  expect_error(
    popgenVCF:::pca_component_count(1L, sample_ids, snp_ids),
    "at least two"
  )
})

test_that("pca_significant_component_count runs LEA's real Tracy-Widom routine and returns a valid component count", {
  skip_if_not(requireNamespace("LEA", quietly = TRUE), "LEA is not installed")

  # A steep, clearly-structured spectrum: two large eigenvalues (real
  # signal) followed by a long flat tail of near-equal small ones (noise) --
  # the sequential test should stop well before the tail, not include all
  # of it, and never return fewer than 2.
  eig <- c(2000, 1500, rep(10, 30))
  n <- popgenVCF:::pca_significant_component_count(eig)

  expect_true(is.numeric(n))
  expect_gte(n, 2L)
  expect_lt(n, length(eig))
})

test_that("pca_significant_component_count errors clearly when LEA is not available", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(
    popgenVCF:::pca_significant_component_count(c(100, 50, 10, 5)),
    "LEA"
  )
})

test_that("run_pca(n_pcs = \"auto\") trims the reported components to the Tracy-Widom significant count", {
  skip_if_not(requireNamespace("LEA", quietly = TRUE), "LEA is not installed")
  set.seed(3L)
  n_samples <- 20L; n_snps <- 300L
  sample_id <- paste0("s", seq_len(n_samples))
  snp_id <- seq_len(n_snps)
  genmat <- matrix(
    sample(0:2, n_samples * n_snps, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    nrow = n_samples, ncol = n_snps
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep("PopA", n_samples)
  ))

  full <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, 19L, 1L)
  expected_n <- popgenVCF:::pca_significant_component_count(full$eigenvalues[full$eigenvalues > 0])

  auto <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, "auto", 1L)

  expect_identical(nrow(auto$variance), expected_n)
  expect_identical(ncol(auto$scores) - (2L + ("population" %in% names(auto$scores))), expected_n)
  expect_lte(expected_n, 19L)
})

test_that("validate_config accepts analyses.n_pcs = \"auto\" and rejects other strings", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  file.create(cfg$input$vcf)
  cfg$output$directory <- tempfile("popgenvcf-output-")
  cfg$analyses$n_pcs <- "auto"
  validated <- popgenVCF:::validate_config(cfg)
  expect_identical(validated$analyses$n_pcs, "auto")

  cfg$analyses$n_pcs <- "AUTO"
  expect_error(popgenVCF:::validate_config(cfg), "auto")

  cfg$analyses$n_pcs <- "bogus"
  expect_error(popgenVCF:::validate_config(cfg), "auto")
})
