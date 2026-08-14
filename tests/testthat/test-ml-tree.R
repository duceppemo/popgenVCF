test_that("ml_tree_encode_dna hand-verifiably maps dosage to IUPAC codes", {
  # 3 samples x 3 loci: locus 1 covers all three dosage states plus missing
  # (via a 4th sample), locus 2 is homozygous-only, locus 3 has non-ACGT
  # alleles and must be dropped entirely.
  geno <- matrix(c(
    0L, 1L, 2L, NA_integer_,
    0L, 0L, 2L, 2L,
    1L, 1L, 0L, 2L
  ), nrow = 4L, ncol = 3L, dimnames = list(paste0("s", 1:4), NULL))
  ref <- c("A", "C", "AT")
  alt <- c("G", "T", "G")
  out <- popgenVCF:::ml_tree_encode_dna(geno, ref, alt)

  expect_identical(out$n_used, 2L)
  expect_identical(out$n_dropped, 1L)
  expect_identical(dim(out$matrix), c(4L, 2L))
  # locus 1 (A/G): dosage 0,1,2,NA -> A, R, G, N
  expect_identical(unname(out$matrix[, 1]), c("A", "R", "G", "N"))
  # locus 2 (C/T): dosage 0,0,2,2 -> C, C, T, T
  expect_identical(unname(out$matrix[, 2]), c("C", "C", "T", "T"))
})

test_that("ml_tree_encode_dna drops loci with identical or non-single-base alleles", {
  geno <- matrix(c(0L, 1L, 2L, 0L, 1L, 2L), nrow = 3L, ncol = 2L)
  ref <- c("A", "N"); alt <- c("A", "G") # first: ref==alt; second: ref is not a base
  out <- popgenVCF:::ml_tree_encode_dna(geno, ref, alt)
  expect_identical(out$n_used, 0L)
  expect_identical(out$n_dropped, 2L)
  expect_identical(ncol(out$matrix), 0L)
})

test_that("ml_tree_encode_dna is case-insensitive for allele letters", {
  geno <- matrix(c(0L, 1L, 2L), nrow = 3L, ncol = 1L)
  out <- popgenVCF:::ml_tree_encode_dna(geno, "a", "g")
  expect_identical(out$n_used, 1L)
  expect_identical(unname(out$matrix[, 1]), c("A", "R", "G"))
})

# Real quickstart-derived fixture (12 samples from two maximally divergent
# real populations -- African LWK/YRI vs. East Asian CHB, the strongest
# continental signal in this dataset per this package's own prior FST/NJ
# findings -- 300 SNPs), not purely synthetic data: a synthetic fixture with
# uniform per-group allele frequencies applied identically across every
# locus (tried first) reliably drove phangorn's GTR branch-length optimizer
# into a real, directly observed numerical-stability failure (returns
# normally but with a non-finite log-likelihood -- see run_ml_tree()'s own
# handling of this) with too little data relative to sample count.
#
# Neither perfect reciprocal monophyly nor bootstrap success turned out to
# be reliably reproducible in general with this few tips/loci, even for
# maximally divergent real populations -- confirmed directly by scanning
# several (SNP count, SNP-sampling seed) combinations, not assumed: most
# gave a non-monophyletic tree and/or a gracefully-degraded ("too
# uninformative to fit") bootstrap. This exact (n = 300, seed = 1 for SNP
# sampling, seed = 3 for tree-building) combination is pinned because it was
# directly verified to give a deterministic, fully-successful bootstrap; it
# is not claimed to be the general minimum, and the tests below deliberately
# do not assert monophyly, only structural correctness and a real (if
# weaker) differentiation signal (mean between-group patristic distance
# exceeding mean within-group distance).
ml_tree_genotype_fixture <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    paths <- popgenVCF::quickstart_dataset_paths()
    gds_path <- tempfile(fileext = ".gds")
    SNPRelate::snpgdsVCF2GDS(paths$vcf, gds_path, method = "biallelic.only", verbose = FALSE)
    gds <- SNPRelate::snpgdsOpen(gds_path)
    on.exit(SNPRelate::snpgdsClose(gds))
    ids <- popgenVCF:::get_gds_ids(gds)
    meta <- data.table::fread(paths$metadata)
    sample_ids <- c(
      meta[population %in% c("LWK", "YRI")][1:6]$sample,
      meta[population == "CHB"][1:6]$sample
    )
    set.seed(1)
    snp_ids <- sample(ids$snp, 300L)
    geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = snp_ids, verbose = FALSE)
    alleles <- ids$allele[match(snp_ids, ids$snp)]
    ref <- sub("/.*", "", alleles); alt <- sub(".*/", "", alleles)
    groups <- rep(c("AFR", "EAS"), each = 6L)
    cache <<- list(geno = geno, ref = ref, alt = alt, sample_ids = sample_ids, groups = groups)
    cache
  }
})

test_that("run_ml_tree produces a structurally valid tree with real between-group differentiation", {
  skip_if_not_installed("phangorn")
  fx <- ml_tree_genotype_fixture()
  result <- popgenVCF:::run_ml_tree(
    fx$geno, fx$ref, fx$alt, fx$sample_ids, seed = 3L, threads = 1L, bootstrap_replicates = 0L
  )
  expect_s3_class(result$tree, "phylo")
  expect_identical(sort(result$tree$tip.label), sort(fx$sample_ids))
  expect_identical(result$model, "GTR+Gamma+ASC")
  expect_true(is.finite(result$log_likelihood))
  expect_identical(result$n_snps_used, 300L)
  expect_identical(result$n_snps_dropped, 0L)

  d <- ape::cophenetic.phylo(result$tree)
  afr <- fx$sample_ids[fx$groups == "AFR"]; eas <- fx$sample_ids[fx$groups == "EAS"]
  within <- mean(c(d[afr, afr][upper.tri(d[afr, afr])], d[eas, eas][upper.tri(d[eas, eas])]))
  between <- mean(d[afr, eas])
  expect_gt(between, within)
})

test_that("run_ml_tree is deterministic for a fixed seed and computes bootstrap support", {
  skip_if_not_installed("phangorn")
  fx <- ml_tree_genotype_fixture()
  r1 <- popgenVCF:::run_ml_tree(fx$geno, fx$ref, fx$alt, fx$sample_ids, seed = 3L, threads = 1L, bootstrap_replicates = 20L)
  r2 <- popgenVCF:::run_ml_tree(fx$geno, fx$ref, fx$alt, fx$sample_ids, seed = 3L, threads = 1L, bootstrap_replicates = 20L)

  expect_identical(r1$tree$node.label, r2$tree$node.label)
  expect_false(r1$bootstrap_failed)
  expect_identical(attr(r1$tree, "bootstrap_replicates"), 20L)
  support <- as.numeric(r1$tree$node.label)
  expect_false(anyNA(support))
  expect_true(all(support >= 0 & support <= 100))
})

test_that("run_ml_tree degrades gracefully instead of crashing when bootstrap resampling is too uninformative to fit", {
  skip_if_not_installed("phangorn")
  # A distinct (SNP count, SNP-sampling seed) combination directly verified
  # (twice, to confirm it is deterministic and not itself flaky) to make
  # phangorn::bootstrap.pml() hit a real internal failure ("cannot unroot a
  # tree with less than three edges", from a degenerate resampled locus
  # subset) -- exercising run_ml_tree()'s own graceful-degradation handling
  # of that failure (see the comment above it), not merely asserting the
  # happy path always works.
  set.seed(2)
  paths <- popgenVCF::quickstart_dataset_paths()
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsVCF2GDS(paths$vcf, gds_path, method = "biallelic.only", verbose = FALSE)
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  meta <- data.table::fread(paths$metadata)
  sample_ids <- c(meta[population %in% c("LWK", "YRI")][1:6]$sample, meta[population == "CHB"][1:6]$sample)
  snp_ids <- sample(ids$snp, 180L)
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = snp_ids, verbose = FALSE)
  alleles <- ids$allele[match(snp_ids, ids$snp)]
  ref <- sub("/.*", "", alleles); alt <- sub(".*/", "", alleles)

  result <- popgenVCF:::run_ml_tree(geno, ref, alt, sample_ids, seed = 3L, threads = 1L, bootstrap_replicates = 20L)
  expect_true(result$bootstrap_failed)
  expect_null(result$tree$node.label)
  expect_identical(attr(result$tree, "bootstrap_replicates"), 0L)
  # The tree itself is still a complete, valid result despite the failed bootstrap.
  expect_s3_class(result$tree, "phylo")
  expect_true(is.finite(result$log_likelihood))
})

test_that("run_ml_tree errors clearly with too few usable SNPs", {
  skip_if_not_installed("phangorn")
  fx <- ml_tree_genotype_fixture()
  expect_error(
    popgenVCF:::run_ml_tree(fx$geno[, 1:2, drop = FALSE], fx$ref[1:2], fx$alt[1:2], fx$sample_ids, seed = 1L),
    "at least 4 usable"
  )
})

test_that("ml_tree_module_spec is registered, disabled by default, and enables via config", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("ml_tree" %in% names(registry$modules))
  module <- registry$modules$ml_tree
  expect_identical(module$outputs, "ml_tree")
  cfg <- popgenVCF::default_config()
  expect_false(popgenVCF:::module_is_enabled(module, cfg))
  cfg$analyses$ml_tree$enabled <- TRUE
  expect_true(popgenVCF:::module_is_enabled(module, cfg))
})

test_that("validate_ml_tree_result accepts a well-formed result and flags defects", {
  skip_if_not_installed("phangorn")
  fx <- ml_tree_genotype_fixture()
  result <- popgenVCF:::run_ml_tree(fx$geno, fx$ref, fx$alt, fx$sample_ids, seed = 1L, threads = 1L, bootstrap_replicates = 0L)
  analysis <- list(samples = list(ids = fx$sample_ids))
  ok <- popgenVCF:::validate_ml_tree_result(result, analysis, NULL)
  expect_true(ok$valid)

  missing_field <- result; missing_field$model <- NULL
  expect_false(popgenVCF:::validate_ml_tree_result(missing_field, analysis, NULL)$valid)

  bad_loglik <- result; bad_loglik$log_likelihood <- NA_real_
  expect_false(popgenVCF:::validate_ml_tree_result(bad_loglik, analysis, NULL)$valid)

  bad_tips <- result
  wrong_analysis <- list(samples = list(ids = c(fx$sample_ids, "extra")))
  expect_false(popgenVCF:::validate_ml_tree_result(bad_tips, wrong_analysis, NULL)$valid)
})

test_that("ml_tree.bootstrap_replicates config validates non-negative", {
  cfg <- popgenVCF::default_config()
  expect_false(cfg$analyses$ml_tree$enabled)
  expect_identical(cfg$analyses$ml_tree$bootstrap_replicates, 100L)

  cfg$input$vcf <- popgenVCF::quickstart_dataset_paths()$vcf
  cfg$output$directory <- tempfile("cfgtest-")
  cfg$analyses$ml_tree$bootstrap_replicates <- -1L
  expect_error(popgenVCF:::validate_config(cfg), "ml_tree.bootstrap_replicates")
})
