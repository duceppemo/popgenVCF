population_tree_fixture <- function() {
  set.seed(1)
  n_pop <- 4L; n_per_pop <- 15L; n_snp <- 60L
  pops <- rep(LETTERS[1:n_pop], each = n_per_pop)
  # Distinct, monotonically ordered allele frequencies per population --
  # Nei's distance should grow monotonically with the frequency gap, a real
  # correctness check, not just a formula self-consistency check.
  base_freqs <- c(A = 0.2, B = 0.4, C = 0.6, D = 0.8)
  sample_id <- paste0("S", seq_len(length(pops)))
  genmat <- matrix(NA_integer_, nrow = length(pops), ncol = n_snp)
  for (j in seq_len(n_snp)) {
    for (i in seq_along(pops)) genmat[i, j] <- rbinom(1, 2, base_freqs[[pops[i]]])
  }
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = seq_len(n_snp),
    snp.chromosome = rep(1L, n_snp), snp.position = seq_len(n_snp) * 100L,
    snp.allele = rep("A/G", n_snp), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- data.table::data.table(sample = sample_id, population = pops)
  div <- popgenVCF:::compute_diversity(gds, sample_id, ids$snp, metadata, ids)
  list(gds = gds, div = div)
}

test_that("compute_population_genetic_distance matches an independent hand calculation of Nei's D", {
  # Two populations, two loci, hand-picked allele counts -- Nei's D
  # computed by hand (I = Jxy / sqrt(Jx*Jy), D = -ln(I)) and cross-checked
  # against adegenet::dist.genpop(method=1) directly before this test was
  # written (see NEWS.md); this pins that exact value.
  lt <- data.table::data.table(
    population = rep(c("A", "B"), each = 2L),
    snp_id = rep(c(1L, 2L), 2L),
    n_called = c(20L, 20L, 20L, 20L),
    alternate_allele_count = c(30L, 15L, 15L, 8L),
    reference_allele_count = c(10L, 5L, 25L, 12L)
  )
  res <- popgenVCF:::compute_population_genetic_distance(lt)
  expect_equal(res$distance["A", "B"], 0.25590850, tolerance = 1e-6)
  expect_equal(res$distance["B", "A"], 0.25590850, tolerance = 1e-6)
  expect_identical(diag(res$distance), c(A = 0, B = 0))
  expect_identical(res$n_snps, 2L)
})

test_that("compute_population_genetic_distance recovers a monotonic ordering by allele-frequency divergence", {
  fx <- population_tree_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_population_genetic_distance(fx$div$locus)

  expect_identical(rownames(res$distance), c("A", "B", "C", "D"))
  d <- res$distance
  # Frequencies are 0.2, 0.4, 0.6, 0.8 for A, B, C, D -- distance should
  # increase monotonically with the frequency gap.
  expect_lt(d["A", "B"], d["A", "C"])
  expect_lt(d["A", "C"], d["A", "D"])
  expect_lt(d["B", "C"], d["B", "D"])
  expect_true(all(d >= 0))
})

test_that("compute_population_genetic_distance excludes loci with zero calls in any population", {
  lt <- data.table::data.table(
    population = rep(c("A", "B"), each = 2L),
    snp_id = rep(c(1L, 2L), 2L),
    n_called = c(20L, 0L, 20L, 20L),
    alternate_allele_count = c(30L, 0L, 15L, 8L),
    reference_allele_count = c(10L, 0L, 25L, 12L)
  )
  res <- popgenVCF:::compute_population_genetic_distance(lt)
  expect_identical(res$n_snps, 1L)
})

test_that("compute_population_genetic_distance returns an empty result with fewer than two populations", {
  lt <- data.table::data.table(
    population = "A", snp_id = 1:3, n_called = c(10L, 10L, 10L),
    alternate_allele_count = c(5L, 3L, 8L), reference_allele_count = c(15L, 17L, 12L)
  )
  res <- popgenVCF:::compute_population_genetic_distance(lt)
  expect_identical(nrow(res$distance), 0L)
  expect_identical(res$populations, "A")
})

test_that("build_population_tree writes a Newick file for at least three populations, NULL otherwise", {
  fx <- population_tree_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_population_genetic_distance(fx$div$locus)
  td <- tempfile("trees-"); dir.create(td)
  tree <- popgenVCF:::build_population_tree(res$distance, list(trees = td))
  expect_s3_class(tree, "phylo")
  expect_identical(sort(tree$tip.label), c("A", "B", "C", "D"))
  expect_true(file.exists(file.path(td, "population_Nei_neighbor_joining.nwk")))

  two_pop <- res$distance[1:2, 1:2]
  td2 <- tempfile("trees-two-"); dir.create(td2)
  expect_null(popgenVCF:::build_population_tree(two_pop, list(trees = td2)))
  expect_false(file.exists(file.path(td2, "population_Nei_neighbor_joining.nwk")))
})

test_that("validate_population_tree_result accepts a well-formed result and flags asymmetric/negative bugs", {
  fx <- population_tree_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_population_genetic_distance(fx$div$locus)
  ok <- popgenVCF:::validate_population_tree_result(res, NULL, NULL)
  expect_true(ok$valid)

  bad_diag <- res; bad_diag$distance <- res$distance; diag(bad_diag$distance) <- 1
  expect_false(popgenVCF:::validate_population_tree_result(bad_diag, NULL, NULL)$valid)

  bad_negative <- res; bad_negative$distance <- res$distance; bad_negative$distance[1, 2] <- -1
  expect_false(popgenVCF:::validate_population_tree_result(bad_negative, NULL, NULL)$valid)

  incomplete <- list(distance = res$distance)
  expect_false(popgenVCF:::validate_population_tree_result(incomplete, NULL, NULL)$valid)
})

test_that("population_tree_module_spec is registered, requires diversity, and is enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("population_tree" %in% names(registry$modules))
  module <- registry$modules$population_tree
  expect_identical(module$requires, "diversity")
  cfg <- popgenVCF::default_config()
  expect_true(popgenVCF:::module_is_enabled(module, cfg))
  cfg$analyses$population_tree <- FALSE
  expect_false(popgenVCF:::module_is_enabled(module, cfg))
})
