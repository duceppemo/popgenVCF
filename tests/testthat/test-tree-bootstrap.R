tree_bootstrap_genotype_fixture <- function() {
  set.seed(11)
  n_pop <- 3L; n_per_pop <- 8L; n_snp <- 40L
  pops <- rep(c("A", "B", "C"), each = n_per_pop)
  base_freqs <- c(A = 0.15, B = 0.5, C = 0.85)
  sample_id <- paste0("S", seq_along(pops))
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
  metadata <- data.table::data.table(sample = sample_id, population = pops)
  list(gds = gds, sample_ids = sample_id, snp_ids = seq_len(n_snp), metadata = metadata)
}

test_that("bootstrap_tree_support recovers known bipartition support exactly", {
  ref <- ape::read.tree(text = "(((A,B),C),D);")
  # 3 replicates agree with the (A,B) split, 2 do not (D placed with A,B
  # instead of outgroup) -- hand-verifiable expected support: (A,B) = 100%,
  # nothing else appears in every replicate.
  agree <- ape::read.tree(text = "(((A,B),C),D);")
  disagree <- ape::read.tree(text = "(((A,D),C),B);")
  replicates <- list(agree, agree, agree, disagree, disagree)
  support <- popgenVCF:::bootstrap_tree_support(ref, replicates)
  expect_type(support, "integer")
  expect_identical(length(support), ref$Nnode)
  expect_true(100L %in% support)
  expect_true(60L %in% support)
})

test_that("bootstrap_tree_support returns NULL with fewer than two valid replicate trees", {
  ref <- ape::read.tree(text = "(((A,B),C),D);")
  expect_null(popgenVCF:::bootstrap_tree_support(ref, list()))
  expect_null(popgenVCF:::bootstrap_tree_support(ref, list(ref)))
  expect_null(popgenVCF:::bootstrap_tree_support(ref, list(simpleError("x"))))
})

test_that("tree_bootstrap_replicate_seeds is deterministic and restores the caller's RNG state", {
  a <- popgenVCF:::tree_bootstrap_replicate_seeds(42L, 10L)
  b <- popgenVCF:::tree_bootstrap_replicate_seeds(42L, 10L)
  expect_identical(a, b)
  expect_length(a, 10L)

  set.seed(123)
  before <- .Random.seed
  popgenVCF:::tree_bootstrap_replicate_seeds(999L, 5L)
  after <- .Random.seed
  expect_identical(before, after)
})

test_that("run_tree_bootstrap_replicates drops non-phylo results (sequential path)", {
  # Real regression: build_one() always wraps its own body in
  # tryCatch(..., error = function(e) e), so a genuine numerical failure
  # (e.g. ape::nj() erroring on a degenerate resampled distance matrix)
  # comes back as a plain, unclassed-as-"try-error" error condition, not
  # "try-error" -- the old `!inherits(x, "try-error")` filter never removed
  # it, and the sequential path (workers <= 1) did not filter AT ALL,
  # letting a caller's `length(trees)` always equal the requested replicate
  # count regardless of real failures.
  phylo_stub <- function() structure(list(), class = "phylo")
  build_one <- function(i) if (i == 2L) simpleError("boom") else phylo_stub()
  sequential <- popgenVCF:::run_tree_bootstrap_replicates(3L, 1L, build_one)
  expect_length(sequential, 2L)
  expect_true(all(vapply(sequential, inherits, logical(1L), what = "phylo")))
})

test_that("run_tree_bootstrap_replicates drops non-phylo results, including a killed worker's NULL (parallel path)", {
  phylo_stub <- function() structure(list(), class = "phylo")
  build_one <- function(i) {
    if (i == 2L) return(simpleError("boom"))
    if (i == 4L) return(NULL)
    phylo_stub()
  }
  parallel_result <- popgenVCF:::run_tree_bootstrap_replicates(4L, 2L, build_one)
  expect_length(parallel_result, 2L)
  expect_true(all(vapply(parallel_result, inherits, logical(1L), what = "phylo")))
})

test_that("ibs_bootstrap_distance matches SNPRelate::snpgdsIBS()'s exact formula on complete data", {
  skip_if_not_installed("SNPRelate")
  fx <- tree_bootstrap_genotype_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  z <- SNPRelate::snpgdsIBS(
    fx$gds, sample.id = fx$sample_ids, snp.id = fx$snp_ids,
    autosome.only = FALSE, remove.monosnp = FALSE, maf = NaN, missing.rate = NaN, verbose = FALSE
  )
  geno <- SNPRelate::snpgdsGetGeno(fx$gds, sample.id = fx$sample_ids, snp.id = z$snp.id, verbose = FALSE)
  expect_false(anyNA(geno))
  d_mine <- as.matrix(popgenVCF:::ibs_bootstrap_distance(geno))
  d_snprelate <- 1 - z$ibs
  expect_equal(d_mine, d_snprelate, tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("ibs_bootstrap_distance mean-imputes missing genotypes per locus", {
  # 2 samples, 1 locus: sample 2 is missing. Mean-imputed value is sample 1's
  # own dosage (the only other observation), so the imputed distance is 0 --
  # hand-verifiable, not just internally self-consistent.
  geno <- matrix(c(1L, NA_integer_), nrow = 2L, ncol = 1L)
  d <- popgenVCF:::ibs_bootstrap_distance(geno)
  expect_equal(as.numeric(d), 0, tolerance = 1e-10)
})

test_that("bootstrap_nj_ibs_tree returns NULL for zero replicates or fewer than two SNPs", {
  fx <- tree_bootstrap_genotype_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ref <- ape::read.tree(text = "(A,B,C);")
  expect_null(popgenVCF:::bootstrap_nj_ibs_tree(ref, fx$gds, fx$sample_ids, fx$snp_ids, fx$metadata, 0L, 1L, 1L))
  expect_null(popgenVCF:::bootstrap_nj_ibs_tree(ref, fx$gds, fx$sample_ids, 1L, fx$metadata, 10L, 1L, 1L))
})

test_that("build_nj_tree computes bootstrap support end to end and is deterministic for a fixed seed", {
  fx <- tree_bootstrap_genotype_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ibs <- popgenVCF:::run_ibs(fx$gds, fx$sample_ids, fx$snp_ids, fx$metadata, 1L)
  cfg <- popgenVCF::default_config()
  cfg$analyses$tree_bootstrap$enabled <- TRUE
  cfg$analyses$tree_bootstrap$replicates <- 30L
  cfg$compute$seed <- 7L
  # R CMD check enforces CRAN's 2-core policy on parallel::mclapply();
  # cfg$compute$threads defaults to the machine's full auto-detected count
  # (unrelated to this test's own purpose), which throws "N simultaneous
  # processes spawned" under that check -- confirmed directly in CI, not a
  # theoretical concern. Every test that reaches run_tree_bootstrap_replicates()
  # must cap threads at or below 2 for exactly this reason.
  cfg$compute$threads <- 2L
  td <- tempfile("trees-"); dir.create(td)

  tree <- popgenVCF:::build_nj_tree(ibs, fx$metadata, cfg, list(trees = td),
                                    gds = fx$gds, sample_ids = fx$sample_ids, snp_ids = fx$snp_ids)
  expect_s3_class(tree, "phylo")
  expect_identical(attr(tree, "bootstrap_replicates"), 30L)
  expect_length(tree$node.label, tree$Nnode)
  support <- as.numeric(tree$node.label)
  expect_false(anyNA(support))
  expect_true(all(support >= 0 & support <= 100))

  tree2 <- popgenVCF:::build_nj_tree(ibs, fx$metadata, cfg, list(trees = td),
                                     gds = fx$gds, sample_ids = fx$sample_ids, snp_ids = fx$snp_ids)
  expect_identical(tree$node.label, tree2$node.label)
})

test_that("build_nj_tree leaves node.label NULL and bootstrap_replicates 0 when disabled", {
  fx <- tree_bootstrap_genotype_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ibs <- popgenVCF:::run_ibs(fx$gds, fx$sample_ids, fx$snp_ids, fx$metadata, 1L)
  cfg <- popgenVCF::default_config()
  cfg$analyses$tree_bootstrap$enabled <- FALSE
  td <- tempfile("trees-"); dir.create(td)
  tree <- popgenVCF:::build_nj_tree(ibs, fx$metadata, cfg, list(trees = td),
                                    gds = fx$gds, sample_ids = fx$sample_ids, snp_ids = fx$snp_ids)
  expect_null(tree$node.label)
  expect_identical(attr(tree, "bootstrap_replicates"), 0L)
})

test_that("population_allele_count_matrix resampled columns give bootstrap distances matching an explicit re-derivation", {
  lt <- data.table::data.table(
    population = rep(c("A", "B", "C"), each = 2L),
    snp_id = rep(c(1L, 2L), 3L),
    n_called = rep(20L, 6L),
    alternate_allele_count = c(30L, 15L, 15L, 8L, 5L, 25L),
    reference_allele_count = c(10L, 5L, 25L, 12L, 15L, 5L)
  )
  built <- popgenVCF:::population_allele_count_matrix(lt)
  expect_identical(built$n_snps, 2L)
  expect_identical(dim(built$tab), c(3L, 4L))

  # Resample locus 1 twice, locus 2 zero times -- distance from the resampled
  # tab must equal the distance computed by literally duplicating locus 1's
  # two columns in a freshly-built matrix (an independent re-derivation, not
  # just checking the code runs).
  resampled <- built$tab[, c(1L, 2L, 1L, 2L)]
  d_via_helper <- popgenVCF:::population_genpop_distance(resampled)
  manual <- built$tab[, c(1L, 2L, 1L, 2L)]
  colnames(manual) <- c("snp1_a.0", "snp1_a.1", "snp1_b.0", "snp1_b.1")
  gp <- methods::new("genpop", tab = manual, ploidy = 2L)
  d_manual <- as.matrix(adegenet::dist.genpop(gp, method = 1L))
  expect_equal(d_via_helper, d_manual, ignore_attr = TRUE)
})

test_that("bootstrap_population_nj_tree's reported replicate count reflects real successes, not the requested count", {
  lt <- data.table::data.table(
    population = rep(c("A", "B", "C"), each = 2L),
    snp_id = rep(c(1L, 2L), 3L),
    n_called = rep(20L, 6L),
    alternate_allele_count = c(30L, 15L, 15L, 8L, 5L, 25L),
    reference_allele_count = c(10L, 5L, 25L, 12L, 15L, 5L)
  )
  res <- popgenVCF:::compute_population_genetic_distance(lt)
  ref <- ape::nj(stats::as.dist(res$distance))

  # Real regression: ape::nj() failing for some replicates (a genuinely
  # degenerate resampled distance matrix is the real-world trigger) used to
  # still leave `replicates` at the full requested count, since build_one()'s
  # own tryCatch() turns that failure into a plain error condition the old
  # `!inherits(x, "try-error")` filter never removed. Forcing every third
  # call to fail deterministically (sequential path, so a single counter is
  # safe across calls) proves the reported count now equals only the real
  # successes.
  original_nj <- ape::nj
  call_n <- 0L
  local_mocked_bindings(nj = function(...) {
    call_n <<- call_n + 1L
    if (call_n %% 3L == 0L) stop("simulated degenerate distance matrix")
    original_nj(...)
  }, .package = "ape")

  result <- popgenVCF:::bootstrap_population_nj_tree(ref, lt, replicates = 9L, workers = 1L, seed = 5L)
  expect_identical(result$replicates, 6L)
})

test_that("bootstrap_population_nj_tree returns NULL for zero replicates or too few usable loci", {
  lt <- data.table::data.table(
    population = rep(c("A", "B", "C"), each = 2L),
    snp_id = rep(c(1L, 2L), 3L),
    n_called = rep(20L, 6L),
    alternate_allele_count = c(30L, 15L, 15L, 8L, 5L, 25L),
    reference_allele_count = c(10L, 5L, 25L, 12L, 15L, 5L)
  )
  res <- popgenVCF:::compute_population_genetic_distance(lt)
  ref <- ape::nj(stats::as.dist(res$distance))
  expect_null(popgenVCF:::bootstrap_population_nj_tree(ref, lt, 0L, 1L, 1L))

  one_snp <- lt[snp_id == 1L]
  expect_null(popgenVCF:::bootstrap_population_nj_tree(ref, one_snp, 10L, 1L, 1L))
})

test_that("build_population_tree computes bootstrap support end to end when enabled", {
  set.seed(1)
  n_pop <- 4L; n_per_pop <- 15L; n_snp <- 60L
  pops <- rep(LETTERS[1:n_pop], each = n_per_pop)
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
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- data.table::data.table(sample = sample_id, population = pops)
  div <- popgenVCF:::compute_diversity(gds, sample_id, ids$snp, metadata, ids)
  res <- popgenVCF:::compute_population_genetic_distance(div$locus)

  cfg <- popgenVCF::default_config()
  cfg$analyses$tree_bootstrap$enabled <- TRUE
  cfg$analyses$tree_bootstrap$replicates <- 25L
  cfg$compute$seed <- 3L
  cfg$compute$threads <- 2L # see the identical rationale above this function's sibling test
  td <- tempfile("trees-"); dir.create(td)
  tree <- popgenVCF:::build_population_tree(res$distance, list(trees = td), cfg, div$locus)
  expect_s3_class(tree, "phylo")
  expect_identical(attr(tree, "bootstrap_replicates"), 25L)
  support <- as.numeric(tree$node.label)
  expect_false(anyNA(support))
  expect_true(all(support >= 0 & support <= 100))
  # The frequencies are set up so A/B/C/D form a monotonic chain (see the
  # fixture above); the deepest split should be well supported.
  expect_true(max(support) >= 90)
})

test_that("plot_nj_tree writes figure files and no-ops for NULL or under-sized trees", {
  cfg <- popgenVCF::default_config()
  cfg$output$figure_formats <- c("png")
  td <- tempfile("figs-"); dir.create(td)
  dirs <- list(figures = td)

  expect_null(popgenVCF:::plot_nj_tree(NULL, NULL, cfg, dirs, "test_tree", "Title"))
  two_tip <- ape::read.tree(text = "(A,B);")
  expect_null(popgenVCF:::plot_nj_tree(two_tip, NULL, cfg, dirs, "test_tree_small", "Title"))
  expect_false(file.exists(file.path(td, "test_tree_small.png")))

  tree <- ape::read.tree(text = "(((A,B),C),D);")
  tree$edge.length <- rep(0.1, nrow(tree$edge))
  attr(tree, "bootstrap_replicates") <- 50L
  tree$node.label <- c("100", "80", "60")
  popgenVCF:::plot_nj_tree(tree, NULL, cfg, dirs, "test_tree", "A tree")
  expect_true(file.exists(file.path(td, "test_tree.png")))
})

test_that("tree_bootstrap_support_errors validates node.label percentages when bootstrap was computed", {
  tree <- ape::read.tree(text = "(((A,B),C),D);")
  attr(tree, "bootstrap_replicates") <- 0L
  expect_identical(popgenVCF:::tree_bootstrap_support_errors(tree), character())

  attr(tree, "bootstrap_replicates") <- 10L
  expect_match(popgenVCF:::tree_bootstrap_support_errors(tree), "match the internal node count")

  tree$node.label <- c("100", "150", "-5")
  expect_match(popgenVCF:::tree_bootstrap_support_errors(tree), "between 0 and 100")

  tree$node.label <- c("100", "80", "0")
  expect_identical(popgenVCF:::tree_bootstrap_support_errors(tree), character())
})

test_that("validate_tree_result and validate_population_tree_result flag bad bootstrap support", {
  tree <- ape::read.tree(text = "(((A,B),C),D);")
  attr(tree, "bootstrap_replicates") <- 5L
  tree$node.label <- c("200", "80", "0")
  analysis <- list(samples = list(ids = tree$tip.label))
  result <- popgenVCF:::validate_tree_result(tree, analysis, NULL)
  expect_false(result$valid)

  pop_result <- list(
    distance = matrix(c(0, 1, 1, 0), 2, dimnames = list(c("A", "B"), c("A", "B"))),
    n_snps = 5L, populations = c("A", "B"), tree = tree
  )
  pop_check <- popgenVCF:::validate_population_tree_result(pop_result, NULL, NULL)
  expect_false(pop_check$valid)
})

test_that("tree_bootstrap config validates replicates and defaults to enabled with 100 replicates", {
  cfg <- popgenVCF::default_config()
  expect_true(cfg$analyses$tree_bootstrap$enabled)
  expect_identical(cfg$analyses$tree_bootstrap$replicates, 100L)

  cfg$input$vcf <- popgenVCF::quickstart_dataset_paths()$vcf
  cfg$output$directory <- tempfile("cfgtest-")
  cfg$analyses$tree_bootstrap$replicates <- -1L
  expect_error(popgenVCF:::validate_config(cfg), "tree_bootstrap.replicates")
})
