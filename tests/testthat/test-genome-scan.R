test_that("genome_scan_windows generates deterministic, hand-verified window boundaries", {
  chromosome <- c("1", "1", "1", "2")
  position <- c(100, 5000, 12000, 200)
  out <- popgenVCF:::genome_scan_windows(chromosome, position, window_bp = 5000, step_bp = 5000)

  expect_setequal(names(out), c("chromosome", "window_start", "window_end"))
  chr1 <- out[chromosome == "1"]
  expect_identical(chr1$window_start, c(100L, 5100L, 10100L))
  expect_identical(chr1$window_end, c(5099L, 10099L, 15099L))
  chr2 <- out[chromosome == "2"]
  expect_identical(chr2$window_start, 200L)
  expect_identical(chr2$window_end, 5199L)
})

test_that("genome_scan_windows orders chromosome blocks naturally (chr2 before chr10)", {
  chromosome <- c("10", "2", "1")
  position <- c(100, 100, 100)
  out <- popgenVCF:::genome_scan_windows(chromosome, position, window_bp = 5000, step_bp = 5000)
  expect_identical(rle(out$chromosome)$values, c("1", "2", "10"))
})

test_that("genome_scan_windows supports a real overlapping/sliding step smaller than the window", {
  out <- popgenVCF:::genome_scan_windows(rep("1", 3), c(0, 1000, 2000), window_bp = 1000, step_bp = 500)
  expect_identical(out$window_start, c(0L, 500L, 1000L, 1500L, 2000L))
  expect_identical(out$window_end, out$window_start + 999L)
})

test_that("run_genome_scan_diversity aggregates the locus table into hand-verified per-window means", {
  # Two loci in window [0,999], one in [1000,1999]; PopA's window-1 mean is
  # the hand-computable average of 0.2/0.4 (Ho) and 0.1/0.3 (He).
  locus <- data.table::data.table(
    population = c("PopA", "PopA", "PopA", "PopB", "PopB"),
    chromosome = c("1", "1", "1", "1", "1"),
    position = c(100L, 900L, 1500L, 100L, 1500L),
    observed_heterozygosity = c(0.2, 0.4, 0.6, 0.5, 0.5),
    unbiased_expected_heterozygosity = c(0.1, 0.3, 0.5, 0.4, 0.4)
  )
  out <- popgenVCF:::run_genome_scan_diversity(locus, window_bp = 1000, step_bp = 1000, min_snps = 1L)

  # Windows align to the first retained position (100), not a round bp
  # boundary (confirmed by the genome_scan_windows() test above): window 1
  # is [100, 1099], window 2 is [1100, 2099].
  win1_a <- out[chromosome == "1" & window_start == 100L & population == "PopA"]
  expect_identical(win1_a$n_snps, 2L)
  expect_equal(win1_a$mean_observed_heterozygosity, mean(c(0.2, 0.4)))
  expect_equal(win1_a$mean_expected_heterozygosity, mean(c(0.1, 0.3)))

  win2_a <- out[chromosome == "1" & window_start == 1100L & population == "PopA"]
  expect_identical(win2_a$n_snps, 1L)
  expect_equal(win2_a$mean_observed_heterozygosity, 0.6)

  win1_b <- out[chromosome == "1" & window_start == 100L & population == "PopB"]
  expect_identical(win1_b$n_snps, 1L)
  expect_equal(win1_b$mean_observed_heterozygosity, 0.5)
})

test_that("run_genome_scan_diversity orders chromosome blocks naturally (chr2 before chr10)", {
  locus <- data.table::data.table(
    population = c("PopA", "PopA", "PopA"),
    chromosome = c("10", "2", "1"),
    position = c(100L, 100L, 100L),
    observed_heterozygosity = c(0.2, 0.3, 0.4),
    unbiased_expected_heterozygosity = c(0.1, 0.2, 0.3)
  )
  out <- popgenVCF:::run_genome_scan_diversity(locus, window_bp = 1000, step_bp = 1000, min_snps = 1L)
  expect_identical(rle(out$chromosome)$values, c("1", "2", "10"))
})

test_that("run_genome_scan_diversity reports NA (not a misleading value) below min_snps", {
  locus <- data.table::data.table(
    population = "PopA", chromosome = "1", position = 100L,
    observed_heterozygosity = 0.3, unbiased_expected_heterozygosity = 0.25
  )
  out <- popgenVCF:::run_genome_scan_diversity(locus, window_bp = 1000, step_bp = 1000, min_snps = 5L)
  expect_identical(out$n_snps, 1L)
  expect_true(is.na(out$mean_observed_heterozygosity))
  expect_true(is.na(out$mean_expected_heterozygosity))
})

test_that("run_genome_scan_fst produces real, plausible windowed values on the real quickstart dataset", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  paths <- popgenVCF::quickstart_dataset_paths()
  gds <- popgenVCF:::prepare_gds(paths$vcf, tempfile(fileext = ".gds"))
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  sample_ids <- as.character(ids$sample)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::fread(paths$metadata))
  vq <- popgenVCF:::variant_qc(gds, sample_ids, ids, 0.05, 0.20)
  qc_snps <- vq[pass_combined == TRUE, snp_id]

  out <- popgenVCF:::run_genome_scan_fst(gds, qc_snps, ids, metadata, window_bp = 50000, step_bp = 50000, min_snps = 5L)
  expect_setequal(names(out), c("chromosome", "window_start", "window_end", "n_snps", "global_fst"))
  expect_gt(nrow(out), 10L)
  finite <- out[is.finite(global_fst)]
  expect_gt(nrow(finite), 0L)
  # Real region-wide global FST (computed elsewhere this session on the same
  # dataset) is 0.0915; per-window estimates vary but should stay in a
  # plausible range around it, not be wildly out of bounds.
  expect_true(all(finite$global_fst > -0.5 & finite$global_fst < 1))
  expect_true(all(out$n_snps[!is.finite(out$global_fst)] < 5L))
})

test_that("plot_genome_scan draws both Manhattan-style figures with correct titles", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  fst_windows <- data.table::data.table(
    chromosome = c("1", "1", "2"), window_start = c(0L, 1000L, 0L),
    window_end = c(999L, 1999L, 999L), n_snps = c(10L, 12L, 8L),
    global_fst = c(0.05, 0.12, NA_real_)
  )
  diversity_windows <- data.table::data.table(
    chromosome = c("1", "1"), window_start = c(0L, 0L), window_end = c(999L, 999L),
    population = c("PopA", "PopB"), n_snps = c(10L, 10L),
    mean_observed_heterozygosity = c(0.2, 0.3), mean_expected_heterozygosity = c(0.25, 0.28),
    segregating_sites = c(6L, 7L), tajima_d = c(0.5, -0.3)
  )
  popgenVCF:::plot_genome_scan(fst_windows, diversity_windows, cfg, dirs)

  expect_true("25_genome_scan_FST_manhattan" %in% names(plots))
  expect_true("26_genome_scan_diversity_manhattan" %in% names(plots))
  expect_true("26b_genome_scan_tajima_d_manhattan" %in% names(plots))
  expect_identical(plots[["25_genome_scan_FST_manhattan"]]$labels$title, "Sliding-window FST scan")
  expect_identical(plots[["26_genome_scan_diversity_manhattan"]]$labels$title, "Sliding-window diversity scan")
  expect_identical(plots[["26b_genome_scan_tajima_d_manhattan"]]$labels$title, "Sliding-window Tajima's D scan")
})

test_that("plot_genome_scan skips a figure entirely when its windows are all non-finite", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  fst_windows <- data.table::data.table(
    chromosome = "1", window_start = 0L, window_end = 999L, n_snps = 1L, global_fst = NA_real_
  )
  diversity_windows <- data.table::data.table(
    chromosome = "1", window_start = 0L, window_end = 999L, population = "PopA",
    n_snps = 1L, mean_observed_heterozygosity = NA_real_, mean_expected_heterozygosity = NA_real_,
    segregating_sites = NA_integer_, tajima_d = NA_real_
  )
  popgenVCF:::plot_genome_scan(fst_windows, diversity_windows, cfg, dirs)
  expect_length(plots, 0L)
})

test_that("genome scan module descriptor owns the complete registry contract", {
  spec <- popgenVCF::genome_scan_module_spec()
  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "genome_scan")
  expect_identical(spec$requires, "diversity")
  expect_identical(spec$outputs, "genome_scan")
  expect_identical(spec$references, c("Weir and Cockerham 1984", "Tajima 1989"))
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$contract_version, "1.0")
  expect_identical(spec$run, popgenVCF:::run_module_genome_scan)
  expect_identical(spec$validate, popgenVCF:::validate_genome_scan_result)
})

test_that("built-in registry reflects the genome scan module descriptor", {
  spec <- popgenVCF::genome_scan_module_spec()
  registry <- popgenVCF::default_analysis_registry()
  module <- registry$modules$genome_scan
  expect_identical(module$name, spec$name)
  expect_identical(module$requires, spec$requires)
  expect_identical(module$run, spec$run)
  expect_identical(module$validate, spec$validate)
})

test_that("validate_genome_scan_result flags missing components and warns on out-of-range FST", {
  ok <- list(
    fst_windows = data.table::data.table(global_fst = 0.1),
    diversity_windows = data.table::data.table(mean_observed_heterozygosity = 0.2),
    outliers = data.table::data.table()
  )
  v <- popgenVCF:::validate_genome_scan_result(ok, NULL, NULL)
  expect_true(v$valid)
  expect_length(v$warnings, 0L)

  incomplete <- list(fst_windows = data.table::data.table())
  expect_false(popgenVCF:::validate_genome_scan_result(incomplete, NULL, NULL)$valid)

  out_of_range <- ok
  out_of_range$fst_windows$global_fst <- 1.5
  v2 <- popgenVCF:::validate_genome_scan_result(out_of_range, NULL, NULL)
  expect_true(v2$valid)
  expect_true(any(grepl("exceed one", v2$warnings)))
})

test_that("genome_scan is gated by population capability like fst", {
  registry <- popgenVCF::default_analysis_registry()
  no_population <- popgenVCF:::analysis_capability_table(
    registry, list(population = FALSE, metadata_supplied = TRUE, coordinates = FALSE)
  )
  expect_false(no_population[module == "genome_scan", available])

  single_population <- popgenVCF:::analysis_capability_table(
    registry, list(population = TRUE, population_levels = 1L, metadata_supplied = TRUE, coordinates = FALSE)
  )
  expect_false(single_population[module == "genome_scan", available])

  two_populations <- popgenVCF:::analysis_capability_table(
    registry, list(population = TRUE, population_levels = 2L, metadata_supplied = TRUE, coordinates = FALSE)
  )
  expect_true(two_populations[module == "genome_scan", available])
})

test_that("tajima_d_statistic matches a published worked example exactly", {
  # n=10 sequences, S=16 segregating sites, pi=3.888889 -> D=-1.446172
  # (independently verified against a secondary source before implementing,
  # not just recalled -- see NEWS.md).
  d <- popgenVCF:::tajima_d_statistic(pi = 3.888889, s = 16L, n = 10L)
  expect_equal(d, -1.446172, tolerance = 1e-6)
})

test_that("tajima_d_constants match the published worked example's intermediate values", {
  k <- popgenVCF:::tajima_d_constants(10L)
  expect_equal(k$a1, 2.828968, tolerance = 1e-6)
  expect_equal(k$e1, 0.01906053, tolerance = 1e-6)
  expect_equal(k$e2, 0.004948928, tolerance = 1e-6)
})

test_that("tajima_d_statistic returns NA for undefined or degenerate inputs", {
  expect_true(is.na(popgenVCF:::tajima_d_statistic(pi = 1, s = 0L, n = 10L)))
  expect_true(is.na(popgenVCF:::tajima_d_statistic(pi = 1, s = 5L, n = NA_real_)))
  expect_true(is.na(popgenVCF:::tajima_d_statistic(pi = 1, s = 5L, n = 1L)))
})

test_that("run_genome_scan_diversity computes hand-verified segregating_sites and tajima_d", {
  # Window has 3 loci for PopA: 2 polymorphic (He 0.3, 0.5), 1 monomorphic
  # (He 0, not segregating). pi is the SUM of unbiased He across all loci in
  # the window (the same units theta_W is estimated in), not the mean.
  locus <- data.table::data.table(
    population = c("PopA", "PopA", "PopA"),
    chromosome = c("1", "1", "1"),
    position = c(100L, 200L, 300L),
    observed_heterozygosity = c(0.2, 0.4, 0),
    unbiased_expected_heterozygosity = c(0.3, 0.5, 0),
    polymorphic = c(TRUE, TRUE, FALSE)
  )
  population_n <- c(PopA = 20L)
  out <- popgenVCF:::run_genome_scan_diversity(
    locus, window_bp = 1000, step_bp = 1000, min_snps = 1L, population_n = population_n
  )
  expect_identical(out$segregating_sites, 2L)
  expected_d <- popgenVCF:::tajima_d_statistic(pi = 0.8, s = 2L, n = 40L)
  expect_equal(out$tajima_d, expected_d)
  expect_true(is.finite(out$tajima_d))
})

test_that("run_genome_scan_diversity reports NA tajima_d when population_n is unavailable", {
  locus <- data.table::data.table(
    population = "PopA", chromosome = "1", position = 100L,
    observed_heterozygosity = 0.3, unbiased_expected_heterozygosity = 0.25,
    polymorphic = TRUE
  )
  out <- popgenVCF:::run_genome_scan_diversity(locus, window_bp = 1000, step_bp = 1000, min_snps = 1L)
  expect_true(is.na(out$tajima_d))
})
