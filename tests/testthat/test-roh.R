# A small, purpose-built plain-text VCF (bcftools reads unindexed plain VCF
# fine, confirmed empirically) with real, verifiable ground truth: one sample
# homozygous-ref across every SNP (expect one run spanning close to the full
# analyzed span), one heterozygous-throughout sample (expect none), plus two
# "anchor" samples (one heterozygous, one homozygous-alt, both throughout)
# that guarantee every site stays polymorphic across the panel so nothing
# gets dropped as monomorphic during self-estimated-AF filtering.
roh_fixture_vcf <- function() {
  n <- 40L
  positions <- seq(100L, by = 100L, length.out = n)
  het <- rep("0/1", n)
  hom_ref <- rep("0/0", n)
  hom_alt <- rep("1/1", n)
  lines <- c(
    "##fileformat=VCFv4.2",
    "##contig=<ID=1,length=4100>",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tTARGET_HET\tTARGET_HOM\tANCHOR_HET\tANCHOR_ALT"
  )
  for (i in seq_len(n)) {
    lines <- c(lines, sprintf(
      "1\t%d\tv%d\tA\tG\t60\tPASS\t.\tGT\t%s\t%s\t%s\t%s",
      positions[i], i, het[i], hom_ref[i], het[i], hom_alt[i]
    ))
  }
  path <- tempfile(fileext = ".vcf")
  writeLines(lines, path)
  path
}

test_that("roh_parse_regions extracts only RG rows from mixed comment/info/log output", {
  lines <- c(
    "Number of target samples: 4",
    "# This file was produced by: bcftools roh(1.24+htslib-1.24)",
    "#",
    "# RG\t[2]Sample\t[3]Chromosome\t[4]Start\t[5]End\t[6]Length (bp)\t[7]Number of markers\t[8]Quality",
    "RG\tS1\t1\t100\t4000\t3901\t40\t83.2",
    "RG\tS2\t2\t500\t900\t401\t5\t12.5"
  )
  out <- popgenVCF:::roh_parse_regions(lines)
  expect_identical(nrow(out), 2L)
  expect_setequal(names(out), c("sample", "chromosome", "start", "end", "length_bp", "n_markers", "quality"))
  expect_identical(out$sample, c("S1", "S2"))
  expect_identical(out$length_bp, c(3901L, 401L))
  expect_equal(out$quality, c(83.2, 12.5))
})

test_that("roh_parse_regions returns a correctly-typed empty table when no runs are found", {
  out <- popgenVCF:::roh_parse_regions(c("Number of target samples: 2", "# comment"))
  expect_identical(nrow(out), 0L)
  expect_setequal(names(out), c("sample", "chromosome", "start", "end", "length_bp", "n_markers", "quality"))
})

test_that("run_roh recovers a known full-length homozygous run and a known absence of one", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  vcf <- roh_fixture_vcf()
  sample_ids <- c("TARGET_HET", "TARGET_HOM", "ANCHOR_HET", "ANCHOR_ALT")
  metadata <- popgenVCF:::metadata_from_samples(sample_ids)

  result <- popgenVCF:::run_roh(vcf, sample_ids, metadata, 0.2, 30, 1L)
  expect_named(result, c("runs", "sample_summary", "analyzed_footprint_bp"))
  expect_equal(result$analyzed_footprint_bp, 3901)

  het_runs <- result$runs[sample %in% c("TARGET_HET", "ANCHOR_HET")]
  expect_identical(nrow(het_runs), 0L)

  hom_runs <- result$runs[sample == "TARGET_HOM"]
  expect_identical(nrow(hom_runs), 1L)
  expect_identical(hom_runs$start, 100L)
  expect_identical(hom_runs$end, 4000L)
  expect_identical(hom_runs$n_markers, 40L)

  summary <- result$sample_summary
  expect_setequal(summary$sample, sample_ids)
  target_hom <- summary[sample == "TARGET_HOM"]
  expect_identical(target_hom$n_runs, 1L)
  expect_equal(target_hom$froh, 1, tolerance = 1e-6)
  target_het <- summary[sample == "TARGET_HET"]
  expect_identical(target_het$n_runs, 0L)
  expect_equal(target_het$froh, 0)
})

test_that("run_roh reports a sample with zero runs correctly rather than omitting it", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  vcf <- roh_fixture_vcf()
  sample_ids <- c("TARGET_HET", "TARGET_HOM", "ANCHOR_HET", "ANCHOR_ALT")
  metadata <- popgenVCF:::metadata_from_samples(sample_ids)
  result <- popgenVCF:::run_roh(vcf, sample_ids, metadata, 0.2, 30, 1L)
  expect_identical(nrow(result$sample_summary), 4L)
  expect_true(all(is.finite(result$sample_summary$froh)))
})

test_that("run_roh joins population labels when metadata provides them, and omits the column otherwise", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  vcf <- roh_fixture_vcf()
  sample_ids <- c("TARGET_HET", "TARGET_HOM", "ANCHOR_HET", "ANCHOR_ALT")

  with_pop <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_ids, population = c("A", "A", "B", "B")
  ))
  result <- popgenVCF:::run_roh(vcf, sample_ids, with_pop, 0.2, 30, 1L)
  expect_true("population" %in% names(result$sample_summary))
  expect_identical(result$sample_summary[sample == "TARGET_HOM", population], "A")

  without_pop <- popgenVCF:::metadata_from_samples(sample_ids)
  result2 <- popgenVCF:::run_roh(vcf, sample_ids, without_pop, 0.2, 30, 1L)
  expect_false("population" %in% names(result2$sample_summary))
  expect_false("population" %in% names(result2$runs))
})

test_that("run_roh executes on the tiny bundled CI validation fixture without crashing", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  paths <- popgenVCF:::validation_fixture_paths()
  sample_ids <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")
  metadata <- popgenVCF:::metadata_from_samples(sample_ids)
  # Values are not scientifically meaningful at 9 SNPs (established this
  # session for HWE and kinship alike); only structural correctness matters.
  result <- popgenVCF:::run_roh(paths$vcf, sample_ids, metadata, 0.2, 30, 1L)
  expect_setequal(names(result$runs), c("sample", "chromosome", "start", "end", "length_bp", "n_markers", "quality"))
  expect_identical(nrow(result$sample_summary), 8L)
  expect_true(all(result$sample_summary$froh >= 0 & result$sample_summary$froh <= 1 + 1e-6))
})

test_that("plot_roh draws the length histogram and FROH-by-sample plot with correct titles", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  result <- list(
    runs = data.table::data.table(
      sample = c("s1", "s1", "s2"), population = c("A", "A", "B"),
      chromosome = "1", start = c(100, 500, 100), end = c(400, 900, 300),
      length_bp = c(300, 400, 200), n_markers = c(5, 6, 4), quality = c(50, 60, 40)
    ),
    sample_summary = data.table::data.table(
      sample = c("s1", "s2", "s3"), population = c("A", "B", "A"),
      n_runs = c(2L, 1L, 0L), total_length_bp = c(700, 200, 0),
      mean_length_bp = c(350, 200, NA), longest_run_bp = c(400L, 200L, 0L),
      froh = c(0.7, 0.2, 0)
    ),
    analyzed_footprint_bp = 1000
  )
  popgenVCF:::plot_roh(result, cfg, dirs)
  expect_true("23_ROH_length_distribution" %in% names(plots))
  expect_true("24_ROH_FROH_by_sample" %in% names(plots))
  expect_identical(plots[["23_ROH_length_distribution"]]$labels$title, "Runs of homozygosity: length distribution")
})

test_that("plot_roh skips the length histogram but still draws FROH-by-sample when no runs exist", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  result <- list(
    runs = data.table::data.table(
      sample = character(), population = character(), chromosome = character(),
      start = integer(), end = integer(), length_bp = integer(),
      n_markers = integer(), quality = numeric()
    ),
    sample_summary = data.table::data.table(
      sample = c("s1", "s2"), n_runs = c(0L, 0L), total_length_bp = c(0, 0),
      mean_length_bp = c(NA, NA), longest_run_bp = c(0L, 0L), froh = c(0, 0)
    ),
    analyzed_footprint_bp = 1000
  )
  popgenVCF:::plot_roh(result, cfg, dirs)
  expect_false("23_ROH_length_distribution" %in% names(plots))
  expect_true("24_ROH_FROH_by_sample" %in% names(plots))
})

test_that("ROH module descriptor owns the complete registry contract", {
  spec <- popgenVCF::roh_module_spec()
  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "roh")
  expect_identical(spec$requires, character())
  expect_identical(spec$outputs, "roh")
  expect_identical(spec$references, "Narasimhan et al. 2016")
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$contract_version, "1.0")
  expect_identical(spec$run, popgenVCF:::run_module_roh)
  expect_identical(spec$validate, popgenVCF:::validate_roh_result)
})

test_that("built-in registry reflects the ROH module descriptor", {
  spec <- popgenVCF::roh_module_spec()
  registry <- popgenVCF::default_analysis_registry()
  module <- registry$modules$roh
  expect_identical(module$name, spec$name)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$run, spec$run)
  expect_identical(module$validate, spec$validate)
})

test_that("validate_roh_result flags missing components, negative lengths, and out-of-range froh", {
  ok <- list(
    runs = data.table::data.table(sample = "s1", length_bp = 300L),
    sample_summary = data.table::data.table(sample = "s1", n_runs = 1L, froh = 0.3)
  )
  expect_true(popgenVCF:::validate_roh_result(ok, NULL, NULL)$valid)

  incomplete <- list(runs = data.table::data.table())
  expect_false(popgenVCF:::validate_roh_result(incomplete, NULL, NULL)$valid)

  negative_length <- ok
  negative_length$runs$length_bp <- -1L
  expect_false(popgenVCF:::validate_roh_result(negative_length, NULL, NULL)$valid)

  impossible_froh <- ok
  impossible_froh$sample_summary$froh <- 1.5
  expect_false(popgenVCF:::validate_roh_result(impossible_froh, NULL, NULL)$valid)
})
