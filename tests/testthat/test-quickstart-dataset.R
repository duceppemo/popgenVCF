test_that("quickstart_dataset_paths returns existing, matching files", {
  paths <- popgenVCF::quickstart_dataset_paths()
  expect_true(dir.exists(paths$directory))
  expect_true(file.exists(paths$vcf))
  expect_true(file.exists(paths$metadata))

  metadata <- data.table::fread(paths$metadata)
  expect_setequal(names(metadata), c("sample", "population", "latitude", "longitude", "sex"))
  expect_identical(nrow(metadata), 160L)
  expect_identical(data.table::uniqueN(metadata$population), 8L)

  # Both real high-kinship pairs this dataset was deliberately built around
  # must be present. NA19331/NA19334 is a confirmed real duplicate/MZ twin
  # (chromosome X later confirmed both are genuinely male, no contradiction).
  # HG03873/HG03998 is a real high chr22-only-kinship pair that chromosome X
  # data proved is NOT actually a duplicate/twin (genuinely different sexes,
  # and MZ twins share genetic sex by definition) -- deliberately kept as a
  # real cautionary example, see inst/extdata/quickstart/README.md.
  expect_true(all(c("HG03873", "HG03998", "NA19331", "NA19334") %in% metadata$sample))
  expect_identical(metadata$population[metadata$sample == "HG03873"], "ITU")
  expect_identical(metadata$population[metadata$sample == "HG03998"], "STU")

  # Real, self-reported sex from the same authoritative panel file already
  # used for population assignment (its "gender" column), not fabricated or
  # inferred. Every sample must have one of the two real recorded values.
  expect_true(all(metadata$sex %in% c("male", "female")))
  expect_identical(sum(metadata$sex == "male"), 77L)
  expect_identical(sum(metadata$sex == "female"), 83L)

  # The same-population duplicate pair (NA19331/NA19334, both LWK) records
  # the same sex for both, as expected for a genuine duplicate/MZ twin. The
  # high-chr22-kinship pair (HG03873 filed as ITU, HG03998 filed as STU)
  # records *different* sex for the two entries -- and this is not just a
  # metadata quirk: their raw chromosome X genotypes independently confirm
  # genuinely different biological sex (real heterozygous chrX calls for
  # HG03873, real hemizygous calls for HG03998), which rules out this pair
  # being the same individual or MZ twins despite the high autosomal
  # kinship. See test-autosome-filtering.R and
  # inst/extdata/quickstart/README.md for the full finding.
  expect_identical(metadata$sex[metadata$sample == "NA19331"], "male")
  expect_identical(metadata$sex[metadata$sample == "NA19334"], "male")
  expect_identical(metadata$sex[metadata$sample == "HG03873"], "female")
  expect_identical(metadata$sex[metadata$sample == "HG03998"], "male")

  # Real, documented population collection-site coordinates (source:
  # igsr/1000Genomes_data_indexes README_populations.md), not fabricated --
  # every sample must have a finite coordinate, and GBR/ITU/STU share one
  # representative UK (London) point since the source gives no more specific
  # city for any of the three.
  expect_true(all(is.finite(metadata$latitude)))
  expect_true(all(is.finite(metadata$longitude)))
  by_pop <- unique(metadata[, c("population", "latitude", "longitude")])
  data.table::setkey(by_pop, population)
  expect_identical(by_pop["GBR", latitude], by_pop["ITU", latitude])
  expect_identical(by_pop["GBR", longitude], by_pop["STU", longitude])
  expect_equal(by_pop["CHB", latitude], 39.9042)
  expect_equal(by_pop["CHB", longitude], 116.4074)
  expect_equal(by_pop["PEL", latitude], -12.0464)
})

test_that("quickstart_dataset_paths' VCF sample IDs exactly match the metadata", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  paths <- popgenVCF::quickstart_dataset_paths()
  vcf_samples <- system2(Sys.which("bcftools"), c("query", "-l", shQuote(paths$vcf)), stdout = TRUE)
  metadata <- data.table::fread(paths$metadata)
  expect_setequal(vcf_samples, metadata$sample)
  expect_false(anyDuplicated(vcf_samples) > 0L)
})

test_that("Mantel/isolation-by-distance actually runs (not skipped) against the quickstart dataset", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  paths <- popgenVCF::quickstart_dataset_paths()
  root <- withr::local_tempdir()

  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- paths$vcf
  cfg$input$metadata <- paths$metadata
  cfg$output$directory <- root
  cfg$compute$threads <- max(1L, parallel::detectCores() - 1L)
  # Only IBD's own inputs (IBS) and QC/LD are needed; the slow modules
  # (DAPC cross-validation, bootstrap, ancestry backends) are unrelated to
  # this test and disabled to keep it fast. Ancestry backends are already
  # off by default.
  cfg$analyses$dapc_cross_validation <- FALSE
  cfg$analyses$dapc_k <- "2:2"
  cfg$analyses$bootstrap$enabled <- FALSE
  cfg$analyses$structure$replicates <- 1L
  cfg$report$enabled <- FALSE

  analysis <- popgenVCF::run_pipeline(cfg)
  expect_identical(analysis$status, "complete")

  summary_path <- file.path(root, "tables", "25_Mantel_IBD_summary.tsv")
  expect_true(file.exists(summary_path))
  ibd_summary <- data.table::fread(summary_path)
  expect_identical(nrow(ibd_summary), 1L)

  # Real, statistically significant isolation-by-distance signal: genetic
  # distance really does increase with geographic distance across these 8
  # real, continentally-diverse populations. Bounds are loose (this is a
  # real-data smoke check, not a hand-derived exact value), but the sign and
  # significance are the point of adding real coordinates at all.
  expect_true(is.finite(ibd_summary$mantel_r))
  expect_gt(ibd_summary$mantel_r, 0)
  expect_lte(ibd_summary$mantel_p, 0.05)
  expect_gt(ibd_summary$slope, 0)

  pairs_path <- file.path(root, "tables", "26_IBD_pairs.tsv")
  expect_true(file.exists(pairs_path))
  pairs <- data.table::fread(pairs_path)
  expect_gt(nrow(pairs), 0L)
})

test_that("the sex metadata column qualifies for, and produces, a real per-metadata-column PCA panel", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  paths <- popgenVCF::quickstart_dataset_paths()
  root <- withr::local_tempdir()

  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- paths$vcf
  cfg$input$metadata <- paths$metadata
  cfg$output$directory <- root
  cfg$compute$threads <- max(1L, parallel::detectCores() - 1L)
  cfg$analyses$dapc_cross_validation <- FALSE
  cfg$analyses$dapc_k <- "2:2"
  cfg$analyses$bootstrap$enabled <- FALSE
  cfg$analyses$structure$replicates <- 1L
  cfg$report$enabled <- FALSE

  analysis <- popgenVCF::run_pipeline(cfg)
  expect_identical(analysis$status, "complete")

  figures <- list.files(file.path(root, "figures"))
  expect_true(any(grepl("^07b_PCA_PC1_PC2_by_sex\\.", figures)))
  # population itself, and the continuous latitude/longitude columns, must
  # not get a redundant "by_<column>" panel.
  expect_false(any(grepl("_by_population\\.", figures)))
  expect_false(any(grepl("_by_latitude\\.", figures)))
  expect_false(any(grepl("_by_longitude\\.", figures)))
})
