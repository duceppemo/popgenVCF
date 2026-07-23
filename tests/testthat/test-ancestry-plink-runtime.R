fake_gds_to_bed <- function(gdsobj, bed.fn, sample.id, snp.id, verbose = FALSE) {
  writeBin(as.raw(c(0x6c, 0x1b, 0x01, 0x00)), paste0(bed.fn, ".bed"))
  data.table::fwrite(
    data.table::data.table(
      V1 = sample.id, V2 = sample.id, V3 = 0, V4 = 0, V5 = 0, V6 = -9
    ),
    paste0(bed.fn, ".fam"),
    sep = " ", col.names = FALSE
  )
  data.table::fwrite(
    data.table::data.table(
      V1 = 1, V2 = snp.id, V3 = 0, V4 = seq_along(snp.id), V5 = "A", V6 = "C"
    ),
    paste0(bed.fn, ".bim"),
    sep = "\t", col.names = FALSE
  )
  invisible(NULL)
}

test_that("ancestry backends generate and reuse the retained PLINK bundle", {
  root <- tempfile("ancestry-plink-")
  dir.create(root)
  samples <- paste0("sample", 1:4)
  snps <- paste0("snp", 1:7)

  generated <- popgenVCF:::prepare_structure_plink_input(
    gds = NULL,
    sample_ids = samples,
    snp_ids = snps,
    preferred_prefix = file.path(root, "missing-configured-prefix"),
    cache_dir = root,
    converter = fake_gds_to_bed
  )

  expect_identical(generated$source, "generated")
  expect_true(all(file.exists(popgenVCF:::plink_bundle_paths(generated$prefix))))
  expect_equal(readLines(generated$sample_file), samples)
  expect_identical(generated$n_samples, 4L)
  expect_identical(generated$n_snps, 7L)

  reused <- popgenVCF:::prepare_structure_plink_input(
    gds = NULL,
    sample_ids = samples,
    snp_ids = snps,
    preferred_prefix = NULL,
    cache_dir = root,
    converter = function(...) stop("converter should not run for a valid cache")
  )
  expect_identical(reused$source, "cache")
  expect_identical(reused$prefix, generated$prefix)
  expect_equal(readLines(reused$sample_file), samples)
})

test_that("a complete configured PLINK bundle is accepted only when aligned", {
  root <- tempfile("configured-plink-")
  dir.create(root)
  prefix <- file.path(root, "cohort")
  samples <- c("A", "B", "C")
  snps <- paste0("v", 1:5)
  fake_gds_to_bed(NULL, prefix, samples, snps)

  configured <- popgenVCF:::prepare_structure_plink_input(
    gds = NULL,
    sample_ids = samples,
    snp_ids = snps,
    preferred_prefix = prefix,
    cache_dir = root,
    converter = function(...) stop("converter should not run for aligned configured input")
  )
  expect_identical(configured$source, "configured")
  expect_equal(readLines(configured$sample_file), samples)

  mismatch <- popgenVCF:::inspect_plink_bundle(prefix, rev(samples), snps)
  expect_false(mismatch$valid)
  expect_match(mismatch$reason, "sample order")
})

test_that("ADMIXTURE and fastStructure modules use prepared PLINK inputs", {
  admixture_body <- paste(deparse(body(popgenVCF:::run_module_admixture)), collapse = "\n")
  faststructure_body <- paste(deparse(body(popgenVCF:::run_module_faststructure)), collapse = "\n")

  expect_match(admixture_body, "prepare_structure_plink_input", fixed = TRUE)
  expect_match(admixture_body, "context$final_snps", fixed = TRUE)
  expect_match(admixture_body, "plink$sample_file", fixed = TRUE)
  expect_match(faststructure_body, "prepare_structure_plink_input", fixed = TRUE)
  expect_match(faststructure_body, "context$structure_plink", fixed = TRUE)
})
