write_test_plink_bundle <- function(prefix, chromosomes = c("chrI", "2")) {
  writeBin(as.raw(c(0x6c, 0x1b, 0x01, 0x00)), paste0(prefix, ".bed"))
  data.table::fwrite(
    data.table::data.table(
      V1 = chromosomes,
      V2 = paste0("snp", seq_along(chromosomes)),
      V3 = 0,
      V4 = seq_along(chromosomes),
      V5 = "A",
      V6 = "C"
    ),
    paste0(prefix, ".bim"),
    sep = "\t",
    col.names = FALSE
  )
  data.table::fwrite(
    data.table::data.table(
      V1 = 0,
      V2 = c("sample1", "sample2"),
      V3 = 0,
      V4 = 0,
      V5 = 0,
      V6 = -9
    ),
    paste0(prefix, ".fam"),
    sep = "\t",
    col.names = FALSE
  )
  prefix
}

test_that("ADMIXTURE BIM normalization converts non-integer chromosome labels", {
  root <- tempfile("admixture-bim-")
  dir.create(root)
  prefix <- write_test_plink_bundle(file.path(root, "cohort"))

  result <- popgenVCF:::normalize_admixture_bim_chromosomes(prefix)
  bim <- data.table::fread(paste0(prefix, ".bim"), header = FALSE)

  expect_true(result$changed)
  expect_identical(result$changed_rows, 1L)
  expect_equal(as.character(bim[[1L]]), c("0", "2"))
  expect_true(file.exists(result$mapping_file))
  mapping <- data.table::fread(result$mapping_file)
  expect_equal(mapping$original_chromosome, "chrI")
  expect_equal(mapping$admixture_chromosome, 0)

  repeated <- popgenVCF:::normalize_admixture_bim_chromosomes(prefix)
  expect_false(repeated$changed)
})

test_that("ADMIXTURE arguments follow the documented command order", {
  root <- tempfile("admixture-args-")
  dir.create(root)
  bed <- file.path(root, "cohort.bed")
  file.create(bed)

  args <- popgenVCF:::admixture_command_arguments(
    bed = bed,
    k = 3L,
    cv_folds = 5L,
    threads = 4L
  )

  expect_identical(args[[1L]], "--cv=5")
  expect_identical(args[[2L]], normalizePath(bed))
  expect_identical(args[[3L]], "3")
  expect_identical(args[[4L]], "-j4")
})

test_that("ADMIXTURE backend failures retain the real diagnostic", {
  skip_on_os("windows")
  root <- tempfile("admixture-failure-")
  dir.create(root)
  prefix <- write_test_plink_bundle(file.path(root, "cohort"))
  executable <- file.path(root, "fake-admixture")
  writeLines(
    c(
      "#!/bin/sh",
      "echo 'Invalid chromosome code! Use integers.'",
      "exit 255"
    ),
    executable
  )
  Sys.chmod(executable, mode = "0755")
  output_dir <- file.path(root, "output")

  expect_error(
    popgenVCF::run_admixture_cv(
      executable = executable,
      plink_prefix = prefix,
      k_values = 2L,
      output_dir = output_dir
    ),
    paste0(
      "ADMIXTURE failed for K=2 with exit status 255.*",
      "Invalid chromosome code! Use integers"
    )
  )
  expect_true(file.exists(file.path(output_dir, "admixture_K2.log")))
})
