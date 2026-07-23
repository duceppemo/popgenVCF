test_that("canonical production accepts positive index metadata counts", {
  calls <- character()
  run <- function(command, args, label) {
    calls <<- c(calls, label)
    if (identical(label, "bcftools indexed variant count")) return("65432")
    stop("unexpected fallback command", call. = FALSE)
  }

  result <- canonical_production_test_env$canonical_production_variant_inventory(
    "bcftools",
    "canonical.vcf.gz",
    run = run
  )

  expect_identical(result$variant_count, 65432)
  expect_identical(result$method, "index_metadata")
  expect_identical(calls, "bcftools indexed variant count")
})

test_that("canonical production falls back when legacy TBI count metadata is unavailable", {
  calls <- character()
  run <- function(command, args, label) {
    calls <<- c(calls, label)
    switch(
      label,
      "bcftools indexed variant count" = ".",
      "bcftools streamed variant inventory" = c("Y", "Y", "Y"),
      "bcftools indexed variant inventory" = c("Y", "Y", "Y"),
      stop("unexpected command", call. = FALSE)
    )
  }

  result <- canonical_production_test_env$canonical_production_variant_inventory(
    "bcftools",
    "canonical.vcf.gz",
    run = run
  )

  expect_identical(result$variant_count, 3)
  expect_identical(result$method, "indexed_query_fallback")
  expect_identical(result$contigs, "Y")
  expect_identical(
    calls,
    c(
      "bcftools indexed variant count",
      "bcftools streamed variant inventory",
      "bcftools indexed variant inventory"
    )
  )
})

test_that("canonical production rejects an incomplete legacy index", {
  run <- function(command, args, label) {
    switch(
      label,
      "bcftools indexed variant count" = "0",
      "bcftools streamed variant inventory" = c("Y", "Y", "Y"),
      "bcftools indexed variant inventory" = c("Y", "Y"),
      stop("unexpected command", call. = FALSE)
    )
  }

  expect_error(
    canonical_production_test_env$canonical_production_variant_inventory(
      "bcftools",
      "canonical.vcf.gz",
      run = run
    ),
    "does not expose the complete streamed variant inventory"
  )
})
