write_minimal_vcf <- function(path) {
  writeLines(c(
    "##fileformat=VCFv4.2",
    "##contig=<ID=1,length=1000>",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\ts1\ts2",
    "1\t20\tv2\tA\tG\t.\tPASS\t.\tGT\t0/1\t1/1",
    "1\t10\tv1\tC\tT\t.\tPASS\t.\tGT\t0/0\t0/1"
  ), path, useBytes = TRUE)
}

test_that("VCF input validation accepts only .vcf and .vcf.gz", {
  x <- tempfile(fileext = ".txt")
  writeLines("not a VCF", x)
  expect_error(
    popgenVCF::prepare_vcf_input(x, tempfile("vcf-cache-")),
    "must end in .vcf or .vcf.gz"
  )
})

test_that("plain VCF is sorted, BGZF-compressed, and indexed", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  source <- tempfile(fileext = ".vcf")
  cache <- tempfile("vcf-cache-")
  write_minimal_vcf(source)

  prepared <- popgenVCF::prepare_vcf_input(source, cache)
  expect_true(prepared$normalized)
  expect_true(grepl("\\.vcf\\.gz$", prepared$path))
  expect_true(file.exists(prepared$path))
  expect_true(file.exists(prepared$index))

  positions <- system2(
    Sys.which("bcftools"),
    c("query", "-f", shQuote("%POS\\n"), shQuote(prepared$path)),
    stdout = TRUE
  )
  expect_equal(as.integer(positions), c(10L, 20L))
})

test_that("ordinary gzip VCF is normalized to indexed BGZF", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  plain <- tempfile(fileext = ".vcf")
  compressed <- paste0(plain, ".gz")
  cache <- tempfile("vcf-cache-")
  write_minimal_vcf(plain)

  input <- file(plain, "rb")
  output <- gzfile(compressed, "wb")
  writeBin(readBin(input, "raw", n = file.info(plain)$size), output)
  close(input)
  close(output)

  prepared <- popgenVCF::prepare_vcf_input(compressed, cache)
  expect_true(prepared$normalized)
  expect_true(file.exists(prepared$index))
})

write_vcf_with_positions <- function(path, positions) {
  writeLines(c(
    "##fileformat=VCFv4.2",
    "##contig=<ID=1,length=1000>",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\ts1\ts2",
    sprintf("1\t%d\tv%d\tA\tG\t.\tPASS\t.\tGT\t0/1\t1/1", positions, seq_along(positions))
  ), path, useBytes = TRUE)
}

test_that("an unchanged source VCF reuses the cached normalized copy", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  source <- tempfile(fileext = ".vcf")
  cache <- tempfile("vcf-cache-")
  write_vcf_with_positions(source, c(10L, 20L))

  first <- popgenVCF::prepare_vcf_input(source, cache)
  cached_mtime <- file.info(first$path)$mtime
  Sys.sleep(1.1)
  second <- popgenVCF::prepare_vcf_input(source, cache)

  expect_identical(file.info(second$path)$mtime, cached_mtime)
})

test_that("a source VCF whose content changed is not served stale even with an older or equal mtime", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  source <- tempfile(fileext = ".vcf")
  cache <- tempfile("vcf-cache-")
  write_vcf_with_positions(source, c(10L, 20L))

  first <- popgenVCF::prepare_vcf_input(source, cache)
  positions <- system2(
    Sys.which("bcftools"), c("query", "-f", shQuote("%POS\\n"), shQuote(first$path)),
    stdout = TRUE
  )
  expect_equal(as.integer(positions), c(10L, 20L))

  # Simulate the real-world scenario that defeats an mtime-only cache check
  # (e.g. restoring an updated file from a backup or via `git checkout`):
  # different content, but a strictly *older* mtime than the existing cache.
  cache_mtime <- file.info(first$path)$mtime
  write_vcf_with_positions(source, c(30L, 40L, 50L))
  Sys.setFileTime(source, cache_mtime - 3600)
  expect_lt(file.info(source)$mtime, file.info(first$path)$mtime)

  second <- popgenVCF::prepare_vcf_input(source, cache)
  positions <- system2(
    Sys.which("bcftools"), c("query", "-f", shQuote("%POS\\n"), shQuote(second$path)),
    stdout = TRUE
  )
  expect_equal(as.integer(positions), c(30L, 40L, 50L))
})

test_that("force = TRUE always rebuilds regardless of cache validity", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  source <- tempfile(fileext = ".vcf")
  cache <- tempfile("vcf-cache-")
  write_vcf_with_positions(source, c(10L, 20L))

  first <- popgenVCF::prepare_vcf_input(source, cache)
  cached_mtime <- file.info(first$path)$mtime
  Sys.sleep(1.1)
  second <- popgenVCF::prepare_vcf_input(source, cache, force = TRUE)

  expect_gt(file.info(second$path)$mtime, cached_mtime)
})
