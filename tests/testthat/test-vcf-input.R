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
  on.exit(close(input), add = TRUE)
  on.exit(close(output), add = TRUE)
  writeBin(readBin(input, "raw", n = file.info(plain)$size), output)
  close(input)
  close(output)

  prepared <- popgenVCF::prepare_vcf_input(compressed, cache)
  expect_true(prepared$normalized)
  expect_true(file.exists(prepared$index))
})
