test_that("SNPRelate MissingRate output is normalized", {
  raw <- list(
    snp.id = c(1L, 2L),
    AlleleFreq = c(0.25, 0.75),
    MinorFreq = c(0.25, 0.25),
    MissingRate = c(0, 0.125)
  )
  z <- popgenVCF:::normalize_snpratefreq(raw)
  expect_equal(z$missing_rate, c(0, 0.125))
  expect_equal(z$call_rate, c(1, 0.875))
  expect_equal(z$snp_id, c(1L, 2L))
})

test_that("SNPRelate CallRate output is normalized", {
  raw <- list(
    snp.id = c(1L, 2L),
    AlleleFreq = c(0.25, 0.75),
    MinorFreq = c(0.25, 0.25),
    CallRate = c(1, 0.875)
  )
  z <- popgenVCF:::normalize_snpratefreq(raw)
  expect_equal(z$missing_rate, c(0, 0.125))
  expect_equal(z$call_rate, c(1, 0.875))
})

test_that("inconsistent SNPRelate result lengths are rejected", {
  raw <- list(
    snp.id = c(1L, 2L),
    AlleleFreq = 0.25,
    MinorFreq = c(0.25, 0.25),
    MissingRate = c(0, 0.125)
  )
  expect_error(popgenVCF:::normalize_snpratefreq(raw), "Inconsistent")
})

test_that("unbounded LD window has an integer-safe representation", {
  expect_true(is.infinite(popgenVCF::default_config()$qc$ld_slide_max_bp))
  expect_equal(.Machine$integer.max, 2147483647)
})
