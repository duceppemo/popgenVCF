test_that("execution-plan identifiers separate plans whose content differs", {
  # The identifier was a placeholder: the SUM of the serialized bytes. Any two
  # plans whose bytes were a rearrangement of each other collided, and a plan
  # edited that way still validated against its recorded identifier.
  plan <- new_execution_plan("pca", "1.0.0", parameters = list(maf = "0.05", k = "12"))
  expect_match(plan$plan_id, "^sha256-[0-9a-f]{64}$")
  expect_true(validate_execution_plan(plan)$valid)
  expect_false(identical(
    new_execution_plan("pca", "1.0.0", parameters = list(maf = "0.50", k = "12"))$plan_id,
    plan$plan_id
  ))
  tampered <- plan
  tampered$parameters$k <- "21"
  expect_false(validate_execution_plan(tampered)$valid)
  # Still a function of content only: argument order does not matter.
  expect_identical(
    new_execution_plan("pca", "1.0.0", parameters = list(k = "12", maf = "0.05"))$plan_id,
    plan$plan_id
  )
})

test_that("content fingerprints do not depend on the session's native encoding", {
  # serialize()'s version-3 header names the native encoding (and the R
  # version), so hashing the whole stream made a record fingerprinted in a
  # UTF-8 session fail verification in a C-locale one.
  skip_on_os("windows")
  header_encoding <- function() {
    raw <- serialize(1L, NULL, version = 3L)
    rawToChar(raw[19:(18L + readBin(raw[15:18], "integer", size = 4L, endian = "big"))])
  }
  value <- list(a = 1L, b = c(x = "one"), c = data.frame(z = 1:2))
  here <- list(
    encoding = header_encoding(),
    hash = popgenVCF:::portable_serialization_sha256(value),
    request = new_public_analysis_request("analysis.execute", "pca", parameters = list(maf = 0.05))
  )
  withr::with_locale(c(LC_CTYPE = "C"), {
    if (identical(header_encoding(), here$encoding)) skip("cannot switch the native encoding in this session")
    expect_identical(popgenVCF:::portable_serialization_sha256(value), here$hash)
    expect_identical(
      new_public_analysis_request("analysis.execute", "pca", parameters = list(maf = 0.05))$fingerprint,
      here$request$fingerprint
    )
    expect_true(validate_public_analysis_request(here$request))
  })
  expect_match(here$hash, "^[0-9a-f]{64}$")
  expect_false(identical(popgenVCF:::portable_serialization_sha256(list(a = 2L)), here$hash))
})
