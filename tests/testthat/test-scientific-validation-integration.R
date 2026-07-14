test_that("SNPRelate core validation agrees with deterministic fixture", {
  skip_on_cran()
  x <- run_scientific_validation(integration = TRUE, threads = 1L)
  expect_true(x$passed, info = paste(x$checks$message[!x$checks$passed], collapse = "; "))
})
