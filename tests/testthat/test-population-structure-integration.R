test_that("synthetic DAPC structure validation passes", {
  skip_on_cran()
  x <- run_population_structure_validation(integration = TRUE)
  expect_true(x$passed, info = paste(x$checks$label[!x$checks$passed], collapse = "; "))
})
