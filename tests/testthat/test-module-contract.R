test_that("default modules expose complete contracts", {
  r <- default_analysis_registry()
  x <- list_analyses(r)
  expect_true(all(c("name", "outputs", "resource_class", "contract_version", "references") %in% names(x)))
  expect_true(all(nzchar(x$outputs)))
  expect_true(all(x$resource_class %in% c("light", "standard", "heavy", "external")))
})

test_that("declared outputs are enforced", {
  cfg <- default_config()
  a <- new_popgen_vcf_analysis(cfg)
  a$samples$ids <- c("a", "b")
  a$samples$metadata <- data.table::data.table(sample = c("a", "b"), population = c("x", "y"))
  a$variants$qc_ids <- 1:2
  a$variants$ld_ids <- 1:2
  r <- new_analysis_registry()
  bad <- function(analysis, context) list(analysis = analysis, context = context)
  r <- register_analysis(r, "bad", bad, outputs = "missing")
  expect_error(execute_analysis_registry(a, list(), r), "did not produce declared output")
})

test_that("PCA validator rejects invalid variance", {
  x <- list(scores = data.table::data.table(sample = c("a", "b"), PC1 = 1:2, PC2 = 2:3),
            variance = data.table::data.table(proportion = c(.8, .5), percent = c(80, 50)))
  v <- popgenVCF:::validate_pca_result(x, NULL, NULL)
  expect_false(v$valid)
})
