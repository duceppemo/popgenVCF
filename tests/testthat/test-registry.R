test_that("registry resolves dependencies in topological order", {
  r <- popgenVCF::new_analysis_registry()
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  r <- popgenVCF::register_analysis(r, "base", runner)
  r <- popgenVCF::register_analysis(r, "child", runner, requires = "base")
  expect_equal(popgenVCF::resolve_analysis_order(r, popgenVCF::default_config(), "child"), c("base", "child"))
})

test_that("registry rejects cycles and missing dependencies", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  r <- popgenVCF::new_analysis_registry()
  r <- popgenVCF::register_analysis(r, "a", runner, requires = "missing")
  expect_error(popgenVCF::resolve_analysis_order(r, popgenVCF::default_config(), "a"), "unregistered")

  r <- popgenVCF::new_analysis_registry()
  r <- popgenVCF::register_analysis(r, "a", runner, requires = "b")
  r <- popgenVCF::register_analysis(r, "b", runner, requires = "a")
  expect_error(popgenVCF::resolve_analysis_order(r, popgenVCF::default_config(), "a"), "Circular")
})

test_that("default registry exposes core modules", {
  x <- popgenVCF::list_analyses(popgenVCF::default_analysis_registry())
  expect_true(all(c("diversity", "pca", "ibs", "tree", "fst") %in% x$name))
})
