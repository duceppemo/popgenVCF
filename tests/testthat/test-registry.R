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

test_that("resolving a module whose dependency is config-disabled errors instead of silently running it", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  r <- popgenVCF::new_analysis_registry()
  r <- popgenVCF::register_analysis(r, "base", runner, enabled = FALSE)
  r <- popgenVCF::register_analysis(r, "child", runner, requires = "base", enabled = TRUE)

  expect_error(
    popgenVCF::resolve_analysis_order(r, popgenVCF::default_config(), "child"),
    "requires disabled module"
  )

  # The reverse -- both explicitly enabled -- still resolves normally.
  r2 <- popgenVCF::new_analysis_registry()
  r2 <- popgenVCF::register_analysis(r2, "base", runner, enabled = TRUE)
  r2 <- popgenVCF::register_analysis(r2, "child", runner, requires = "base", enabled = TRUE)
  expect_equal(popgenVCF::resolve_analysis_order(r2, popgenVCF::default_config(), "child"), c("base", "child"))
})

test_that("disabling a dependency via a real analyses.* config flag is honored, not overridden", {
  # Reproduces the real diversity/fst relationship: fst declares
  # requires = "diversity", and the real diversity module's `enabled` reads
  # cfg$analyses$diversity, exactly like the production registry.
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  r <- popgenVCF::new_analysis_registry()
  r <- popgenVCF::register_analysis(
    r, "diversity", runner,
    enabled = function(cfg) !identical(cfg$analyses$diversity, FALSE)
  )
  r <- popgenVCF::register_analysis(r, "fst", runner, requires = "diversity", enabled = TRUE)

  cfg <- popgenVCF::default_config()
  cfg$analyses$diversity <- FALSE
  cfg$analyses$fst <- TRUE

  expect_error(
    popgenVCF::resolve_analysis_order(r, cfg, "fst"),
    "requires disabled module\\(s\\): diversity"
  )
})

test_that("default registry exposes core modules", {
  x <- popgenVCF::list_analyses(popgenVCF::default_analysis_registry())
  expect_true(all(c("diversity", "pca", "ibs", "tree", "fst") %in% x$name))
})
