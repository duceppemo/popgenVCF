test_that("artifact declarations are opt-in and visible", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  registry <- popgenVCF::new_analysis_registry()
  registry <- popgenVCF::register_analysis(registry, "pca", runner)
  registry <- popgenVCF::register_analysis_artifacts(
    registry,
    "pca",
    c("scores", "scree"),
    must_exist = FALSE
  )

  listed <- popgenVCF::list_analyses(registry)
  expect_equal(listed[name == "pca", artifacts], "scores,scree")
  expect_false(listed[name == "pca", artifacts_must_exist])
})

test_that("artifact declarations reject unknown modules and invalid names", {
  registry <- popgenVCF::new_analysis_registry()
  expect_error(
    popgenVCF::register_analysis_artifacts(registry, "missing", "table"),
    "Unknown analysis module"
  )

  runner <- function(analysis, context) list(analysis = analysis, context = context)
  registry <- popgenVCF::register_analysis(registry, "pca", runner)
  expect_error(
    popgenVCF::register_analysis_artifacts(registry, "pca", c("scores", "")),
    "non-empty names"
  )
})

test_that("module artifact validation enforces namespace and declarations", {
  manifest <- popgenVCF::new_artifact_manifest(list(
    popgenVCF::new_analysis_artifact(
      module = "pca",
      name = "scores",
      type = "table",
      path = "tables/pca_scores.tsv",
      format = "tsv"
    )
  ))

  expect_true(popgenVCF:::validate_module_artifacts(
    "pca", "scores", manifest
  ))
  expect_error(
    popgenVCF:::validate_module_artifacts("pca", "scree", manifest),
    "did not produce declared artifact"
  )
  expect_error(
    popgenVCF:::validate_module_artifacts("fst", "scores", manifest),
    "did not produce declared artifact"
  )
})

test_that("manifests accumulate and reject cross-module duplicates", {
  first <- popgenVCF::new_artifact_manifest(list(
    popgenVCF::new_analysis_artifact(
      "pca", "scores", "table", "pca.tsv", "tsv"
    )
  ))
  second <- popgenVCF::new_artifact_manifest(list(
    popgenVCF::new_analysis_artifact(
      "fst", "matrix", "table", "fst.tsv", "tsv"
    )
  ))

  combined <- popgenVCF:::append_artifact_manifest(first, second)
  expect_equal(nrow(popgenVCF::artifact_manifest_table(combined)), 2L)

  duplicate <- popgenVCF::new_artifact_manifest(list(
    popgenVCF::new_analysis_artifact(
      "pca", "scores", "table", "duplicate.tsv", "tsv"
    )
  ))
  expect_error(
    popgenVCF:::append_artifact_manifest(first, duplicate),
    "duplicate artifact identifier"
  )
})
