test_that("regeneration plans propagate direct and transitive changes", {
  dependencies <- data.frame(
    section_id = c("methods", "results", "discussion", "discussion"),
    dependency_id = c("analysis.parameters", "analysis.results", "results", "author.interpretation"),
    dependency_type = c("input", "input", "section", "input"),
    policy = c("regenerate", "regenerate", "manual_review", "manual_review")
  )
  changes <- data.frame(
    dependency_id = "analysis.results",
    before_identity = "sha256:old",
    after_identity = "sha256:new",
    change_type = "modified"
  )
  plan <- new_manuscript_regeneration_plan("manuscript-1", "revision-2", dependencies, changes)
  expect_s3_class(plan, "PopgenVCFRegenerationPlan")
  expect_identical(plan$plan[section_id == "results", state], "affected")
  expect_identical(plan$plan[section_id == "discussion", state], "manual_review")
  expect_identical(plan$plan[section_id == "methods", state], "unaffected")
  expect_true(validate_manuscript_regeneration_plan(plan))
})

test_that("blocked policies fail strict validation", {
  dependencies <- data.frame(
    section_id = "results",
    dependency_id = "analysis.results",
    dependency_type = "input",
    policy = "blocked"
  )
  changes <- data.frame(
    dependency_id = "analysis.results",
    before_identity = "old",
    after_identity = "new",
    change_type = "identity_changed"
  )
  plan <- new_manuscript_regeneration_plan("m1", "r2", dependencies, changes)
  expect_identical(plan$plan$state, "blocked")
  expect_error(validate_manuscript_regeneration_plan(plan, strict = TRUE), "blocked manuscript sections")
})

test_that("dependency mappings reject duplicates, unknown sections, and cycles", {
  changes <- data.frame(dependency_id = "x", before_identity = "a", after_identity = "b", change_type = "modified")
  duplicate <- data.frame(
    section_id = c("methods", "methods"), dependency_id = c("x", "x"),
    dependency_type = c("input", "input"), policy = c("regenerate", "regenerate")
  )
  expect_error(new_manuscript_regeneration_plan("m", "r", duplicate, changes), "must be unique")

  unknown <- data.frame(section_id = "results", dependency_id = "missing", dependency_type = "section", policy = "regenerate")
  expect_error(new_manuscript_regeneration_plan("m", "r", unknown, changes), "unknown sections")

  cyclic <- data.frame(
    section_id = c("a", "b", "a"), dependency_id = c("x", "a", "b"),
    dependency_type = c("input", "section", "section"),
    policy = c("regenerate", "regenerate", "regenerate")
  )
  expect_error(new_manuscript_regeneration_plan("m", "r", cyclic, changes), "acyclic")
})

test_that("written regeneration bundles are deterministic and tamper evident", {
  dependencies <- data.frame(
    section_id = "methods", dependency_id = "parameters", dependency_type = "input", policy = "regenerate"
  )
  changes <- data.frame(
    dependency_id = "parameters", before_identity = "old", after_identity = "new", change_type = "modified"
  )
  plan <- new_manuscript_regeneration_plan("m1", "r2", dependencies, changes)
  path <- file.path(tempdir(), paste0("regeneration-", sample.int(1e8, 1L)))
  expect_silent(write_manuscript_regeneration_plan(plan, path))
  expect_true(validate_manuscript_regeneration_plan(path))
  expect_error(write_manuscript_regeneration_plan(plan, path), "already exists")
  writeLines("tampered", file.path(path, "regeneration-plan.md"))
  expect_error(validate_manuscript_regeneration_plan(path), "checksum mismatch")
})

test_that("revision and regeneration APIs are exported", {
  exports <- getNamespaceExports("popgenVCF")
  expect_true(all(c(
    "new_manuscript_revision", "compare_manuscript_revisions",
    "new_manuscript_regeneration_plan", "manuscript_regeneration_table"
  ) %in% exports))
})

test_that("a regeneration plan names every change that reaches a section", {
  # reason/source_changes were single strings overwritten by the last edge
  # applied, and only when that edge did not lower the state -- so a section
  # with its own changed input and a changed upstream section listed one of
  # them, and a manual_review section never listed what arrived through a
  # regenerate edge.
  dependencies <- data.frame(
    section_id = c("methods", "results", "results", "discussion", "discussion"),
    dependency_id = c("config", "fst_table", "methods", "results", "figure_1"),
    dependency_type = c("input", "input", "section", "section", "input"),
    policy = c("regenerate", "manual_review", "regenerate", "regenerate", "regenerate")
  )
  changes <- data.frame(
    dependency_id = c("config", "fst_table", "figure_1"),
    before_identity = "a", after_identity = "b", change_type = "modified"
  )
  plan <- as.data.frame(popgenVCF:::manuscript_regeneration_table(list(dependencies = dependencies, changes = changes)))
  row <- function(id) plan[plan$section_id == id, ]
  expect_identical(row("methods")$source_changes, "config")
  expect_identical(row("results")$state, "manual_review")
  expect_identical(row("results")$source_changes, "config;fst_table")
  expect_identical(row("results")$reason, "Direct changed input: fst_table; Depends on changed section: methods")
  expect_identical(row("discussion")$state, "affected")
  expect_identical(row("discussion")$source_changes, "config;figure_1;fst_table")
  expect_identical(row("discussion")$reason, "Direct changed input: figure_1; Depends on changed section: results")
})
