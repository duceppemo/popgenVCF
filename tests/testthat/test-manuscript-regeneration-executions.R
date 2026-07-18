regeneration_execution_fixture <- function() {
  dependencies <- data.frame(
    section_id = c("methods", "results", "discussion", "conclusion"),
    dependency_id = c("analysis", "methods", "results", "results"),
    dependency_type = c("input", "section", "section", "section"),
    policy = c("regenerate", "regenerate", "manual_review", "blocked"),
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    dependency_id = "analysis",
    before_identity = "sha256:old",
    after_identity = "sha256:new",
    change_type = "modified",
    stringsAsFactors = FALSE
  )
  plan <- new_manuscript_regeneration_plan("paper", "revision-2", dependencies, changes)
  actions <- data.frame(
    section_id = c("methods", "results", "discussion", "conclusion"),
    action = c("regenerate", "regenerate", "manual_review", "resolve_block"),
    status = rep("completed", 4),
    executor_id = c("generator-v1", "generator-v1", "author-orcid", "author-orcid"),
    output_identity = c("sha256:m", "sha256:r", "sha256:d", "sha256:c"),
    note = c("", "", "Reviewed by author", "Scientific decision resolved by author"),
    stringsAsFactors = FALSE
  )
  list(plan = plan, actions = actions)
}

test_that("regeneration execution records are deterministic", {
  fixture <- regeneration_execution_fixture()
  first <- new_manuscript_regeneration_execution(fixture$plan, fixture$actions, "execution-1")
  second <- new_manuscript_regeneration_execution(fixture$plan, fixture$actions[4:1, ], "execution-1")

  expect_s3_class(first, "PopgenVCFRegenerationExecution")
  expect_identical(first$digest, second$digest)
  expect_identical(first$actions, second$actions)
  expect_true(validate_manuscript_regeneration_execution(first, fixture$plan, strict = TRUE))
})

test_that("execution validation enforces the regeneration plan", {
  fixture <- regeneration_execution_fixture()

  duplicate <- rbind(fixture$actions, fixture$actions[1, ])
  expect_error(new_manuscript_regeneration_execution(fixture$plan, duplicate, "execution-1"), "unique section_id")

  missing <- fixture$actions[-1, ]
  expect_error(new_manuscript_regeneration_execution(fixture$plan, missing, "execution-1"), "missing required sections")

  wrong <- fixture$actions
  wrong$action[wrong$section_id == "discussion"] <- "regenerate"
  expect_error(new_manuscript_regeneration_execution(fixture$plan, wrong, "execution-1"), "incompatible")

  no_output <- fixture$actions
  no_output$output_identity[no_output$section_id == "methods"] <- ""
  expect_error(new_manuscript_regeneration_execution(fixture$plan, no_output, "execution-1"), "require output_identity")
})

test_that("strict execution validation rejects incomplete actions", {
  fixture <- regeneration_execution_fixture()
  actions <- fixture$actions
  actions$status[actions$section_id == "discussion"] <- "pending"
  execution <- new_manuscript_regeneration_execution(fixture$plan, actions, "execution-2")

  expect_true(validate_manuscript_regeneration_execution(execution, fixture$plan))
  expect_error(validate_manuscript_regeneration_execution(execution, fixture$plan, strict = TRUE), "incomplete actions")
})

test_that("execution bundles are deterministic and tamper evident", {
  fixture <- regeneration_execution_fixture()
  execution <- new_manuscript_regeneration_execution(fixture$plan, fixture$actions, "execution-1")
  path <- tempfile("regeneration-execution-")

  written <- write_manuscript_regeneration_execution(execution, path, fixture$plan)
  expect_true(dir.exists(written))
  expect_true(validate_manuscript_regeneration_execution(written))
  expect_error(write_manuscript_regeneration_execution(execution, path, fixture$plan), "already exists")

  write("tampered", file.path(path, "regeneration-execution.md"), append = TRUE)
  expect_error(validate_manuscript_regeneration_execution(path), "checksum mismatch")
})
