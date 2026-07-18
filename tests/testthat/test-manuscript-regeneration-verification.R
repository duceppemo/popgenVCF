regeneration_verification_fixture <- function() {
  dependencies <- data.frame(
    section_id = c("methods", "results"),
    dependency_id = c("analysis", "methods"),
    dependency_type = c("input", "section"),
    policy = c("regenerate", "regenerate"),
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    dependency_id = "analysis",
    before_identity = "sha256:old",
    after_identity = "sha256:new",
    change_type = "modified",
    stringsAsFactors = FALSE
  )
  plan <- new_manuscript_regeneration_plan("paper", "revision-3", dependencies, changes)
  actions <- data.frame(
    section_id = c("methods", "results"),
    action = c("regenerate", "regenerate"),
    status = c("completed", "completed"),
    executor_id = c("worker-a", "worker-b"),
    output_identity = c("sha256:methods", "sha256:results"),
    note = c("", ""),
    stringsAsFactors = FALSE
  )
  execution <- new_manuscript_regeneration_execution(plan, actions, "execution-3")
  reviews <- data.frame(
    section_id = c("results", "methods"),
    decision = c("accepted", "accepted"),
    reviewer_id = c("reviewer-b", "reviewer-a"),
    evidence_identity = c("sha256:review-results", "sha256:review-methods"),
    note = c("checked", "checked"),
    stringsAsFactors = FALSE
  )
  list(plan = plan, execution = execution, reviews = reviews)
}

test_that("verification identity is deterministic and canonical", {
  fixture <- regeneration_verification_fixture()
  first <- new_manuscript_regeneration_verification(fixture$execution, fixture$reviews, "verification-3")
  second <- new_manuscript_regeneration_verification(
    fixture$execution,
    fixture$reviews[rev(seq_len(nrow(fixture$reviews))), ],
    "verification-3"
  )
  expect_identical(first$digest, second$digest)
  expect_identical(manuscript_regeneration_verification_table(first)$section_id, c("methods", "results"))
  expect_true(validate_manuscript_regeneration_verification(first, fixture$execution, fixture$plan, strict = TRUE))
})

test_that("verification rejects invalid review contracts", {
  fixture <- regeneration_verification_fixture()
  duplicate <- rbind(fixture$reviews, fixture$reviews[1, ])
  expect_error(new_manuscript_regeneration_verification(fixture$execution, duplicate, "verification"), "unique section_id")

  missing <- fixture$reviews[1, ]
  expect_error(new_manuscript_regeneration_verification(fixture$execution, missing, "verification"), "missing required sections")

  unknown <- fixture$reviews
  unknown$section_id[1] <- "discussion"
  expect_error(new_manuscript_regeneration_verification(fixture$execution, unknown, "verification"), "unknown or unverifiable")

  no_evidence <- fixture$reviews
  no_evidence$evidence_identity[1] <- ""
  expect_error(new_manuscript_regeneration_verification(fixture$execution, no_evidence, "verification"), "require evidence_identity")
})

test_that("strict validation rejects unresolved decisions", {
  fixture <- regeneration_verification_fixture()
  fixture$reviews$decision[1] <- "manual_review"
  fixture$reviews$evidence_identity[1] <- ""
  verification <- new_manuscript_regeneration_verification(fixture$execution, fixture$reviews, "verification")
  expect_true(validate_manuscript_regeneration_verification(verification, fixture$execution))
  expect_error(
    validate_manuscript_regeneration_verification(verification, fixture$execution, strict = TRUE),
    "unaccepted sections"
  )
})

test_that("verification bundles are protected and tamper evident", {
  fixture <- regeneration_verification_fixture()
  verification <- new_manuscript_regeneration_verification(fixture$execution, fixture$reviews, "verification")
  path <- tempfile("regeneration-verification-")
  write_manuscript_regeneration_verification(verification, path, fixture$execution, fixture$plan)
  expect_true(validate_manuscript_regeneration_verification(path))
  expect_error(write_manuscript_regeneration_verification(verification, path), "already exists")
  cat("tampered", file = file.path(path, "regeneration-verification.md"), append = TRUE)
  expect_error(validate_manuscript_regeneration_verification(path), "checksum mismatch")
})

test_that("verification is bound to its execution", {
  fixture <- regeneration_verification_fixture()
  verification <- new_manuscript_regeneration_verification(fixture$execution, fixture$reviews, "verification")
  altered <- fixture$execution
  altered$execution_id <- "different"
  altered$digest <- digest::digest(
    popgenVCF:::regeneration_execution_payload(altered), algo = "sha256", serialize = TRUE
  )
  expect_error(
    validate_manuscript_regeneration_verification(verification, altered),
    "does not reference"
  )
})