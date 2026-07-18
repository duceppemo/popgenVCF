phase_73_fixture <- function() {
  dependencies <- data.frame(
    section_id = c("methods", "results"), dependency_id = c("analysis", "methods"),
    dependency_type = c("input", "section"), policy = c("regenerate", "regenerate"),
    stringsAsFactors = FALSE
  )
  changes <- data.frame(dependency_id = "analysis", before_identity = "sha256:old", after_identity = "sha256:new", change_type = "modified", stringsAsFactors = FALSE)
  plan <- new_manuscript_regeneration_plan("paper", "revision-3", dependencies, changes)
  actions <- data.frame(
    section_id = c("methods", "results"), action = c("regenerate", "regenerate"),
    status = c("completed", "completed"), executor_id = c("generator-v1", "generator-v1"),
    output_identity = c(paste0(rep("a", 64), collapse = ""), paste0(rep("b", 64), collapse = "")), note = c("", ""), stringsAsFactors = FALSE
  )
  execution <- new_manuscript_regeneration_execution(plan, actions, "execution-3")
  checks <- data.frame(
    section_id = c("results", "methods"), output_identity = rev(actions$output_identity),
    verified_identity = rev(actions$output_identity), status = c("verified", "verified"),
    verifier_id = c("validator-v1", "validator-v1"), note = c("", ""), stringsAsFactors = FALSE
  )
  verification <- new_manuscript_regeneration_verification(execution, checks, "verification-3", plan)
  list(plan = plan, execution = execution, verification = verification)
}

test_that("regeneration verification is deterministic and linked", {
  fixture <- phase_73_fixture()
  reversed <- fixture$verification$checks[2:1]
  second <- new_manuscript_regeneration_verification(fixture$execution, reversed, "verification-3", fixture$plan)
  expect_s3_class(fixture$verification, "PopgenVCFRegenerationVerification")
  expect_identical(fixture$verification$digest, second$digest)
  expect_true(validate_manuscript_regeneration_verification(fixture$verification, fixture$execution, fixture$plan, strict = TRUE))
})

test_that("regeneration verification rejects identity mismatches", {
  fixture <- phase_73_fixture()
  checks <- as.data.frame(fixture$verification$checks)
  checks$verified_identity[[1]] <- paste0(rep("c", 64), collapse = "")
  expect_error(new_manuscript_regeneration_verification(fixture$execution, checks, "verification-bad", fixture$plan), "matching identities")
})

test_that("regeneration verification bundles are tamper evident", {
  fixture <- phase_73_fixture()
  path <- tempfile("regeneration-verification-")
  write_manuscript_regeneration_verification(fixture$verification, path, fixture$execution, fixture$plan)
  expect_true(validate_manuscript_regeneration_verification(path))
  expect_error(write_manuscript_regeneration_verification(fixture$verification, path), "already exists")
  write("tampered", file.path(path, "regeneration-verification.md"), append = TRUE)
  expect_error(validate_manuscript_regeneration_verification(path), "checksum mismatch")
})

test_that("scientific releases are deterministic", {
  fixture <- phase_73_fixture()
  digest_names <- c("analysis_registry", "provenance_dag", "artifact_lineage", "fair_bundle", "manuscript", "regeneration_plan", "regeneration_execution", "regeneration_verification", "benchmark", "scientific_validation")
  digests <- setNames(as.list(vapply(seq_along(digest_names), function(i) digest::digest(paste0("component-", i), algo = "sha256", serialize = FALSE), character(1))), digest_names)
  digests$regeneration_plan <- fixture$plan$digest
  digests$regeneration_execution <- fixture$execution$digest
  digests$regeneration_verification <- fixture$verification$digest
  first <- new_scientific_release_bundle("v0.10.0", "0.10.0", paste0(rep("d", 40), collapse = ""), "v0.10.0", "2026-07-17", digests, c(data.table = "1.17.8", digest = "0.6.37"), r_version = "R version 4.5.1", platform = "x86_64-pc-linux-gnu")
  second <- new_scientific_release_bundle("v0.10.0", "0.10.0", paste0(rep("d", 40), collapse = ""), "v0.10.0", "2026-07-17", digests, c(digest = "0.6.37", data.table = "1.17.8"), r_version = "R version 4.5.1", platform = "x86_64-pc-linux-gnu")
  expect_s3_class(first, "PopgenVCFScientificRelease")
  expect_identical(first$digest, second$digest)
  expect_identical(first$dependencies, second$dependencies)
  expect_true(validate_scientific_release_bundle(first))
})

test_that("scientific release bundles are protected and tamper evident", {
  digest_names <- c("analysis_registry", "provenance_dag", "artifact_lineage", "fair_bundle", "manuscript", "regeneration_plan", "regeneration_execution", "regeneration_verification", "benchmark", "scientific_validation")
  digests <- setNames(rep(list(paste0(rep("e", 64), collapse = "")), length(digest_names)), digest_names)
  release <- new_scientific_release_bundle("release-1", "0.10.0", paste0(rep("f", 40), collapse = ""), "v0.10.0", "2026-07-17", digests, r_version = "R version 4.5.1", platform = "test-platform")
  path <- tempfile("scientific-release-")
  write_scientific_release_bundle(release, path)
  expect_true(validate_scientific_release_bundle(path))
  expect_error(write_scientific_release_bundle(release, path), "already exists")
  write("tampered", file.path(path, "scientific-release.tsv"), append = TRUE)
  expect_error(validate_scientific_release_bundle(path), "checksum mismatch")
})
