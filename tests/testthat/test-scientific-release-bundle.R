scientific_release_fixture <- function() {
  hex <- function(x) paste(rep(x, 64L), collapse = "")
  digests <- setNames(
    vapply(c("a", "b", "c", "d", "e", "f", "1", "2", "3", "4"), hex, character(1)),
    c("analysis_registry", "provenance_dag", "artifact_lineage", "fair_bundle",
      "manuscript", "regeneration_plan", "regeneration_execution",
      "regeneration_verification", "benchmark", "scientific_validation")
  )
  artifacts <- data.frame(
    path = c("results/analysis.json", "manuscript/paper.md"),
    size_bytes = c(120, 240),
    sha256 = c(hex("5"), hex("6")),
    stringsAsFactors = FALSE
  )
  dependencies <- data.frame(
    package = c("jsonlite", "digest"),
    version = c("1.9.0", "0.6.37"),
    stringsAsFactors = FALSE
  )
  new_scientific_release_bundle(
    release_id = "popgenvcf-0.10.0",
    package_version = "0.10.0",
    git_commit = paste(rep("7", 40L), collapse = ""),
    git_tag = "v0.10.0",
    release_date = "2026-07-18",
    digest_chain = digests,
    artifacts = artifacts,
    dependencies = dependencies,
    git_branch = "main",
    git_remote = "https://github.com/duceppemo/popgenVCF",
    r_version = "R version 4.5.1",
    platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    operating_system = "Linux"
  )
}

test_that("scientific release identity is deterministic", {
  first <- scientific_release_fixture()
  second <- scientific_release_fixture()
  expect_s3_class(first, "PopgenVCFScientificRelease")
  expect_identical(first$digest, second$digest)
  expect_true(validate_scientific_release_bundle(first))
})

test_that("canonical tables and rendering are stable", {
  release <- scientific_release_fixture()
  table <- scientific_release_bundle_table(release)
  expect_identical(table$component, c(
    "analysis_registry", "provenance_dag", "artifact_lineage", "fair_bundle",
    "manuscript", "regeneration_plan", "regeneration_execution",
    "regeneration_verification", "benchmark", "scientific_validation"
  ))
  markdown <- render_scientific_release_bundle(release)
  expect_true(any(grepl("popgenVCF scientific release", markdown, fixed = TRUE)))
  expect_true(any(grepl(release$digest, markdown, fixed = TRUE)))
})

test_that("digest chains are complete and immutable", {
  release <- scientific_release_fixture()
  incomplete <- release$digest_chain[-1L]
  expect_error(scientific_release_digest_chain(incomplete), "must contain")
  duplicate <- release$digest_chain
  duplicate[[2L]] <- duplicate[[1L]]
  expect_error(scientific_release_digest_chain(duplicate), "must be unique")
  damaged <- release
  damaged$digest_chain[[1L]] <- paste(rep("9", 64L), collapse = "")
  expect_error(validate_scientific_release_bundle(damaged), "digest mismatch")
})

test_that("artifact manifests reject unsafe or ambiguous identities", {
  release <- scientific_release_fixture()
  bad_path <- as.data.frame(release$artifacts)
  bad_path$path[[1L]] <- "../escape.json"
  expect_error(scientific_release_artifacts(bad_path), "relative and normalized")
  duplicate <- as.data.frame(release$artifacts)
  duplicate$path[[2L]] <- duplicate$path[[1L]]
  expect_error(scientific_release_artifacts(duplicate), "paths must be unique")
  invalid <- as.data.frame(release$artifacts)
  invalid$sha256[[1L]] <- "not-a-digest"
  expect_error(scientific_release_artifacts(invalid), "must be a SHA256")
})

test_that("release dates and dependency identities are validated", {
  args <- as.list(environment(scientific_release_fixture))
  release <- scientific_release_fixture()
  bad_dependencies <- as.data.frame(release$dependencies)
  bad_dependencies$package[[2L]] <- bad_dependencies$package[[1L]]
  expect_error(scientific_release_dependencies(bad_dependencies), "unique package names")
  expect_error(new_scientific_release_bundle(
    "release", "0.10.0", "commit", "tag", "18-07-2026",
    release$digest_chain, release$artifacts, release$dependencies
  ), "YYYY-MM-DD")
})

test_that("written bundles are protected and checksum verified", {
  release <- scientific_release_fixture()
  path <- tempfile("scientific-release-")
  written <- write_scientific_release_bundle(release, path)
  expect_true(dir.exists(written))
  expect_true(validate_scientific_release_bundle(written))
  expect_error(write_scientific_release_bundle(release, path), "already exists")
  cat("tampered", file = file.path(path, "scientific-release.md"), append = TRUE)
  expect_error(validate_scientific_release_bundle(path), "checksum mismatch")
})
