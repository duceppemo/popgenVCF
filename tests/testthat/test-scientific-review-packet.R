scientific_review_packet_environment <- function() {
  env <- new.env(parent = globalenv())
  module <- system.file(
    "scripts", "scientific_review_packet.R", package = "popgenVCF"
  )
  if (!nzchar(module)) {
    module <- testthat::test_path(
      "..", "..", "inst", "scripts", "scientific_review_packet.R"
    )
  }
  sys.source(module, envir = env)
  env
}

scientific_review_metadata_path <- function(filename) {
  path <- system.file("metadata", filename, package = "popgenVCF")
  if (nzchar(path)) return(path)
  testthat::test_path("..", "..", "inst", "metadata", filename)
}

scientific_review_fixture <- function(tamper = FALSE) {
  evidence <- tempfile("scientific-review-evidence-")
  dir.create(evidence)
  payload <- file.path(evidence, "component-result.txt")
  writeLines("retained scientific result", payload, useBytes = TRUE)
  expected <- digest::digest(payload, algo = "sha256", file = TRUE)
  if (isTRUE(tamper)) expected <- paste(rep("0", 64L), collapse = "")
  writeLines(
    paste(expected, basename(payload), sep = "  "),
    file.path(evidence, "component-SHA256SUMS.txt"),
    useBytes = TRUE
  )
  evidence
}

test_that("review packet summarizes incomplete component evidence without approval", {
  env <- scientific_review_packet_environment()
  evidence <- scientific_review_fixture()
  output <- tempfile("scientific-review-packet-")

  result <- env$build_scientific_review_packet(
    evidence, output,
    scientific_review_metadata_path("scientific-review-assignment.json"),
    scientific_review_metadata_path("release-candidate-policy.json")
  )

  expect_identical(result$status, "EVIDENCE INCOMPLETE")
  expect_true(all(file.exists(file.path(output, c(
    "scientific-review-report.md", "automated-checks.tsv",
    "assigned-gates.tsv", "manual-review-checklist.tsv",
    "scientific-review-decision-template.json",
    "scientific-review-packet-SHA256SUMS.txt"
  )))))
  checks <- data.table::fread(file.path(output, "automated-checks.tsv"))
  expect_true(any(checks$category == "integrity" & checks$status == "pass"))
  decision <- jsonlite::read_json(
    file.path(output, "scientific-review-decision-template.json"),
    simplifyVector = TRUE
  )
  expect_identical(decision$decision, "pending")
  expect_identical(decision$reviewer$orcid, "0000-0003-2130-0427")
})

test_that("review packet reports checksum failures and strict mode blocks", {
  env <- scientific_review_packet_environment()
  evidence <- scientific_review_fixture(tamper = TRUE)
  output <- tempfile("scientific-review-packet-")

  result <- env$build_scientific_review_packet(
    evidence, output,
    scientific_review_metadata_path("scientific-review-assignment.json"),
    scientific_review_metadata_path("release-candidate-policy.json")
  )
  expect_identical(result$status, "INTEGRITY FAILED")
  expect_true(any(result$checks$status == "fail"))

  expect_error(
    env$build_scientific_review_packet(
      evidence, tempfile("scientific-review-strict-"),
      scientific_review_metadata_path("scientific-review-assignment.json"),
      scientific_review_metadata_path("release-candidate-policy.json"),
      strict = TRUE
    ),
    "not complete in strict mode"
  )
})

test_that("review packet checks all six baseline proposal values", {
  env <- scientific_review_packet_environment()
  evidence <- scientific_review_fixture()
  baseline <- file.path(evidence, "autosomal-baseline-proposal")
  dir.create(baseline)
  ids <- c(
    "subset_variant_count", "retained_sample_count", "qc_variant_count",
    "ld_pruned_variant_count", "pca_pc1_variance_proportion",
    "pca_pc2_variance_proportion"
  )
  values <- c(21418, 2504, 2028, 350, 0.26553988138366075, 0.17740063253018323)
  metrics <- lapply(seq_along(ids), function(i) list(
    id = ids[[i]], expected = values[[i]],
    comparator = if (i <= 4L) "exact" else "relative",
    tolerance = if (i <= 4L) 0 else 1e-6
  ))
  snapshot_path <- file.path(baseline, "autosomal-baseline-proposal.json")
  jsonlite::write_json(
    list(
      approval = "proposed", approved_by = NULL, approved_at = NULL,
      baseline_registry = list(metrics = metrics)
    ),
    snapshot_path, auto_unbox = TRUE, pretty = TRUE, null = "null",
    digits = 17
  )
  observation_path <- file.path(baseline, "autosomal-baseline-observations.tsv")
  data.table::fwrite(
    data.frame(
      metric_id = ids,
      value = format(values, digits = 17, scientific = FALSE, trim = TRUE)
    ),
    observation_path, sep = "\t"
  )
  retained <- c(snapshot_path, observation_path)
  writeLines(
    paste(
      vapply(
        retained, digest::digest, character(1L),
        algo = "sha256", file = TRUE
      ),
      basename(retained), sep = "  "
    ),
    file.path(baseline, "autosomal-baseline-SHA256SUMS.txt"),
    useBytes = TRUE
  )

  output <- tempfile("scientific-review-baseline-")
  result <- env$build_scientific_review_packet(
    evidence, output,
    scientific_review_metadata_path("scientific-review-assignment.json"),
    scientific_review_metadata_path("release-candidate-policy.json")
  )

  baseline_summary <- data.table::fread(
    file.path(output, "baseline-summary.tsv")
  )
  expect_equal(nrow(baseline_summary), 6L)
  expect_true(all(baseline_summary$internal_match))
  expect_true(any(
    result$checks$check_id == "proposal_internal_consistency" &
      result$checks$status == "pass"
  ))
  expect_true(any(
    result$checks$check_id == "proposal_unapproved_boundary" &
      result$checks$status == "pass"
  ))
})
