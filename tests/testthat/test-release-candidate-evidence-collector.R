rcec_script_path <- function() {
  root <- testthat::test_path("..", "..")
  path <- file.path(root, "scripts", "build_release_candidate_evidence_index.R")
  if (!file.exists(path)) {
    testthat::skip("build_release_candidate_evidence_index.R is unavailable outside the repository checkout")
  }
  path
}

rcec_policy_path <- function() {
  # Prefer the checked-out source tree over a possibly-stale installed copy --
  # see the identical rationale on rc_policy_path() in
  # helper-release-candidate-dossier.R.
  source_path <- testthat::test_path("..", "..", "inst", "metadata", "release-candidate-policy.json")
  if (file.exists(source_path)) return(source_path)
  installed <- system.file("metadata", "release-candidate-policy.json", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  source_path
}

rcec_write_fragment <- function(dir, gate_id, status = "passed", summary = "test summary",
                                artifacts = list(), approval = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  record <- list(
    gate_id = gate_id, status = status, summary = summary,
    artifacts = artifacts, approval = approval
  )
  jsonlite::write_json(
    record, file.path(dir, paste0(gate_id, "-gate-record.json")),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
}

rcec_artifact_entry <- function(dir, relative_path, content) {
  path <- file.path(dir, relative_path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path, useBytes = TRUE)
  list(
    path = relative_path,
    size_bytes = as.numeric(file.info(path)$size),
    sha256 = tolower(digest::digest(path, algo = "sha256", file = TRUE))
  )
}

rcec_run <- function(sources_dir, index_path = tempfile(fileext = ".json"),
                     evidence_dir = tempfile(), candidate_id = "test-candidate-1") {
  rscript <- file.path(
    R.home("bin"), paste0("Rscript", if (.Platform$OS.type == "windows") ".exe" else "")
  )
  output <- suppressWarnings(system2(
    rscript,
    shQuote(c(
      rcec_script_path(), rcec_policy_path(), sources_dir, index_path, evidence_dir,
      candidate_id, paste(rep("a", 40L), collapse = ""), "2026-08-02T00:00:00Z"
    )),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = output, index_path = index_path, evidence_dir = evidence_dir)
}

test_that("fragments from multiple source directories are merged and flattened", {
  sources <- tempfile("popgenvcf-rcec-sources-")
  gate_a <- file.path(sources, "gate-a")
  gate_b <- file.path(sources, "gate-b")
  rcec_write_fragment(gate_a, "metadata_consistency",
    artifacts = list(rcec_artifact_entry(gate_a, "report.txt", "hello")))
  rcec_write_fragment(gate_b, "public_api_contract",
    artifacts = list(rcec_artifact_entry(gate_b, "nested/data.txt", "world")))

  result <- rcec_run(sources)
  expect_identical(result$status, 0L)
  expect_true(any(grepl("Fragments merged: 2", result$output, fixed = TRUE)))
  expect_true(any(grepl("required gates passed: 2 / 15", result$output, fixed = TRUE)))

  index <- jsonlite::read_json(result$index_path, simplifyVector = FALSE)
  by_id <- stats::setNames(index$records, vapply(index$records, `[[`, character(1L), "gate_id"))
  expect_identical(by_id$metadata_consistency$status, "passed")
  expect_identical(by_id$public_api_contract$status, "passed")
  expect_identical(by_id$oci_distribution$status, "not_run")

  expect_setequal(
    list.files(result$evidence_dir),
    c("metadata-consistency--report.txt", "public-api-contract--nested-data.txt")
  )
})

test_that("duplicate gate-record fragments for the same gate are rejected", {
  sources <- tempfile("popgenvcf-rcec-sources-")
  rcec_write_fragment(file.path(sources, "one"), "metadata_consistency")
  rcec_write_fragment(file.path(sources, "two"), "metadata_consistency")

  result <- rcec_run(sources)
  expect_gt(result$status, 0L)
  expect_true(any(grepl("Duplicate gate-record fragments", result$output, fixed = TRUE)))
})

test_that("a tampered artifact checksum is rejected", {
  sources <- tempfile("popgenvcf-rcec-sources-")
  gate_dir <- file.path(sources, "gate")
  dir.create(gate_dir, recursive = TRUE)
  writeLines("real content", file.path(gate_dir, "report.txt"), useBytes = TRUE)
  rcec_write_fragment(gate_dir, "metadata_consistency", artifacts = list(list(
    path = "report.txt", size_bytes = 999,
    sha256 = paste(rep("0", 64L), collapse = "")
  )))

  result <- rcec_run(sources)
  expect_gt(result$status, 0L)
  expect_true(any(grepl("Checksum mismatch", result$output, fixed = TRUE)))
})

test_that("an unknown gate_id is rejected", {
  sources <- tempfile("popgenvcf-rcec-sources-")
  rcec_write_fragment(file.path(sources, "gate"), "not_a_real_gate")

  result <- rcec_run(sources)
  expect_gt(result$status, 0L)
  expect_true(any(grepl("unknown gate_id", result$output, fixed = TRUE)))
})

test_that("supplying every gate with approved, passed fragments makes the dossier ready", {
  policy <- jsonlite::read_json(rcec_policy_path(), simplifyVector = FALSE)
  sources <- tempfile("popgenvcf-rcec-sources-")

  for (gate in policy$gates) {
    gate_dir <- file.path(sources, gate$id)
    artifact <- rcec_artifact_entry(gate_dir, "evidence.txt", paste("evidence for", gate$id))
    approval <- if (isTRUE(gate$approval_required)) {
      list(state = "approved", reviewer = "Test Reviewer", reviewed_at = "2026-08-02",
           notes = "Synthetic fixture approval.")
    } else {
      NULL
    }
    rcec_write_fragment(
      gate_dir, gate$id, summary = paste("synthetic evidence for", gate$id),
      artifacts = list(artifact), approval = approval
    )
  }

  result <- rcec_run(sources)
  expect_identical(result$status, 0L)
  expect_true(any(grepl("release_ready: TRUE", result$output, fixed = TRUE)))
  expect_true(any(grepl("required gates passed: 15 / 15", result$output, fixed = TRUE)))
})

test_that("a source directory with no gate-record fragments fails closed", {
  sources <- tempfile("popgenvcf-rcec-sources-")
  dir.create(sources, recursive = TRUE)
  result <- rcec_run(sources)
  expect_gt(result$status, 0L)
  expect_true(any(grepl("No \\*-gate-record.json fragments found", result$output)))
})
