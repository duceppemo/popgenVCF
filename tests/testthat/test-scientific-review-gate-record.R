wsrgr_script_path <- function() {
  root <- testthat::test_path("..", "..")
  path <- file.path(root, "scripts", "write_scientific_review_gate_record.R")
  if (!file.exists(path)) {
    testthat::skip("write_scientific_review_gate_record.R is unavailable outside the repository checkout")
  }
  path
}

wsrgr_run <- function(spec, evidence_dir, output_path = tempfile(fileext = ".json")) {
  spec_path <- tempfile(fileext = ".json")
  jsonlite::write_json(spec, spec_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  rscript <- file.path(
    R.home("bin"), paste0("Rscript", if (.Platform$OS.type == "windows") ".exe" else "")
  )
  output <- suppressWarnings(system2(
    rscript,
    shQuote(c(wsrgr_script_path(), spec_path, evidence_dir, output_path)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = output, output_path = output_path)
}

wsrgr_evidence_dir <- function() {
  dir <- tempfile("popgenvcf-wsrgr-evidence-")
  dir.create(dir, recursive = TRUE)
  writeLines("real evidence content", file.path(dir, "report.txt"), useBytes = TRUE)
  writeLines("more evidence", file.path(dir, "observations.tsv"), useBytes = TRUE)
  dir
}

test_that("a review spec with approval produces a correctly checksummed fragment", {
  evidence_dir <- wsrgr_evidence_dir()
  spec <- list(
    gate_id = "production_baseline", status = "passed",
    summary = "chr22 quantitative baseline reviewed and approved.",
    artifacts = list("report.txt", "observations.tsv"),
    approval = list(state = "approved", reviewer = "Marc-Olivier Duceppe",
                     reviewed_at = "2026-07-30", notes = "See docs/.")
  )
  result <- wsrgr_run(spec, evidence_dir)
  expect_identical(result$status, 0L)

  record <- jsonlite::read_json(result$output_path, simplifyVector = FALSE)
  expect_identical(record$gate_id, "production_baseline")
  expect_identical(record$status, "passed")
  expect_length(record$artifacts, 2L)
  expect_identical(record$approval$state, "approved")
  expect_identical(record$approval$reviewer, "Marc-Olivier Duceppe")

  for (artifact in record$artifacts) {
    real_path <- file.path(evidence_dir, artifact$path)
    expect_identical(
      tolower(digest::digest(real_path, algo = "sha256", file = TRUE)),
      tolower(artifact$sha256)
    )
    expect_identical(as.numeric(file.info(real_path)$size), as.numeric(artifact$size_bytes))
  }
})

test_that("a review spec with no approval field produces a fragment with a null approval", {
  evidence_dir <- wsrgr_evidence_dir()
  spec <- list(
    gate_id = "apptainer_distribution", status = "passed",
    summary = "No named approval required for this gate.",
    artifacts = list("report.txt")
  )
  result <- wsrgr_run(spec, evidence_dir)
  expect_identical(result$status, 0L)
  record <- jsonlite::read_json(result$output_path, simplifyVector = FALSE)
  expect_null(record$approval)
})

test_that("a review spec referencing a nonexistent artifact fails closed", {
  evidence_dir <- wsrgr_evidence_dir()
  spec <- list(
    gate_id = "production_baseline", status = "passed", summary = "x",
    artifacts = list("does-not-exist.txt")
  )
  result <- wsrgr_run(spec, evidence_dir)
  expect_gt(result$status, 0L)
  expect_true(any(grepl("do not exist", result$output, fixed = TRUE)))
})

test_that("a review spec missing a required field fails closed", {
  evidence_dir <- wsrgr_evidence_dir()
  spec <- list(gate_id = "production_baseline", status = "passed", artifacts = list("report.txt"))
  result <- wsrgr_run(spec, evidence_dir)
  expect_gt(result$status, 0L)
  expect_true(any(grepl("missing required field", result$output, fixed = TRUE)))
})

test_that("the fragment produced merges correctly through the evidence-index collector", {
  collector_script <- file.path(testthat::test_path("..", ".."), "scripts", "build_release_candidate_evidence_index.R")
  if (!file.exists(collector_script)) {
    testthat::skip("build_release_candidate_evidence_index.R is unavailable outside the repository checkout")
  }
  evidence_dir <- wsrgr_evidence_dir()
  sources_dir <- tempfile("popgenvcf-wsrgr-sources-")
  gate_dir <- file.path(sources_dir, "production_baseline")
  dir.create(gate_dir, recursive = TRUE)
  file.copy(list.files(evidence_dir, full.names = TRUE), gate_dir)

  spec <- list(
    gate_id = "production_baseline", status = "passed",
    summary = "chr22 quantitative baseline reviewed and approved.",
    artifacts = list("report.txt", "observations.tsv"),
    approval = list(state = "approved", reviewer = "Marc-Olivier Duceppe",
                     reviewed_at = "2026-07-30", notes = "See docs/.")
  )
  fragment_result <- wsrgr_run(spec, gate_dir, file.path(gate_dir, "production_baseline-gate-record.json"))
  expect_identical(fragment_result$status, 0L)

  policy_path <- system.file("metadata", "release-candidate-policy.json", package = "popgenVCF")
  if (!nzchar(policy_path)) {
    policy_path <- testthat::test_path("..", "..", "inst", "metadata", "release-candidate-policy.json")
  }
  rscript <- file.path(R.home("bin"), paste0("Rscript", if (.Platform$OS.type == "windows") ".exe" else ""))
  index_path <- tempfile(fileext = ".json")
  collect_output <- suppressWarnings(system2(
    rscript,
    shQuote(c(
      collector_script, policy_path, sources_dir, index_path, tempfile(),
      "test-merge", paste(rep("a", 40L), collapse = ""), "2026-08-02T00:00:00Z"
    )),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(collect_output, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L)

  index <- jsonlite::read_json(index_path, simplifyVector = FALSE)
  by_id <- stats::setNames(index$records, vapply(index$records, `[[`, character(1L), "gate_id"))
  expect_identical(by_id$production_baseline$status, "passed")
  expect_identical(by_id$production_baseline$approval$reviewer, "Marc-Olivier Duceppe")
})
