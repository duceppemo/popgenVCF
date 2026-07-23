release_candidate_implementation <- function() {
  installed <- system.file(
    "scripts", "build_release_candidate_dossier.R",
    package = "popgenVCF"
  )
  if (nzchar(installed)) return(installed)
  testthat::test_path(
    "..", "..", "inst", "scripts", "build_release_candidate_dossier.R"
  )
}

release_candidate_policy_path <- function() {
  installed <- system.file(
    "metadata", "release-candidate-policy.json",
    packae = "popgenVCF"
  )
  if (nzchar(installed)) return(installed)
  testthat::test_path(
    "..", "..", "inst", "metadata", "release-candidate-policy.json"
  )
}

release_candidate_environment <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(release_candidate_implementation(), envir = environment)
  environment
}

write_release_candidate_fixture <- function(
    environment,
    mode = "production",
    blocked_gate = NULL,
    omit_gate = NULL,
    omit_approval = NULL) {
  fixture <- tempfile("popgenvcf-release-candidate-")
  evidence_root <- file.path(fixture, "evidence")
  dir.create(evidence_root, recursive = TRUE)
  policy_path <- release_candidate_policy_path()
  policy <- environment$read_release_candidate_policy(policy_path)

  records <- lapply(seq_len(nrow(policy$gate_table), function(i) {
    gate <- policy$gate_table[i, , drop = FALSE]
    artifact_path <- paste0(sprintf("%02d", gate$order), "-", gate$gate_id, ".txt")
    artifact_absolute <- file.path(evidence_root, artifact_path)
    writeLines(
      paste("verified evidence for", gate$gate_id),
      artifact_absolute,
      useBytes = TRUE
   )
    status <- if (identical(gate$gate_id, blocked_gate)) "blocked" else "passed"
    artifacts <- if (identical(status, "passed")) {
      list(list(
        path = artifact_path,
        size_bytes = as.numeric(file.info(artifact_absolute)$size),
        sha256 = digest::digest(artifact_absolute, algo = "sha256", file = TRUE)
      ))
    } else {
      list()
    }
    approval <- if (isTRUE(gate$approval_required) &&
                    !identical(gate$gate_id, omit_approval)) {
      if (identical(status, "passed")) {
        list(
          state = "approved",
          reviewer = "Scientific Reviewer",
          reviewed_at = "2026-07-22",
          notes = "Reviewed against the retained evidence."
        )
      } else {
        list(
          state = "pending",
          notes = "Approval is pending."
        )
      }
    } else {
      NULL
    }
    list(
      gate_id = gate$gate_id,
      status = status,
      summary = if (identical(status, "passed")) {
        "Evidence is complete and verified."
      } else {
        "Production evidence remains incomplete."
      },
      artifacts = artifacts,
      approval = approval
    )
  })
  if (!is.null(omit_gate)) {
    records <- records[vapply(
      records,
      function(record) !identical(record$gate_id, omit_gate),
      logical(1L)
    )]
  }

  index <- list(
    schema_version = "1.0",
    mode = mode,
    candidate_id = "0.10.0-rc1",
    target_release = "v0.10.0",
    package_version = "0.10.0",
    git_commit = paste(rep("a", 40L), collapse = ""),
    evaluated_at = "2026-07-22T23:59:59Z",
    records = records
  )
  index_path <- file.path(fixture, "release-candidate-evidence-index.json")
  jsonlite::write_json(
    index,
    index_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  list(
    fixture = fixture,
    evidence_root = evidence_root,
    policy_path = policy_path,
    index_path = index_path,
    index = index,
    policy = policy
  )
}

test_that("complete approved production evidence yields a deterministic ready dossier", {
  environment <- release_candidate_environment()
  fixture <- write_release_candidate_fixture(environment)
  result <- environment$evaluate_release_candidate_dossier(
    fixture$policy_path,
    fixture$index_path,
    fixture$evidence_root
  )
  expect_true(result$release_ready)
  expect_equal(nrow(result$blockers), 0L)
  expect_equal(
    nrow(result$artifacts),
    nrow(fixture$policy$gate_table)
  )

  first <- file.path(fixture$fixture, "dossier-a")
  second <- file.path(fixture$fixture, "dossier-b")
  environment$write_release_candidate_dossier(result, first)
  environment$write_release_candidate_dossier(result, second)
  files <- c(
    "release-candidate-gates.tsv",
    "release-candidate-blockers.tsv",
    "release-candidate-artifacts.tsv",
    "release-candidate-dossier.json",
    "release-candidate-readiness.md",
    "release-candidate-SHA256SUMS.txt"
  )
  expect_identical(
    lapply(file.path(first, files), readBin, what = "raw", n = 1e7),
    lapply(file.path(second, files), readBin, what = "raw", n = 1e7)
  )
  expect_true(environment$verify_release_candidate_dossier(first))
})

test_that("rehearsal and incomplete production evidence remain blocked", {
  environment <- release_candidate_environment()
  rehearsal <- write_release_candidate_fixture(
    environment,
    mode = "rehearsal"
  )
  rehearsal_result <- environment$evaluate_release_candidate_dossier(
    rehearsal$policy_path,
    rehearsal$index_path,
    rehearsal$evidence_root
  )
  expect_false(rehearsal_result$release_ready)
  expect_true(any(rehearsal_result$blockers$gate_id == "evaluation_mode"))

  blocked <- write_release_candidate_fixture(
    environment,
    blocked_gate = "production_baseline"
  )
  blocked_result <- environment$evaluate_release_candidate_dossier(
    blocked$policy_path,
    blocked$index_path,
    blocked$evidence_root
  )
  expect_false(blocked_result$release_ready)
  expect_true(any(blocked_result$blockers$gate_id == "production_baseline"))
})

test_that("release-candidate evidence fails closed on approval and inventory defects", {
  environment <- release_candidate_environment()
  missing_approval <- write_release_candidate_fixture(
    environment,
    omit_approval = "scientific_approval"
  )
  expect_error(
    environment$evaluate_release_candidate_dossier(
      missing_approval$policy_path,
      missing_approval$index_path,
      missing_approval$evidence_root
    ),
    "requires approval metadata"
  )

  missing_gate <- write_release_candidate_fixture(
    environment,
    omit_gate = "external_concordance"
  )
  expect_error(
    environment$evaluate_release_candidate_dossier(
      missing_gate$policy_path,
      missing_gate$index_path,
      missing_gate$evidence_root
    ),
    "gate inventory mismatch"
  )
})

test_that("release-candidate evidence detects artifact tampering", {
  environment <- release_candidate_environment()
  fixture <- write_release_candidate_fixture(environment)
  tampered <- file.path(
    fixture$evidence_root,
    "01-metadata_consistency.txt"
  )
  cat("\ntampered\n", file = tampered, append = TRUE)
  expect_error(
    environment$evaluate_release_candidate_dossier(
      fixture$policy_path,
      fixture$index_path,
      fixture$evidence_root
    ),
    "size mismatch|checksum mismatch"
  )
})


release_candidate_source_root <- function() {
  required <- c(
    "DESCRIPTION",
    "docs/developer/release-candidate-closure.md",
    "docs/user/ancestry-backends.md",
    "inst/metadata/release-candidate-policy.json"
  )
  ancestors <- function(path) {
    out <- character()
    current <- path
    repeat {
      out <- c(out, current)
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
    out
  }
  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  test_dir <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  working_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(workspace, ancestors(test_dir), ancestors(working_dir)))
  candidates <- candidates[nzchar(candidates)]
  candidates <- unique(c(
    candidates,
    file.path(candidates, "popgenVCF"),
    file.path(candidates, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(
    candidates,
    function(path) dir.exists(path) && all(file.exists(file.path(path, required))),
    logical(1L)
  )]
  if (!length(matches)) return(NA_character_)
  matches[[1L]]
}

test_that("release-candidate and ancestry operator documentation is retained", {
  source_root <- release_candidate_source_root()
  if (is.na(source_root)) {
    testthat::skip("Repository-only release-candidate documentation is unavailable")
  }
  closure <- readLines(
    file.path(source_root, "docs", "developer", "release-candidate-closure.md"),
    warn = FALSE
  )
  ancestry <- readLines(
    file.path(source_root, "docs", "user", "ancestry-backends.md"),
    warn = FALSE
  )
  expect_true(any(grepl("rehearsal", closure, fixed = TRUE)))
  expect_true(any(grepl("production", closure, fixed = TRUE)))
  expect_true(any(grepl("release-candidate-SHA256SUMS.txt", closure, fixed = TRUE)))
  expect_true(any(grepl("ADMIXTURE", ancestry, fixed = TRUE)))
  expect_true(any(grepl("fastStructure", ancestry, fixed = TRUE)))
  expect_true(any(grepl("LEA/sNMF", ancestry, fixed = TRUE)))
  expect_true(any(grepl("q_sample_file", ancestry, fixed = TRUE)))
})
