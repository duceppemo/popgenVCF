rc_impl_path <- function() {
  installed <- system.file("scripts", "build_release_candidate_dossier.R", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  testthat::test_path("..", "..", "inst", "scripts", "build_release_candidate_dossier.R")
}

rc_policy_path <- function() {
  installed <- system.file("metadata", "release-candidate-policy.json", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  testthat::test_path("..", "..", "inst", "metadata", "release-candidate-policy.json")
}

rc_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(rc_impl_path(), envir = env)
  env
}

rc_fixture <- function(env, mode = "production", blocked = NULL,
                       omit_gate = NULL, omit_approval = NULL) {
  root <- tempfile("popgenvcf-rc-")
  evidence <- file.path(root, "evidence")
  dir.create(evidence, recursive = TRUE)
  policy_path <- rc_policy_path()
  policy <- env$read_release_candidate_policy(policy_path)

  records <- lapply(seq_len(nrow(policy$gate_table)), function(i) {
    gate <- policy$gate_table[i, , drop = FALSE]
    rel <- paste0(sprintf("%02d", gate$order), "-", gate$gate_id, ".txt")
    abs <- file.path(evidence, rel)
    writeLines(paste("verified evidence for", gate$gate_id), abs, useBytes = TRUE)
    status <- if (identical(gate$gate_id, blocked)) "blocked" else "passed"
    artifacts <- if (status == "passed") list(list(
      path = rel,
      size_bytes = as.numeric(file.info(abs)$size),
      sha256 = digest::digest(abs, algo = "sha256", file = TRUE)
    )) else list()
    approval <- NULL
    if (isTRUE(gate$approval_required) && !identical(gate$gate_id, omit_approval)) {
      approval <- if (status == "passed") {
        list(state = "approved", reviewer = "Scientific Reviewer",
             reviewed_at = "2026-07-22", notes = "Reviewed retained evidence.")
      } else {
        list(state = "pending", notes = "Approval pending.")
      }
    }
    list(
      gate_id = gate$gate_id,
      status = status,
      summary = if (status == "passed") "Evidence is complete." else "Evidence is incomplete.",
      artifacts = artifacts,
      approval = approval
    )
  })
  if (!is.null(omit_gate)) {
    records <- records[vapply(records, function(x) !identical(x$gate_id, omit_gate), logical(1L))]
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
  index_path <- file.path(root, "release-candidate-evidence-index.json")
  jsonlite::write_json(index, index_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  list(root = root, evidence = evidence, policy = policy,
       policy_path = policy_path, index_path = index_path)
}

test_that("complete approved production evidence is deterministic and ready", {
  env <- rc_env()
  fixture <- rc_fixture(env)
  result <- env$evaluate_release_candidate_dossier(
    fixture$policy_path, fixture$index_path, fixture$evidence
  )
  expect_true(result$release_ready)
  expect_equal(nrow(result$blockers), 0L)
  expect_equal(nrow(result$artifacts), nrow(fixture$policy$gate_table))

  dirs <- file.path(fixture$root, c("dossier-a", "dossier-b"))
  env$write_release_candidate_dossier(result, dirs[[1L]])
  env$write_release_candidate_dossier(result, dirs[[2L]])
  files <- c("release-candidate-gates.tsv", "release-candidate-blockers.tsv",
             "release-candidate-artifacts.tsv", "release-candidate-dossier.json",
             "release-candidate-readiness.md", "release-candidate-SHA256SUMS.txt")
  expect_identical(
    lapply(file.path(dirs[[1L]], files), readBin, what = "raw", n = 1e7),
    lapply(file.path(dirs[[2L]], files), readBin, what = "raw", n = 1e7)
  )
  expect_true(env$verify_release_candidate_dossier(dirs[[1L]]))
})

test_that("rehearsal and incomplete production evidence remain blocked", {
  env <- rc_env()
  rehearsal <- rc_fixture(env, mode = "rehearsal")
  result <- env$evaluate_release_candidate_dossier(
    rehearsal$policy_path, rehearsal$index_path, rehearsal$evidence
  )
  expect_false(result$release_ready)
  expect_true(any(result$blockers$gate_id == "evaluation_mode"))

  incomplete <- rc_fixture(env, blocked = "production_baseline")
  result <- env$evaluate_release_candidate_dossier(
    incomplete$policy_path, incomplete$index_path, incomplete$evidence
  )
  expect_false(result$release_ready)
  expect_true(any(result$blockers$gate_id == "production_baseline"))
})

test_that("approval, inventory, and artifact defects fail closed", {
  env <- rc_env()
  missing_approval <- rc_fixture(env, omit_approval = "scientific_approval")
  expect_error(
    env$evaluate_release_candidate_dossier(
      missing_approval$policy_path, missing_approval$index_path, missing_approval$evidence
    ),
    "requires approval metadata"
  )

  missing_gate <- rc_fixture(env, omit_gate = "external_concordance")
  expect_error(
    env$evaluate_release_candidate_dossier(
      missing_gate$policy_path, missing_gate$index_path, missing_gate$evidence
    ),
    "gate inventory mismatch"
  )

  tampered <- rc_fixture(env)
  cat("\ntampered\n", file = file.path(tampered$evidence, "01-metadata_consistency.txt"), append = TRUE)
  expect_error(
    env$evaluate_release_candidate_dossier(
      tampered$policy_path, tampered$index_path, tampered$evidence
    ),
    "size mismatch|checksum mismatch"
  )
})

rc_source_root <- function() {
  required <- c("DESCRIPTION", "docs/developer/release-candidate-closure.md",
                "docs/user/ancestry-backends.md",
                "inst/metadata/release-candidate-policy.json")
  ancestors <- function(path) {
    out <- character()
    repeat {
      out <- c(out, path)
      parent <- dirname(path)
      if (identical(parent, path)) break
      path <- parent
    }
    out
  }
  bases <- unique(c(Sys.getenv("GITHUB_WORKSPACE", unset = ""),
                    ancestors(normalizePath(testthat::test_path(), mustWork = TRUE)),
                    ancestors(normalizePath(getwd(), mustWork = TRUE))))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(bases, file.path(bases, "popgenVCF"),
                         file.path(bases, "00_pkg_src", "popgenVCF")))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(candidates, function(x) {
    dir.exists(x) && all(file.exists(file.path(x, required)))
  }, logical(1L))]
  if (length(matches)) matches[[1L]] else NA_character_
}

test_that("closure and ancestry operator documentation is retained", {
  root <- rc_source_root()
  if (is.na(root)) testthat::skip("Repository-only closure documentation is unavailable")
  closure <- readLines(file.path(root, "docs/developer/release-candidate-closure.md"), warn = FALSE)
  ancestry <- readLines(file.path(root, "docs/user/ancestry-backends.md"), warn = FALSE)
  expect_true(all(c("rehearsal", "production", "release-candidate-SHA256SUMS.txt") %in%
                    unlist(lapply(c("rehearsal", "production", "release-candidate-SHA256SUMS.txt"),
                                  function(x) x[any(grepl(x, closure, fixed = TRUE))]))))
  for (term in c("ADMIXTURE", "fastStructure", "LEA/sNMF", "q_sample_file")) {
    expect_true(any(grepl(term, ancestry, fixed = TRUE)))
  }
})
