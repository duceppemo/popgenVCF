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

test_that("evidence artifacts must not alias the same underlying content", {
  env <- rc_env()
  fixture <- rc_fixture(env)
  idx <- jsonlite::read_json(fixture$index_path, simplifyVector = FALSE)
  passed <- Filter(
    function(r) identical(r$status, "passed") && length(r$artifacts) == 1L,
    idx$records
  )
  expect_gte(length(passed), 2L)
  first <- passed[[1L]]
  second <- passed[[2L]]
  first_abs <- file.path(fixture$evidence, first$artifacts[[1L]]$path)
  second_abs <- file.path(fixture$evidence, second$artifacts[[1L]]$path)

  # Make the second gate's evidence file byte-identical to the first's (as a
  # symlink, hard link, or duplicate copy would in practice), and update only
  # its declared checksum/size to match -- so each artifact still passes its
  # own individual integrity check, isolating the new cross-artifact
  # content-duplication check as the one that must fail.
  file.copy(first_abs, second_abs, overwrite = TRUE)
  for (i in seq_along(idx$records)) {
    if (identical(idx$records[[i]]$gate_id, second$gate_id)) {
      idx$records[[i]]$artifacts[[1L]]$sha256 <- digest::digest(second_abs, algo = "sha256", file = TRUE)
      idx$records[[i]]$artifacts[[1L]]$size_bytes <- as.numeric(file.info(second_abs)$size)
    }
  }
  jsonlite::write_json(idx, fixture$index_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")

  expect_error(
    env$evaluate_release_candidate_dossier(
      fixture$policy_path, fixture$index_path, fixture$evidence
    ),
    "must not alias the same underlying content"
  )
})

