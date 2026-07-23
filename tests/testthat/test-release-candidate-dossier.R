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

