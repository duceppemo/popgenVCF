test_that("runtime replay verification links ledgers deterministically", {
  execution <- new_persisted_execution_ledger(data.frame(
    module = c("a", "b"), status = c("success", "failed"), attempt = c(1L, 2L)
  ))
  attempts <- new_attempt_ledger(data.frame(
    module = c("a", "b", "b"),
    status = c("success", "failed", "failed"),
    attempt = c(1L, 1L, 2L)
  ))
  first <- new_runtime_replay_bundle(execution, attempts)
  second <- new_runtime_replay_bundle(execution, attempts)
  expect_true(first$verification$verified)
  expect_identical(first$verification$replay_fingerprint,
                   second$verification$replay_fingerprint)
  expect_equal(first$verification$module_count, 2L)
  expect_equal(first$verification$attempt_count, 3L)
})

test_that("runtime replay verification rejects inconsistent retry histories", {
  execution <- new_persisted_execution_ledger(data.frame(
    module = "a", status = "success", attempt = 1L
  ))
  attempts <- new_attempt_ledger(data.frame(
    module = "a", status = "failed", attempt = 1L
  ))
  expect_error(
    new_runtime_replay_bundle(execution, attempts),
    "final attempt status conflicts"
  )
})

test_that("runtime replay verification rejects different module sets", {
  execution <- new_persisted_execution_ledger(data.frame(
    module = "a", status = "success"
  ))
  attempts <- new_attempt_ledger(data.frame(
    module = "b", status = "success", attempt = 1L
  ))
  expect_error(
    new_runtime_replay_bundle(execution, attempts),
    "different module sets"
  )
})

test_that("runtime replay verification links process results and workspaces", {
  command <- new_external_command("R", "--version", label = "r-version")
  result <- structure(list(
    command = command,
    command_fingerprint = command$fingerprint,
    status = "success", exit_status = 0L, stdout = "", stderr = "",
    started_at = "2026-01-01T00:00:00Z",
    finished_at = "2026-01-01T00:00:01Z", elapsed_seconds = 1,
    resolved_executable = "R", error_message = NA_character_,
    original_command_fingerprint = command$fingerprint
  ), class = "PopgenVCFExternalProcessResult")
  workspace <- structure(list(
    command_fingerprint = command$fingerprint,
    workspace_command_fingerprint = command$fingerprint,
    process_status = "success", policy = "test",
    identifier = paste(rep("a", 64), collapse = ""), path = NA_character_,
    retained = FALSE,
    input_manifest = data.table::data.table(
      source = character(), staged_name = character(), sha256 = character()
    ),
    contents_fingerprint = paste(rep("b", 64), collapse = ""),
    events = data.table::data.table(
      sequence = 1:5,
      event = c("workspace_created", "inputs_staged", "process_dispatched",
                "process_completed", "workspace_cleaned"),
      detail = c("workspace", "0", "r-version", "success", "completed")
    )
  ), class = "PopgenVCFExternalProcessWorkspace")
  execution <- new_persisted_execution_ledger(data.frame(
    module = "a", status = "success"
  ))
  bundle <- new_runtime_replay_bundle(execution,
    process_results = list(result), process_workspaces = list(workspace))
  expect_equal(bundle$verification$process_count, 1L)
  expect_equal(bundle$verification$workspace_count, 1L)

  workspace$process_status <- "nonzero_exit"
  workspace$events$detail[4] <- "nonzero_exit"
  expect_error(
    new_runtime_replay_bundle(execution,
      process_results = list(result), process_workspaces = list(workspace)),
    "status conflicts"
  )
})
