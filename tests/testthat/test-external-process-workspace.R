workspace_test_script <- function(lines) {
  path <- tempfile(fileext = ".R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

workspace_rscript <- function() {
  file.path(R.home("bin"), paste0("Rscript", .Platform$exeext))
}

test_that("workspace policies validate", {
  root <- tempfile("workspace-root-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  policy <- new_external_process_workspace_policy(root, FALSE, TRUE, "diagnostic")
  expect_s3_class(policy, "PopgenVCFExternalProcessWorkspacePolicy")
  expect_false(policy$cleanup_on_success)
  expect_true(policy$retain_on_failure)
  expect_error(new_external_process_workspace_policy(tempfile()), "existing directory")
  expect_error(new_external_process_workspace_policy(root, NA), "TRUE or FALSE")
})

test_that("successful commands clean workspaces and record ordered events", {
  root <- tempfile("workspace-root-")
  dir.create(root)
  script <- workspace_test_script(c(
    "cat('workspace-output')",
    "writeLines('artifact', 'artifact.txt')"
  ))
  on.exit(unlink(c(root, script), recursive = TRUE), add = TRUE)

  command <- new_external_command(
    workspace_rscript(),
    args = shQuote(script),
    label = "workspace-success"
  )
  result <- run_supervised_external_command_in_workspace(
    command,
    workspace_policy = new_external_process_workspace_policy(root)
  )

  expect_identical(result$status, "success")
  expect_false(result$workspace$retained)
  expect_true(is.na(result$workspace$path))
  expect_identical(
    result$workspace$events$event,
    c("workspace_created", "inputs_staged", "process_dispatched",
      "process_completed", "workspace_cleaned")
  )
  expect_identical(result$workspace$events$sequence, 1:5)
  expect_match(result$workspace$contents_fingerprint, "^[0-9a-f]{64}$")
  expect_identical(list.files(root), character())
})

test_that("failed commands retain fingerprinted diagnostic workspaces", {
  root <- tempfile("workspace-root-")
  dir.create(root)
  script <- workspace_test_script(c(
    "writeLines('diagnostic', 'failure.txt')",
    "quit(status = 7L)"
  ))
  input <- tempfile("declared-input-", fileext = ".txt")
  writeLines("input", input)
  on.exit(unlink(c(root, script, input), recursive = TRUE), add = TRUE)

  command <- new_external_command(
    workspace_rscript(),
    args = shQuote(script),
    label = "workspace-failure"
  )
  result <- run_supervised_external_command_in_workspace(
    command,
    inputs = input,
    execution_label = "replicate-001",
    workspace_policy = new_external_process_workspace_policy(root)
  )

  expect_identical(result$status, "nonzero_exit")
  expect_true(result$workspace$retained)
  expect_true(dir.exists(result$workspace$path))
  expect_true(file.exists(file.path(result$workspace$path, basename(input))))
  expect_true(file.exists(file.path(result$workspace$path, "failure.txt")))
  expect_identical(tail(result$workspace$events$event, 1), "workspace_retained")
  expect_match(result$workspace$identifier, "^[0-9a-f]{64}$")
  expect_match(result$workspace$contents_fingerprint, "^[0-9a-f]{64}$")
  expect_identical(nrow(result$workspace$input_manifest), 1L)
})

test_that("workspace identity is deterministic and staging fails closed", {
  root <- tempfile("workspace-root-")
  dir.create(root)
  script <- workspace_test_script("cat('ok')")
  input <- tempfile("input-", fileext = ".txt")
  writeLines("stable", input)
  on.exit(unlink(c(root, script, input), recursive = TRUE), add = TRUE)

  command <- new_external_command(workspace_rscript(), shQuote(script), label = "stable")
  policy <- new_external_process_workspace_policy(
    root, cleanup_on_success = FALSE, retain_on_failure = TRUE
  )
  first <- run_supervised_external_command_in_workspace(
    command, inputs = input, execution_label = "same", workspace_policy = policy
  )
  expect_error(
    run_supervised_external_command_in_workspace(
      command, inputs = input, execution_label = "same", workspace_policy = policy
    ),
    "already exists"
  )
  expect_error(
    run_supervised_external_command_in_workspace(
      command, inputs = tempfile(), execution_label = "missing", workspace_policy = policy
    ),
    "existing regular files"
  )
  expect_true(first$workspace$retained)
})
