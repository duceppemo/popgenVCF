workspace_result_fixture <- function(retained = FALSE) {
  command <- new_external_command(
    executable = R.home("bin/R"),
    args = "--version",
    working_directory = tempdir(),
    label = "workspace-fixture"
  )
  result <- new_external_process_result(
    command = command,
    status = "success",
    exit_status = 0L,
    stdout = "",
    stderr = "",
    started_at = "2026-07-19 15:00:00 UTC",
    finished_at = "2026-07-19 15:00:01 UTC",
    elapsed_seconds = 1,
    resolved_executable = normalizePath(R.home("bin/R"), mustWork = TRUE)
  )
  identifier <- digest::digest("workspace", algo = "sha256")
  result$workspace <- list(
    policy = "test-policy",
    identifier = identifier,
    path = if (retained) file.path(tempdir(), "retained-workspace") else NA_character_,
    retained = retained,
    input_manifest = data.table::data.table(
      source = "/input/sample.vcf",
      staged_name = "sample.vcf",
      sha256 = digest::digest("input", algo = "sha256")
    ),
    contents_fingerprint = digest::digest("contents", algo = "sha256"),
    events = data.table::data.table(
      sequence = 1:5,
      event = c(
        "workspace_created", "inputs_staged", "process_dispatched",
        "process_completed",
        if (retained) "workspace_retained" else "workspace_cleaned"
      ),
      detail = c("popgenvcf-workspace", "1", "workspace-fixture", "success",
                 if (retained) "success" else "completed")
    )
  )
  result$original_command_fingerprint <- command$fingerprint
  result
}

test_that("workspace records validate provenance and lifecycle invariants", {
  workspace <- new_external_process_workspace(workspace_result_fixture())
  expect_s3_class(workspace, "PopgenVCFExternalProcessWorkspace")
  expect_invisible(validate_external_process_workspace(workspace))

  bad_events <- workspace
  bad_events$events$sequence[[5]] <- 7L
  expect_error(
    validate_external_process_workspace(bad_events),
    "event sequence must be contiguous"
  )

  conflicting <- workspace
  conflicting$retained <- TRUE
  expect_error(
    validate_external_process_workspace(conflicting),
    "retained workspace must record a path"
  )

  bad_manifest <- workspace
  bad_manifest$input_manifest$sha256[[1]] <- "invalid"
  expect_error(
    validate_external_process_workspace(bad_manifest),
    "invalid SHA-256 digests"
  )
})

test_that("workspace serialization is deterministic", {
  workspace <- new_external_process_workspace(workspace_result_fixture())
  first <- tempfile(fileext = ".rds")
  second <- tempfile(fileext = ".rds")
  on.exit(unlink(c(first, paste0(first, ".sha256"), second,
                   paste0(second, ".sha256"))), add = TRUE)

  write_external_process_workspace(workspace, first)
  write_external_process_workspace(workspace, second)

  expect_identical(
    readBin(first, "raw", n = file.info(first)$size),
    readBin(second, "raw", n = file.info(second)$size)
  )
  restored <- read_external_process_workspace(first)
  expect_s3_class(restored, "PopgenVCFExternalProcessWorkspace")
  expect_identical(restored, workspace)
})

test_that("workspace readers fail closed", {
  workspace <- new_external_process_workspace(workspace_result_fixture())
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".sha256"))), add = TRUE)
  write_external_process_workspace(workspace, path)

  writeLines("malformed", paste0(path, ".sha256"))
  expect_error(read_external_process_workspace(path), "sidecar is malformed")

  write_external_process_workspace(workspace, path, overwrite = TRUE)
  envelope <- readRDS(path)
  envelope$payload$identifier <- digest::digest("mutated", algo = "sha256")
  saveRDS(envelope, path, version = 3, compress = "gzip")
  writeLines(
    paste(external_process_workspace_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(
    read_external_process_workspace(path),
    "runtime integrity digest mismatch"
  )

  saveRDS(workspace, path, version = 3, compress = "gzip")
  writeLines(
    paste(external_process_workspace_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(read_external_process_workspace(path), "explicit migration")
})

test_that("a workspace cleaned up after success can still be validated, persisted and read back", {
  # The default policy removes the workspace on success, and the result's
  # command points at that (now missing) directory. Validation rebuilt the
  # command through the public constructor, which demands the directory, so
  # every real workspace-backed result failed with "working_directory must
  # be an existing directory" -- only hand-built fixtures ever passed.
  skip_on_os("windows")
  source_dir <- tempfile("workspace-source-"); dir.create(source_dir)
  root <- tempfile("workspace-root-"); dir.create(root)
  writeLines("1", file.path(source_dir, "B.txt")); writeLines("2", file.path(source_dir, "a.txt"))
  result <- run_supervised_external_command_in_workspace(
    new_external_command("/bin/true", working_directory = source_dir),
    inputs = file.path(source_dir, c("B.txt", "a.txt")),
    workspace_policy = new_external_process_workspace_policy(root = root)
  )
  expect_false(result$workspace$retained)
  expect_false(dir.exists(result$command$working_directory))
  expect_silent(validate_external_process_result(result))
  # Byte order, whatever the session's collation.
  expect_identical(result$workspace$input_manifest$staged_name, c("B.txt", "a.txt"))
  path <- tempfile(fileext = ".rds")
  write_external_process_workspace(result, path)
  expect_s3_class(read_external_process_workspace(path), "PopgenVCFExternalProcessWorkspace")
  result_path <- tempfile(fileext = ".rds")
  write_external_process_result(result, result_path)
  expect_identical(read_external_process_result(result_path)$status, "success")
})

test_that("launching a command whose working directory has gone reports launch_failed", {
  skip_on_os("windows")
  gone <- tempfile("gone-"); dir.create(gone)
  command <- new_external_command("/bin/true", working_directory = gone)
  unlink(gone, recursive = TRUE)
  before <- getwd()
  results <- list(
    run_external_command(command),
    run_supervised_external_command(command),
    finalize_supervised_external_command(start_supervised_external_command(command))
  )
  for (result in results) {
    expect_identical(result$status, "launch_failed")
    expect_match(result$error_message, "Working directory does not exist", fixed = TRUE)
  }
  expect_identical(getwd(), before)
  expect_error(new_external_command("/bin/true", working_directory = gone), "existing directory")
})
