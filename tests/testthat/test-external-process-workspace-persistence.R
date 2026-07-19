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
  saveRDS(envelope, path, version = 3, compress = "xz")
  writeLines(
    paste(external_process_workspace_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(
    read_external_process_workspace(path),
    "runtime integrity digest mismatch"
  )

  saveRDS(workspace, path, version = 3, compress = "xz")
  writeLines(
    paste(external_process_workspace_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(read_external_process_workspace(path), "explicit migration")
})
