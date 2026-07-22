workflow_pin_source_root <- function() {
  required <- c(
    ".github/workflows",
    ".github/dependabot.yml",
    "scripts/validate_workflow_action_pins.py"
  )

  is_root <- function(path) {
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required)))
  }

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
  bases <- unique(c(workspace, ancestors(test_dir), ancestors(working_dir)))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(
    bases,
    file.path(bases, "popgenVCF"),
    file.path(bases, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(candidates, is_root, logical(1L))]
  if (!length(matches)) return(NA_character_)
  matches[[1L]]
}

require_workflow_pin_root <- function() {
  root <- workflow_pin_source_root()
  if (is.na(root)) {
    testthat::skip("Repository-only workflow files are unavailable in the built source package")
  }
  root
}

test_that("external GitHub Actions are pinned to immutable commits", {
  root <- require_workflow_pin_root()
  workflows <- list.files(
    file.path(root, ".github", "workflows"),
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )
  expect_gt(length(workflows), 0L)

  uses_lines <- unlist(lapply(workflows, function(path) {
    lines <- readLines(path, warn = FALSE)
    lines[grepl("^[[:space:]]*uses:[[:space:]]*", lines)]
  }), use.names = FALSE)
  expect_gt(length(uses_lines), 0L)

  external <- uses_lines[!grepl(
    "uses:[[:space:]]*(\\./|docker://)",
    uses_lines,
    perl = TRUE
  )]
  expect_gt(length(external), 0L)
  expect_true(all(grepl(
    "@[0-9a-f]{40}[[:space:]]+#[[:space:]]+v[0-9]+(?:\\.[0-9]+){0,2}[[:space:]]*$",
    external,
    perl = TRUE
  )))
  expect_false(any(grepl("@(main|master|latest|v[0-9]+)([[:space:]#]|$)", external, perl = TRUE)))
})

test_that("workflow pin audit emits passing deterministic evidence", {
  root <- require_workflow_pin_root()
  python <- Sys.which("python3")
  if (!nzchar(python)) testthat::skip("python3 is unavailable")

  evidence <- tempfile(fileext = ".json")
  output <- suppressWarnings(system2(
    python,
    shQuote(file.path(root, "scripts", "validate_workflow_action_pins.py")),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("POPGENVCF_ACTION_PIN_ROOT=", root),
      paste0("POPGENVCF_ACTION_PIN_EVIDENCE=", evidence)
    )
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L

  expect_identical(status, 0L)
  expect_true(file.exists(evidence))
  audit <- jsonlite::read_json(evidence, simplifyVector = TRUE)
  expect_true(audit$passed)
  expect_gt(audit$workflow_count, 0L)
  expect_gt(audit$external_reference_count, 0L)
  expect_length(audit$findings, 0L)
})
