repository_health_source_root <- function() {
  required <- c(
    "DESCRIPTION", "LICENSE", "docs/ROADMAP.md", "inst/doc/ROADMAP.md",
    "docs/developer/ROADMAP_v0.10.md", "scripts/validate_license_metadata.R",
    "inst/metadata/software-identity.json"
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

require_repository_health_root <- function() {
  root <- repository_health_source_root()
  if (is.na(root)) {
    testthat::skip("Repository-only health contracts are unavailable in the built source package")
  }
  root
}

run_license_metadata_validator <- function(root, evidence_path = tempfile(fileext = ".json")) {
  rscript <- file.path(R.home("bin"), paste0("Rscript", if (.Platform$OS.type == "windows") ".exe" else ""))
  output <- suppressWarnings(system2(
    rscript,
    shQuote(file.path(root, "scripts", "validate_license_metadata.R")),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("R_METADATA_ROOT=", root),
      paste0("POPGENVCF_LICENSE_METADATA_EVIDENCE=", evidence_path)
    )
  ))
  list(
    status = attr(output, "status") %||% 0L,
    output = output,
    evidence_path = evidence_path
  )
}

test_that("LICENSE matches the canonical software identity", {
  root <- require_repository_health_root()
  identity <- jsonlite::read_json(
    file.path(root, "inst", "metadata", "software-identity.json"),
    simplifyVector = TRUE
  )
  license <- read.dcf(file.path(root, "LICENSE"))

  expect_identical(unname(license[1L, "YEAR"]), as.character(identity$citation_year))
  expect_identical(
    unname(license[1L, "COPYRIGHT HOLDER"]),
    paste(identity$author$given_name, identity$author$family_name)
  )

  validation <- run_license_metadata_validator(root)
  expect_identical(validation$status, 0L)
  expect_true(file.exists(validation$evidence_path))
  evidence <- jsonlite::read_json(validation$evidence_path, simplifyVector = TRUE)
  expect_true(evidence$passed)
})

test_that("LICENSE metadata validation fails closed on drift", {
  root <- require_repository_health_root()
  fixture <- tempfile("popgenvcf-license-fixture-")
  dir.create(file.path(fixture, "scripts"), recursive = TRUE)
  dir.create(file.path(fixture, "inst", "metadata"), recursive = TRUE)

  expect_true(file.copy(
    file.path(root, "scripts", "validate_license_metadata.R"),
    file.path(fixture, "scripts", "validate_license_metadata.R")
  ))
  expect_true(file.copy(
    file.path(root, "inst", "metadata", "software-identity.json"),
    file.path(fixture, "inst", "metadata", "software-identity.json")
  ))
  writeLines(
    c("YEAR: 1900", "COPYRIGHT HOLDER: Incorrect Holder"),
    file.path(fixture, "LICENSE")
  )

  validation <- run_license_metadata_validator(fixture)
  expect_gt(validation$status, 0L)
  expect_true(any(grepl("LICENSE metadata validation failed", validation$output, fixed = TRUE)))
})

test_that("only the authoritative roadmap represents current project state", {
  root <- require_repository_health_root()
  source_roadmap <- readLines(file.path(root, "docs", "ROADMAP.md"), warn = FALSE)
  installed_roadmap <- readLines(file.path(root, "inst", "doc", "ROADMAP.md"), warn = FALSE)
  archived <- readLines(
    file.path(root, "docs", "developer", "ROADMAP_v0.10.md"),
    warn = FALSE
  )

  expect_identical(source_roadmap, installed_roadmap)
  expect_true(any(grepl("Status: archived", archived, fixed = TRUE)))
  expect_true(any(grepl("docs/ROADMAP.md", archived, fixed = TRUE)))
  expect_false(any(grepl("## 4. Current work", archived, fixed = TRUE)))
  expect_false(any(grepl("The current objective is", archived, fixed = TRUE)))
})
