zenodo_metadata_source_root <- function() {
  required <- c(
    ".zenodo.json",
    "inst/metadata/software-identity.json",
    "scripts/validate_zenodo_metadata.R"
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

require_zenodo_metadata_root <- function() {
  root <- zenodo_metadata_source_root()
  if (is.na(root)) {
    testthat::skip("Repository-only Zenodo metadata is unavailable in the built source package")
  }
  root
}

run_zenodo_metadata_validator <- function(root, evidence_path = tempfile(fileext = ".json")) {
  rscript <- file.path(
    R.home("bin"),
    paste0("Rscript", if (.Platform$OS.type == "windows") ".exe" else "")
  )
  output <- suppressWarnings(system2(
    rscript,
    shQuote(file.path(root, "scripts", "validate_zenodo_metadata.R")),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("R_METADATA_ROOT=", root),
      paste0("POPGENVCF_ZENODO_METADATA_EVIDENCE=", evidence_path)
    )
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = output, evidence_path = evidence_path)
}

test_that("Zenodo metadata is complete, synchronized, and DOI-free", {
  root <- require_zenodo_metadata_root()
  identity <- jsonlite::read_json(
    file.path(root, "inst", "metadata", "software-identity.json"),
    simplifyVector = TRUE
  )
  zenodo <- jsonlite::read_json(file.path(root, ".zenodo.json"), simplifyVector = FALSE)

  expect_identical(zenodo$title, identity$citation_title)
  expect_identical(zenodo$description, identity$description)
  expect_identical(zenodo$upload_type, "software")
  expect_identical(zenodo$access_right, "open")
  expect_identical(tolower(zenodo$license), tolower(identity$license$spdx))
  expect_identical(zenodo$version, identity$version)
  expect_setequal(unlist(zenodo$keywords, use.names = FALSE), identity$keywords)
  expect_identical(
    zenodo$creators[[1L]]$name,
    paste0(identity$author$family_name, ", ", identity$author$given_name)
  )
  expect_false(any(c(
    "doi", "conceptdoi", "conceptrecid", "recid", "record_id",
    "publication_date", "date_released"
  ) %in% names(zenodo)))

  validation <- run_zenodo_metadata_validator(root)
  expect_identical(validation$status, 0L)
  expect_true(file.exists(validation$evidence_path))
  evidence <- jsonlite::read_json(validation$evidence_path, simplifyVector = TRUE)
  expect_true(evidence$passed)
})

test_that("Zenodo metadata validation fails on premature DOI claims", {
  root <- require_zenodo_metadata_root()
  fixture <- tempfile("popgenvcf-zenodo-fixture-")
  dir.create(file.path(fixture, "scripts"), recursive = TRUE)
  dir.create(file.path(fixture, "inst", "metadata"), recursive = TRUE)

  expect_true(file.copy(
    file.path(root, "scripts", "validate_zenodo_metadata.R"),
    file.path(fixture, "scripts", "validate_zenodo_metadata.R")
  ))
  expect_true(file.copy(
    file.path(root, "inst", "metadata", "software-identity.json"),
    file.path(fixture, "inst", "metadata", "software-identity.json")
  ))
  zenodo <- jsonlite::read_json(file.path(root, ".zenodo.json"), simplifyVector = FALSE)
  zenodo$doi <- "10.0000/unpublished"
  jsonlite::write_json(
    zenodo,
    file.path(fixture, ".zenodo.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  validation <- run_zenodo_metadata_validator(fixture)
  expect_gt(validation$status, 0L)
  expect_true(any(grepl("development Zenodo metadata", validation$output, fixed = TRUE)))
})
