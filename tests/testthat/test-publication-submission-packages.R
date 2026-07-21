test_that("submission package contracts are deterministic and bound", {
  journal <- generic_journal_profile()
  layout <- publication_layout_profile("general")
  style <- publication_figure_style_profile("accessibility-first")
  report <- new_publication_report_spec("paper", formats = "html")

  spec <- new_publication_submission_package_spec(
    "paper-submission", journal, layout, style, report,
    required_roles = c("provenance", "manuscript", "metadata")
  )
  expect_true(validate_publication_submission_package_spec(spec, journal, layout, style, report))
  expect_identical(
    spec$fingerprint,
    new_publication_submission_package_spec(
      "paper-submission", journal, layout, style, report,
      required_roles = c("metadata", "manuscript", "provenance")
    )$fingerprint
  )

  files <- list(
    list(path = "submission/provenance.json", role = "provenance", media_type = "application/json", size_bytes = 30, sha256 = "p", source_fingerprint = "source-p"),
    list(path = "submission/manuscript.html", role = "manuscript", media_type = "text/html", size_bytes = 100, sha256 = "m", source_fingerprint = "source-m"),
    list(path = "submission/metadata.json", role = "metadata", media_type = "application/json", size_bytes = 20, sha256 = "d", source_fingerprint = "source-d"),
    list(path = "submission/supplement/s1.csv", role = "supplement", media_type = "text/csv", size_bytes = 10, sha256 = "s", source_fingerprint = "source-s")
  )
  supplements <- new_publication_supplementary_index(list(
    list(path = "submission/supplement/s1.csv", role = "supplement", media_type = "text/csv", size_bytes = 10, sha256 = "s", source_fingerprint = "source-s", label = "Table S1", title = "Source data", manuscript_reference = "Results")
  ))
  manifest <- new_publication_submission_package_manifest(
    spec, rev(files), supplements, "output-fingerprint", "execution-fingerprint"
  )
  expect_true(validate_publication_submission_package_manifest(manifest, spec, supplements))
  expect_identical(manifest$files, manifest$files[order(vapply(manifest$files, `[[`, character(1), "path"))])
  expect_match(publication_submission_package_report(manifest)[1], "Submission package")
})

test_that("submission packages fail closed on invalid composition and mutation", {
  journal <- generic_journal_profile()
  layout <- publication_layout_profile("general")
  style <- publication_figure_style_profile("standard-color")
  report <- new_publication_report_spec("paper", formats = "pdf")
  spec <- new_publication_submission_package_spec("paper", journal, layout, style, report)

  incomplete <- list(
    list(path = "manuscript.pdf", role = "manuscript", media_type = "application/pdf", size_bytes = 1, sha256 = "m", source_fingerprint = "source")
  )
  expect_error(
    new_publication_submission_package_manifest(spec, incomplete, output_manifest_fingerprint = "o", execution_fingerprint = "e"),
    "missing required role"
  )

  duplicate <- list(
    list(path = "same.csv", role = "supplement", media_type = "text/csv", size_bytes = 1, sha256 = "a", source_fingerprint = "a", label = "S1", title = "One", manuscript_reference = "Results"),
    list(path = "same.csv", role = "supplement", media_type = "text/csv", size_bytes = 1, sha256 = "b", source_fingerprint = "b", label = "S2", title = "Two", manuscript_reference = "Methods")
  )
  expect_error(new_publication_supplementary_index(duplicate), "paths must be unique")

  changed <- spec
  changed$id <- "changed"
  expect_error(
    validate_publication_submission_package_spec(changed, journal, layout, style, report),
    "fingerprint mismatch"
  )

  orphan <- new_publication_supplementary_index(list(
    list(path = "missing.csv", role = "supplement", media_type = "text/csv", size_bytes = 1, sha256 = "x", source_fingerprint = "x", label = "S1", title = "Missing", manuscript_reference = "Results")
  ))
  complete <- c(incomplete, list(
    list(path = "metadata.json", role = "metadata", media_type = "application/json", size_bytes = 1, sha256 = "d", source_fingerprint = "d"),
    list(path = "provenance.json", role = "provenance", media_type = "application/json", size_bytes = 1, sha256 = "p", source_fingerprint = "p")
  ))
  expect_error(
    new_publication_submission_package_manifest(spec, complete, orphan, "o", "e"),
    "absent from the package manifest"
  )
})
