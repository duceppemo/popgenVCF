test_that("scientific review assignment covers approval-gated result evidence", {
  assignment_path <- system.file(
    "metadata", "scientific-review-assignment.json", package = "popgenVCF"
  )
  assignment <- jsonlite::read_json(assignment_path, simplifyVector = TRUE)

  expect_identical(assignment$schema_version, "1.0")
  expect_identical(assignment$state, "assigned")
  expect_false(assignment$approval_conferred)
  expect_identical(assignment$reviewer$name, "Marc-Olivier Duceppe")
  expect_identical(
    assignment$reviewer$email,
    "marc-olivier.duceppe@inspection.gc.ca"
  )
  expect_identical(assignment$reviewer$orcid, "0000-0003-2130-0427")
  expect_setequal(
    assignment$gate_ids,
    c(
      "production_baseline", "external_concordance",
      "ancestry_three_backend", "benchmark_history", "scientific_approval"
    )
  )
  expect_false("release_authorization" %in% assignment$gate_ids)
})
