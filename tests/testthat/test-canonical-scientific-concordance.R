concordance_result <- function(status = "passed", role = "equivalence") {
  structure(list(
    schema_version = "1.0", id = "pca_plink", analysis = "pca",
    reference_tool = "PLINK 2", reference_version = "2.00a6",
    role = role, mode = "numeric", status = status,
    comparisons = data.table::data.table(metric = "variance", observed = 1,
      reference = 1, absolute_error = 0, relative_error = 0,
      passed = status == "passed"),
    message = "within tolerance", interpretation = "Equivalent PCA variance",
    citations = "Chang et al. 2015"
  ), class = "PopgenVCFExternalReferenceResult")
}

concordance_record <- function(tool = "PLINK 2", analysis = "pca",
                               dataset_id = "1000g_phase3_chry_v2a",
                               version = "1.0", status = "passed",
                               role = "equivalence", approval = "approved") {
  new_scientific_concordance_record(
    dataset_id = dataset_id, analysis = analysis,
    reference_tool = tool, reference_version = version,
    command = paste(tool, "--canonical"), result = concordance_result(status, role),
    tolerance_profile = list(relative = 1e-6, absolute = 1e-8),
    environment = list(platform = "linux", container_digest = "sha256:test"),
    approval = approval,
    approved_by = if (approval == "approved") "scientific reviewer" else NULL,
    approved_at = if (approval == "approved") "2026-07-22" else NULL)
}

test_that("concordance records preserve deterministic provenance", {
  record <- concordance_record()
  expect_equal(names(record$tolerance_profile), c("absolute", "relative"))
  expect_equal(names(record$environment), c("container_digest", "platform"))
  expect_true(record$passed)
  expect_silent(validate_scientific_concordance_record(record, require_approved = TRUE))
})

test_that("proposed and failed equivalence records fail closed", {
  proposed <- concordance_record(approval = "proposed")
  expect_error(validate_scientific_concordance_record(proposed, TRUE), "not approved")
  failed <- concordance_record(status = "failed")
  suite <- new_scientific_concordance_suite(list(failed))
  expect_false(suite$release_ready)
})

test_that("diagnostic differences remain transparent and non-gating", {
  diagnostic <- concordance_record(status = "passed", role = "diagnostic")
  suite <- new_scientific_concordance_suite(list(diagnostic))
  expect_true(suite$release_ready)
  expect_equal(scientific_concordance_table(suite)$role, "diagnostic")
})

test_that("suite inventory completeness is enforced and release gating is fail closed", {
  record <- concordance_record()
  expect_error(new_scientific_concordance_suite(list(record),
    required_tools = c("PLINK 2", "SNPRelate")), "incomplete")
  suite <- new_scientific_concordance_suite(list(record), require_complete = FALSE,
    required_tools = c("plink 2", "SNPRelate"), required_analyses = c("PCA", "FST"))
  expect_equal(suite$missing_tools, "SNPRelate")
  expect_equal(suite$missing_analyses, "fst")
  expect_false(suite$inventory_complete)
  expect_false(suite$release_ready)
  expect_error(write_scientific_concordance_evidence(suite, tempfile(), TRUE),
               "not release ready")
})

test_that("record identity includes dataset and external-tool version", {
  records <- list(
    concordance_record(dataset_id = "dataset_a"),
    concordance_record(dataset_id = "dataset_b"),
    concordance_record(dataset_id = "dataset_a", version = "2.0")
  )
  suite <- new_scientific_concordance_suite(records)
  expect_length(suite$records, 3L)
  expect_error(
    new_scientific_concordance_suite(c(records, list(records[[1L]]))),
    "must be unique"
  )
})

test_that("suite mutation is detected", {
  suite <- new_scientific_concordance_suite(list(concordance_record()))
  suite$release_ready <- FALSE
  expect_error(validate_scientific_concordance_suite(suite), "inconsistent")
})

test_that("evidence is deterministic and release gated", {
  suite <- new_scientific_concordance_suite(list(concordance_record()))
  paths <- write_scientific_concordance_evidence(suite, tempfile(), TRUE)
  expect_true(all(file.exists(paths)))
  table <- data.table::fread(paths[["tsv"]])
  json <- jsonlite::read_json(paths[["json"]], simplifyVector = TRUE)
  expect_true(table$passed)
  expect_true(json$release_ready)
  expect_true(json$inventory_complete)

  blocked <- new_scientific_concordance_suite(list(concordance_record(approval = "proposed")))
  expect_error(write_scientific_concordance_evidence(blocked, tempfile(), TRUE),
               "not release ready")
})
