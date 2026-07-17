test_that("reporting checklist identities are deterministic", {
  first <- generic_reporting_checklist()
  second <- generic_reporting_checklist()

  expect_s3_class(first, "PopgenVCFReportingChecklist")
  expect_identical(first$digest, second$digest)
  expect_true(validate_reporting_checklist(first))
  expect_identical(first$items$item_id, second$items$item_id)
})

test_that("reporting checklist items reject malformed definitions", {
  items <- data.frame(
    item_id = c("duplicate", "duplicate"),
    category = c("methods", "methods"),
    label = c("One", "Two"),
    requirement = c("required", "recommended"),
    guidance = c("One", "Two")
  )
  expect_error(new_reporting_checklist("bad", items), "item_id values must be unique")

  items$item_id <- c("valid", "Invalid ID")
  expect_error(new_reporting_checklist("bad", items), "lowercase letters")

  items$item_id <- c("valid", "also-valid")
  items$requirement[[2L]] <- "optional"
  expect_error(new_reporting_checklist("bad", items), "required or recommended")
})

test_that("verified checklists require source metadata", {
  items <- generic_reporting_checklist()$items
  expect_error(
    new_reporting_checklist("verified", items, status = "verified"),
    "require source_url and source_date"
  )

  checklist <- new_reporting_checklist(
    "verified", items, status = "verified",
    source_url = "https://example.org/checklist",
    source_date = "2026-07-17"
  )
  expect_true(validate_reporting_checklist(checklist))
})

test_that("responses require explicit evidence and rationales", {
  checklist <- generic_reporting_checklist()
  responses <- data.frame(
    item_id = c("samples.identity", "samples.grouping", "variants.filtering"),
    response = c("yes", "not_applicable", "yes"),
    evidence = c("Methods: Samples", "", ""),
    notes = c("", "No grouping was used", "")
  )
  report <- validate_reporting_checklist_responses(checklist, responses)

  expect_identical(report[item_id == "samples.identity", status], "pass")
  expect_identical(report[item_id == "samples.grouping", status], "not_applicable")
  expect_identical(report[item_id == "variants.filtering", status], "incomplete")
  expect_true(all(report$response[!report$item_id %in% responses$item_id] == "unanswered"))
  expect_error(
    validate_reporting_checklist_responses(checklist, responses, strict = TRUE),
    "Required reporting checklist items are incomplete"
  )
})

test_that("responses reject duplicates, unknown items, and invalid states", {
  checklist <- generic_reporting_checklist()
  expect_error(
    validate_reporting_checklist_responses(checklist, data.frame(item_id = c("samples.identity", "samples.identity"), response = c("yes", "yes"))),
    "at most one row"
  )
  expect_error(
    validate_reporting_checklist_responses(checklist, data.frame(item_id = "unknown", response = "yes")),
    "unknown item_id"
  )
  expect_error(
    validate_reporting_checklist_responses(checklist, data.frame(item_id = "samples.identity", response = "maybe")),
    "response must be"
  )
})

test_that("reporting checklist bundles are deterministic and tamper evident", {
  checklist <- generic_reporting_checklist()
  responses <- data.frame(
    item_id = checklist$items$item_id,
    response = "yes",
    evidence = paste("Evidence for", checklist$items$item_id),
    notes = ""
  )
  root <- tempfile("reporting-checklist-")
  dir.create(root)
  out <- write_reporting_checklist(checklist, root, responses)

  expect_true(validate_reporting_checklist(out))
  expect_true(all(file.exists(file.path(out, c(
    "reporting-checklist.json", "reporting-checklist.md",
    "reporting-checklist-items.tsv", "reporting-checklist-manifest.tsv"
  )))))
  expect_error(write_reporting_checklist(checklist, root, responses), "already exists")

  write("tampered", file.path(out, "reporting-checklist.md"), append = TRUE)
  expect_error(validate_reporting_checklist(out), "checksum mismatch")
})

test_that("rendered reporting checklists are stable", {
  checklist <- generic_reporting_checklist()
  first <- render_reporting_checklist(checklist)
  second <- render_reporting_checklist(checklist)
  expect_identical(first, second)
  expect_match(first[[1L]], "Generic population-genomics reporting checklist", fixed = TRUE)
  expect_true(any(grepl("samples.identity", first, fixed = TRUE)))
})
