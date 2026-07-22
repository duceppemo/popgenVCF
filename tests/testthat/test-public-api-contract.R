test_that("public API contract snapshots are deterministic", {
  first <- public_api_contract_snapshot("popgenVCF")
  second <- public_api_contract_snapshot("popgenVCF")
  expect_identical(first, second)
  expect_true(all(c("kind", "generic", "class", "symbol", "signature") %in% names(first)))
})

test_that("removed API entries are blocking", {
  baseline <- data.frame(kind = "export", generic = NA_character_, class = NA_character_, symbol = "old_api", signature = "x=<required>")
  current <- baseline[0, , drop = FALSE]
  findings <- compare_public_api_contract(baseline, current)
  expect_true(any(findings$severity == "blocking" & findings$category == "removed-api"))
})

test_that("optional additions are advisory", {
  baseline <- data.frame(kind = "export", generic = NA_character_, class = NA_character_, symbol = "f", signature = "x=<required>")
  current <- baseline
  current$signature <- "x=<required>;verbose=FALSE"
  findings <- compare_public_api_contract(baseline, current)
  expect_true(any(findings$severity == "advisory" & findings$category == "added-optional-argument"))
  expect_false(any(findings$severity == "blocking"))
})

test_that("default changes and required additions are blocking", {
  baseline <- data.frame(kind = "export", generic = NA_character_, class = NA_character_, symbol = "f", signature = "x=<required>;method=\"a\"")
  current <- baseline
  current$signature <- "x=<required>;y=<required>;method=\"b\""
  findings <- compare_public_api_contract(baseline, current)
  expect_true(any(findings$category == "added-required-argument" & findings$severity == "blocking"))
  expect_true(any(findings$category == "changed-default" & findings$severity == "blocking"))
})

test_that("evidence output is byte-identical", {
  dir1 <- tempfile("api-contract-")
  dir2 <- tempfile("api-contract-")
  write_public_api_contract(dir1)
  write_public_api_contract(dir2)
  file1 <- file.path(dir1, "public-api-current.tsv")
  file2 <- file.path(dir2, "public-api-current.tsv")
  expect_identical(readBin(file1, "raw", n = file.info(file1)$size), readBin(file2, "raw", n = file.info(file2)$size))
})
