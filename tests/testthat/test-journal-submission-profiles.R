test_that("generic journal profile is deterministic", {
  first <- generic_journal_profile()
  second <- generic_journal_profile()
  expect_s3_class(first, "PopgenVCFJournalProfile")
  expect_identical(first, second)
  expect_true(validate_journal_profile(first))
})

test_that("custom profiles validate role and filename rules", {
  profile <- new_journal_profile(
    id = "example",
    required_roles = c("manuscript_source", "jats_xml"),
    optional_roles = "figure",
    filenames = c(manuscript_source = "main.md", jats_xml = "article.xml")
  )
  expect_true(validate_journal_profile(profile))
  expect_error(new_journal_profile(required_roles = "x", optional_roles = "x"), "must not overlap")
  expect_error(new_journal_profile(required_roles = "x", filenames = c(y = "y.txt")), "unknown roles")
})

test_that("profiles rename and validate submission plans", {
  profile <- new_journal_profile(
    id = "example",
    required_roles = c("manuscript_source", "jats_xml"),
    optional_roles = "figure",
    filenames = c(manuscript_source = "main.md", jats_xml = "article.xml")
  )
  plan <- data.table::data.table(
    role = c("jats_xml", "manuscript_source", "figure"),
    destination = c("jats/manuscript.xml", "source/manuscript.md", "figures/figure-1.png")
  )
  out <- apply_journal_profile(plan, profile)
  expect_identical(out[role == "manuscript_source", destination], "main.md")
  expect_identical(out[role == "jats_xml", destination], "article.xml")
  expect_identical(attr(out, "journal_profile_digest"), profile$digest)
  expect_error(apply_journal_profile(plan[role != "jats_xml"], profile), "missing required roles")
})

test_that("profile digest detects tampering", {
  profile <- generic_journal_profile()
  profile$id <- "changed"
  expect_error(validate_journal_profile(profile), "digest mismatch")
})
