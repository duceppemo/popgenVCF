journal_profile_test_manuscript <- function() {
  project <- new_popgenvcf_project("journal-profile-test")
  new_manuscript(
    project,
    title = "Population structure",
    authors = data.frame(name = "Test Author", stringsAsFactors = FALSE),
    abstract = "A concise complete abstract.",
    keywords = c("genetics", "population", "structure"),
    introduction = "Introduction.",
    results = "Results.",
    discussion = "Discussion.",
    declarations = list(
      funding = "No external funding.",
      author_contributions = "Test Author performed the work.",
      competing_interests = "The author declares no competing interests."
    )
  )
}

test_that("generic journal profile is deterministic", {
  first <- generic_journal_profile()
  second <- generic_journal_profile()
  expect_s3_class(first, "PopgenVCFJournalProfile")
  expect_identical(first, second)
  expect_true(validate_journal_profile(first))
  expect_false(identical(first$digest, journal_profile("data-note")$digest))
})

test_that("custom profiles validate roles, sections, and filename rules", {
  profile <- new_journal_profile(
    id = "example",
    required_roles = c("manuscript_source", "jats_xml"),
    optional_roles = "figure",
    filenames = c(manuscript_source = "main.md", jats_xml = "article.xml"),
    required_sections = c("abstract", "methods"),
    optional_sections = "discussion"
  )
  expect_true(validate_journal_profile(profile))
  expect_error(new_journal_profile(required_roles = "x", optional_roles = "x"), "must not overlap")
  expect_error(new_journal_profile(required_roles = "x", filenames = c(y = "y.txt")), "unknown roles")
  expect_error(new_journal_profile(required_sections = "abstract", optional_sections = "abstract"), "must not overlap")
  expect_error(new_journal_profile(keyword_min = 5L, keyword_max = 3L), "cannot exceed")
  expect_error(new_journal_profile(overrides = list("value")), "named list")
})

test_that("an NA role name is rejected instead of being silently dropped by sort()", {
  # sort()'s own default (na.last = NA) REMOVES NA entirely -- so
  # anyNA(x) after sorting could never actually trigger; a genuinely NA
  # role name silently vanished instead of being rejected.
  expect_error(
    new_journal_profile(required_roles = c("manuscript_source", NA_character_)),
    "non-empty role names"
  )
})

test_that("keyword_min = NA (explicitly 'no minimum') does not crash validation", {
  # journal_profile_limit() explicitly documents NA as a supported value
  # ("must be a non-negative integer or NA"). A bare
  # `keyword_count >= limits$keyword_min` with keyword_min = NA
  # propagated NA into journal_submission_row()'s own
  # `if (ok) "pass" else "fail"`, a hard crash instead of the intended
  # "no minimum imposed" pass.
  profile <- new_journal_profile(id = "no-keyword-min", keyword_min = NA_integer_)
  manuscript <- journal_profile_test_manuscript()
  report <- expect_no_error(validate_journal_submission(profile, manuscript))
  expect_identical(report[requirement == "keywords:min", status], "pass")
})

test_that("highlight_min = NA with no other highlight limits configured does not crash validation", {
  # `if (limits$highlight_min > 0L || !is.na(limits$highlight_max) || !is.na(limits$highlight_max_chars))`
  # is NA (not FALSE) when highlight_min is NA and the other two limits
  # are also left at their own NA defaults -- R's `||` only
  # short-circuits to TRUE on an actual TRUE operand, not on FALSE -- so
  # this crashed outright instead of correctly skipping the optional
  # highlights-limits block entirely.
  profile <- new_journal_profile(id = "no-highlight-min", highlight_min = NA_integer_)
  manuscript <- journal_profile_test_manuscript()
  expect_no_error(validate_journal_submission(profile, manuscript))
})

test_that("verified named profiles require versioned sources", {
  expect_error(new_journal_profile(id = "named", status = "verified"), "require source_url and source_date")
  profile <- new_journal_profile(
    id = "named", journal = "Named journal", status = "verified",
    source_url = "https://example.org/requirements", source_date = "2026-07-17"
  )
  expect_true(validate_journal_profile(profile))
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

test_that("submission reports are deterministic and actionable", {
  manuscript <- journal_profile_test_manuscript()
  profile <- journal_profile("research-article")
  first <- validate_journal_submission(profile, manuscript)
  second <- validate_journal_submission(profile, manuscript)
  expect_s3_class(first, "PopgenVCFJournalSubmissionReport")
  expect_identical(first, second)
  expect_true(all(c("requirement", "status", "observed", "expected", "message") %in% names(first)))
  expect_true(all(first$status == "pass"))
})

test_that("strict submission validation exposes incomplete author inputs", {
  project <- new_popgenvcf_project("journal-profile-incomplete")
  manuscript <- new_manuscript(
    project,
    title = "Incomplete",
    authors = data.frame(name = "Test Author", stringsAsFactors = FALSE)
  )
  profile <- journal_profile("research-article")
  report <- validate_journal_submission(profile, manuscript)
  expect_true(any(report$status == "fail"))
  expect_error(validate_journal_submission(profile, manuscript, strict = TRUE), "requirements failed")
})

test_that("companion and graphical abstract requirements are explicit", {
  manuscript <- journal_profile_test_manuscript()
  profile <- new_journal_profile(
    id = "companions", required_companions = "highlights",
    highlight_min = 2L, highlight_max = 3L, highlight_max_chars = 30L,
    graphical_abstract = TRUE
  )
  report <- validate_journal_submission(profile, manuscript)
  expect_true(all(report$status[report$requirement %in% c("companion:highlights", "graphical_abstract")] == "fail"))
})

test_that("profile digest detects tampering", {
  profile <- generic_journal_profile()
  profile$id <- "changed"
  expect_error(validate_journal_profile(profile), "digest mismatch")
})

test_that("journal profile bundles are written and checksum protected", {
  profile <- journal_profile("data-note")
  root <- tempfile()
  out <- write_journal_profile(profile, root)
  expect_true(file.exists(file.path(out, "journal-profile.json")))
  expect_true(file.exists(file.path(out, "journal-profile.md")))
  expect_true(file.exists(file.path(out, "journal-profile-manifest.tsv")))
  expect_true(validate_journal_profile(out))
  expect_error(write_journal_profile(profile, root), "already exists")
  writeLines("changed", file.path(out, "journal-profile.md"))
  expect_error(validate_journal_profile(out), "checksum mismatch")
})

test_that("author text supplied as several paragraphs is kept whole and validates", {
  # manuscript_text() kept only the first element of a character vector, so
  # every later paragraph of an abstract, section or declaration was dropped
  # silently; declarations were stored un-normalized and a two-paragraph one
  # crashed validate_journal_submission() ("'length = 2' in coercion to
  # 'logical(1)'").
  manuscript <- new_manuscript(
    new_popgenvcf_project("paragraphs"), title = "Paragraphs",
    abstract = c("First paragraph.", NA, "", "Second paragraph."),
    declarations = list(data_availability = c("Reads are in the archive.", "Scripts are on the forge."))
  )
  expect_identical(manuscript$abstract, "First paragraph.\n\nSecond paragraph.")
  expect_identical(manuscript$declarations$data_availability, "Reads are in the archive.\n\nScripts are on the forge.")
  expect_identical(popgenVCF:::manuscript_text(c(NA, " "), "fallback"), "fallback")
  report <- validate_journal_submission(journal_profile("data-note"), manuscript)
  expect_identical(report[requirement == "declaration:data_availability"]$status, "pass")
  declarations <- popgenVCF:::render_author_declarations(new_submission_companions(manuscript))
  expect_true(any(grepl("Scripts are on the forge.", declarations, fixed = TRUE)))
})
