test_that("built-in publication layouts are deterministic and distinct", {
  ids <- c("general", "nature-style", "g3", "molecular-ecology", "plos")
  profiles <- lapply(ids, publication_layout_profile)
  expect_identical(vapply(profiles, `[[`, character(1L), "id"), ids)
  expect_equal(length(unique(vapply(profiles, `[[`, character(1L), "fingerprint"))), length(ids))
  expect_true(all(vapply(profiles, validate_publication_layout_profile, logical(1L))))
  expect_identical(publication_layout_profile("general")$fingerprint,
                   publication_layout_profile("general")$fingerprint)
})

test_that("layout profiles remain bound to existing journal profiles", {
  journal <- generic_journal_profile()
  profile <- new_publication_layout_profile("custom", journal)
  expect_true(validate_publication_layout_profile(profile, journal))

  changed <- new_journal_profile(
    id = "changed",
    journal = "Changed valid journal profile",
    required_roles = journal$required_roles,
    optional_roles = journal$optional_roles,
    filenames = journal$filenames,
    required_sections = journal$requirements$sections$required,
    optional_sections = journal$requirements$sections$optional,
    required_declarations = journal$requirements$declarations,
    required_companions = journal$requirements$companions,
    title_max_chars = journal$requirements$limits$title_max_chars,
    abstract_max_words = journal$requirements$limits$abstract_max_words,
    keyword_min = journal$requirements$limits$keyword_min,
    keyword_max = journal$requirements$limits$keyword_max,
    highlight_min = journal$requirements$limits$highlight_min,
    highlight_max = journal$requirements$limits$highlight_max,
    highlight_max_chars = journal$requirements$limits$highlight_max_chars,
    graphical_abstract = journal$requirements$graphical_abstract,
    figure_max = journal$requirements$limits$figure_max,
    table_max = journal$requirements$limits$table_max,
    supplementary_max = journal$requirements$limits$supplementary_max,
    allowed_figure_extensions = journal$requirements$allowed_figure_extensions,
    filename_pattern = journal$requirements$filename_pattern,
    overrides = journal$requirements$overrides
  )
  expect_true(validate_journal_profile(changed))
  expect_error(validate_publication_layout_profile(profile, changed), "not bound")
})

test_that("layout bindings normalize overrides and reject unsupported fields", {
  spec <- new_publication_report_spec(c("pdf", "html"))
  profile <- publication_layout_profile("g3")
  a <- bind_publication_layout(
    spec, profile,
    list(typography = list(line_spacing = 1.5, font_size_pt = 11),
         renderer_parameters = list(z = 2L, a = 1L))
  )
  b <- bind_publication_layout(
    spec, profile,
    list(renderer_parameters = list(a = 1L, z = 2L),
         typography = list(font_size_pt = 11, line_spacing = 1.5))
  )
  expect_s3_class(a, "PopgenVCFPublicationLayoutBinding")
  expect_identical(a$fingerprint, b$fingerprint)
  expect_true(validate_publication_layout_binding(a, spec, profile))
  expect_identical(names(publication_layout_parameters(a)),
                   sort(names(publication_layout_parameters(a))))
  expect_error(bind_publication_layout(spec, profile, list(unknown = list(x = 1))), "unknown")
})

test_that("layout profiles fail closed on unsupported formats and mutation", {
  spec <- new_publication_report_spec(c("html", "pdf"))
  profile <- new_publication_layout_profile("html-only", formats = "html")
  expect_error(bind_publication_layout(spec, profile), "does not support")

  profile <- publication_layout_profile("plos")
  profile$typography$font_size_pt <- 8
  expect_error(validate_publication_layout_profile(profile), "fingerprint mismatch")
})

test_that("layout reports are deterministic", {
  profile <- publication_layout_profile("nature-style")
  report <- publication_layout_report(profile)
  expect_match(report[[1L]], "Publication layout profile", fixed = TRUE)
  expect_true(any(grepl(profile$fingerprint, report, fixed = TRUE)))
})