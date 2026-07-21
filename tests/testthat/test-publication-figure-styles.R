test_that("built-in figure styles are deterministic and valid", {
  ids <- c("accessibility-first", "grayscale-safe", "standard-color")
  profiles <- lapply(ids, publication_figure_style_profile)
  expect_identical(vapply(profiles, `[[`, character(1L), "id"), ids)
  expect_equal(length(unique(vapply(profiles, `[[`, character(1L), "fingerprint"))), length(ids))
  expect_true(all(vapply(profiles, validate_publication_figure_style_profile, logical(1L))))
  expect_identical(publication_figure_style_profile("grayscale-safe")$fingerprint,
                   publication_figure_style_profile("grayscale-safe")$fingerprint)
})

test_that("grayscale and contrast validation fail closed", {
  expect_error(new_publication_figure_style_profile(
    "bad-contrast", "#777777", "solid", 16L,
    background = "#FFFFFF", foreground = "#777777", min_contrast = 4.5
  ), "contrast")
  expect_error(new_publication_figure_style_profile(
    "bad-gray", c("#111111", "#222222"), c("solid", "dashed"), c(16L, 17L),
    grayscale_safe = TRUE
  ), "distinguishable")
})

test_that("figure-style bindings preserve report and layout identity", {
  spec <- new_publication_report_spec(c("html", "pdf"))
  layout <- publication_layout_profile("general")
  style <- publication_figure_style_profile("accessibility-first")
  binding <- bind_publication_figure_style(spec, layout, style, groups = 4L)
  expect_s3_class(binding, "PopgenVCFPublicationFigureStyleBinding")
  expect_true(validate_publication_figure_style_binding(binding, spec, layout, style))
  parameters <- publication_figure_parameters(binding, style)
  expect_identical(parameters$style_fingerprint, style$fingerprint)

  changed <- publication_figure_style_profile("standard-color")
  expect_error(validate_publication_figure_style_binding(binding, spec, layout, changed), "not bound")
})

test_that("palette capacity and redundant encodings fail closed", {
  spec <- new_publication_report_spec("html")
  layout <- publication_layout_profile("general")
  style <- publication_figure_style_profile("grayscale-safe")
  expect_error(bind_publication_figure_style(spec, layout, style, groups = 4L), "cannot preserve")

  colour_heavy <- new_publication_figure_style_profile(
    "colour-heavy", c("#000000", "#444444", "#888888"), "solid", 16L,
    labels_required = FALSE, colour_alone = FALSE
  )
  expect_error(bind_publication_figure_style(spec, layout, colour_heavy, groups = 2L), "redundant")
})

test_that("accessibility audits are deterministic", {
  style <- publication_figure_style_profile("accessibility-first")
  audit <- publication_figure_accessibility_audit(style)
  expect_s3_class(audit, "PopgenVCFPublicationFigureAccessibilityAudit")
  expect_true(audit$passed)
  expect_true(audit$redundant_encoding)
  expect_identical(audit$fingerprint, publication_figure_accessibility_audit(style)$fingerprint)

  style$foreground <- "#333333"
  expect_error(validate_publication_figure_style_profile(style), "fingerprint mismatch")
})
