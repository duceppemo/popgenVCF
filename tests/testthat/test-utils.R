test_that("parse_int_range returns an ascending, sorted range even when the bounds are given reversed", {
  # Every other branch of parse_int_range() returns sort(unique(...)) --
  # a reversed range ("10:2", a plausible typo for "2:10") went straight to
  # seq.int(10, 2) unsorted (10, 9, ..., 2) instead, silently breaking that
  # same contract for any caller relying on ascending K order (e.g.
  # ancestry_k_selection.R's plateau detection, which assumes its input K
  # values are already sorted).
  expect_identical(popgenVCF:::parse_int_range("10:2"), 2:10)
  expect_identical(popgenVCF:::parse_int_range("2:10"), 2:10)
})

test_that("parse_int_range still handles a comma list and a single integer as before", {
  expect_identical(popgenVCF:::parse_int_range("4,2,2,6"), c(2L, 4L, 6L))
  expect_identical(popgenVCF:::parse_int_range("5"), 5L)
  expect_identical(popgenVCF:::parse_int_range(c(3, 1, 2)), 1:3)
})

test_that("wrap_plot_text does not crash on a length > 1 input", {
  # `||` (like the `if` it used to feed) requires a length-1 operand as of
  # R >= 4.3 and errors outright for a length > 1 `text` -- confirmed
  # directly: "'length = 2' in coercion to 'logical(1)'". A dynamically
  # built caption/title/subtitle at any of this function's 30+ call sites
  # could plausibly produce a length > 1 value if not explicitly collapsed.
  result <- popgenVCF:::wrap_plot_text(c(
    "a fairly short caption",
    "a second, independently long enough caption that might actually need wrapping at some point"
  ), width = 20L)
  expect_length(result, 2L)
  expect_identical(result[[1L]], "a fairly short\ncaption")
})

test_that("wrap_plot_text still handles the ordinary scalar case as before", {
  expect_identical(popgenVCF:::wrap_plot_text(NULL), NULL)
  expect_identical(popgenVCF:::wrap_plot_text(NA_character_), NA_character_)
  expect_identical(popgenVCF:::wrap_plot_text(""), "")
  expect_identical(popgenVCF:::wrap_plot_text("short"), "short")
})

test_that("log_msg joins its parts without inserting extra spaces", {
  line <- NULL
  expect_output(line <- popgenVCF:::log_msg("Starting ", "pca", " with ", 3L, " thread(s)"), "Starting pca with 3 thread(s)", fixed = TRUE)
  expect_false(grepl("  ", sub("^\\[[^]]*\\] \\[[^]]*\\] ", "", line)))
})
