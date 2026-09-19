test_that("publication AMOVA outputs are deterministic and ordered", {
  spec <- new_publication_amova_spec(phi_columns = c("statistic", "p_value"))
  components <- data.frame(
    source = c("Within populations", "Among populations"),
    df = c(18, 2),
    sum_squares = c(40, 10),
    variance_component = c(1.5, 0.5),
    percent_variation = c(75, 25),
    stringsAsFactors = FALSE
  )
  phi <- data.frame(
    comparison = "Among populations",
    statistic = 0.25,
    p_value = 0.01,
    stringsAsFactors = FALSE
  )
  permutations <- data.frame(
    test = "Phi_ST",
    observed = 0.25,
    permutations = 999,
    p_value = 0.01,
    stringsAsFactors = FALSE
  )

  output <- new_publication_amova_output(
    spec, components, phi, permutations,
    result_fingerprint = "amova-result-sha256"
  )

  # Real AMOVA variance-source labels are hierarchical, not alphabetical
  # -- the caller-supplied row order is trusted and preserved, not
  # re-sorted (a real bug: alphabetical sorting previously moved a real
  # "Total variations" row to the very front of the table, since "T"
  # sorts before "Variations...", scrambling a scientific table's
  # canonical order into something actively misleading).
  expect_identical(output$variance_components$source,
                   c("Within populations", "Among populations"))
  expect_true(validate_publication_amova_output(output, spec))
  # The fingerprint is now sensitive to the caller-supplied row order
  # (it is no longer normalized away by an alphabetical re-sort) --
  # calling with the same rows in reverse produces a genuinely different
  # (but still internally self-consistent) output and fingerprint, the
  # deliberate tradeoff for preserving real hierarchical AMOVA order.
  expect_false(identical(
    output$fingerprint,
    new_publication_amova_output(
      spec, components[2:1, ], phi, permutations,
      result_fingerprint = "amova-result-sha256"
    )$fingerprint
  ))
  expect_match(publication_amova_caption(output, spec), "2 hierarchical")
  expect_match(paste(publication_amova_report(output, spec), collapse = "\n"),
               "Permutation evidence: `present`")
})

test_that("a real hierarchical AMOVA row order, including a trailing 'Total' row, is preserved verbatim", {
  # Real bug: sorting variance-source labels alphabetically pulled a
  # "Total variations" row to the very FRONT of the published table
  # ("T" sorts before "Variations..."), matching run_amova_analysis()'s
  # own real poppr::poppr.amova() output shape (R/amova.R) exactly.
  spec <- new_publication_amova_spec()
  components <- data.frame(
    source = c(
      "Variations  Between population",
      "Variations  Between samples Within population",
      "Variations  Within samples",
      "Total variations"
    ),
    df = c(1, 4, 6, 11),
    sum_squares = c(0.5, 8.2, 7.0, 15.7),
    variance_component = c(0.01, 1.71, 1.17, 2.89),
    percent_variation = c(0.48, 59.13, 40.38, 100),
    stringsAsFactors = FALSE
  )
  output <- new_publication_amova_output(
    spec, components, result_fingerprint = "amova-hierarchy-check"
  )
  expect_identical(output$variance_components$source, components$source)
  expect_identical(output$variance_components$source[[4L]], "Total variations")
})

test_that("publication AMOVA contracts fail closed", {
  spec <- new_publication_amova_spec()
  components <- data.frame(
    source = c("Among", "Within"),
    df = c(2, 18),
    sum_squares = c(10, 40),
    variance_component = c(0.5, 1.5),
    percent_variation = c(25, 75),
    stringsAsFactors = FALSE
  )
  output <- new_publication_amova_output(
    spec, components,
    result_fingerprint = "amova-result-sha256"
  )

  tampered_spec <- spec
  tampered_spec$source_column <- "level"
  expect_error(validate_publication_amova_spec(tampered_spec), "fingerprint")

  drifted <- output
  drifted$source_data$variance_components$df[[1]] <- 99
  expect_error(validate_publication_amova_output(drifted, spec), "drifted")

  mutated <- output
  mutated$variance_components$df[[1]] <- 99
  mutated$source_data$variance_components <- mutated$variance_components
  expect_error(validate_publication_amova_output(mutated, spec), "fingerprint")

  expect_error(
    new_publication_amova_output(
      spec,
      transform(components, source = c("Among", "Among")),
      result_fingerprint = "amova-result-sha256"
    ),
    "unique"
  )
})
