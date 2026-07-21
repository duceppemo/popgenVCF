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

  expect_identical(output$variance_components$source,
                   c("Among populations", "Within populations"))
  expect_true(validate_publication_amova_output(output, spec))
  expect_identical(
    output$fingerprint,
    new_publication_amova_output(
      spec, components[2:1, ], phi, permutations,
      result_fingerprint = "amova-result-sha256"
    )$fingerprint
  )
  expect_match(publication_amova_caption(output, spec), "2 hierarchical")
  expect_match(paste(publication_amova_report(output, spec), collapse = "\n"),
               "Permutation evidence: `present`")
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
