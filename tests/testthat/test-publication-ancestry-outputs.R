test_that("publication ancestry outputs are deterministic and ordered", {
  spec <- new_publication_ancestry_spec()
  q <- data.frame(
    sample_id = c("s2", "s1", "s3"),
    population = c("B", "A", "A"),
    Q1 = c(0.2, 0.8, 0.5),
    Q2 = c(0.8, 0.2, 0.5)
  )
  output <- new_publication_ancestry_output(
    spec,
    q_matrix = q,
    consensus = data.frame(sample_id = c("s1", "s2", "s3"), cluster = c(1L, 2L, 1L)),
    replicate_diagnostics = data.frame(replicate = 1:2, score = c(0.95, 0.97)),
    k_selection = data.frame(k = 2:3, criterion = c(0.4, 0.6)),
    result_fingerprint = "ancestry-result"
  )

  expect_identical(output$q_matrix$sample_id, c("s1", "s3", "s2"))
  expect_identical(output$k, 2L)
  expect_true(validate_publication_ancestry_output(output, spec))
  expect_match(publication_ancestry_caption(output, spec), "3 samples at K = 2")
  expect_identical(
    output$fingerprint,
    new_publication_ancestry_output(
      spec, q, result_fingerprint = "ancestry-result",
      consensus = output$consensus,
      replicate_diagnostics = output$replicate_diagnostics,
      k_selection = output$k_selection
    )$fingerprint
  )
})

test_that("publication ancestry outputs fail closed", {
  spec <- new_publication_ancestry_spec()
  bad_q <- data.frame(sample_id = c("s1", "s2"), population = "A", Q1 = c(0.8, 0.4), Q2 = c(0.4, 0.6))
  expect_error(new_publication_ancestry_output(spec, bad_q, result_fingerprint = "x"), "sum to one")

  q <- data.frame(sample_id = c("s1", "s2"), population = "A", Q1 = c(0.8, 0.4), Q2 = c(0.2, 0.6))
  output <- new_publication_ancestry_output(spec, q, result_fingerprint = "x")
  mutated <- output
  mutated$q_matrix$Q1[[1L]] <- 0.7
  expect_error(validate_publication_ancestry_output(mutated, spec), "source data drifted|fingerprint mismatch")

  mutated_spec <- spec
  mutated_spec$ancestry_prefix <- "A"
  expect_error(validate_publication_ancestry_spec(mutated_spec), "Invalid or mutated")
})
