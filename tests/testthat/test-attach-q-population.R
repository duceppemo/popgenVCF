test_that("attach_q_population leaves the Q table unchanged when metadata has no population column", {
  # A real production incident: admixture/fastStructure/sNMF are
  # unsupervised ancestry backends that need no population metadata to run
  # (unlike diversity/fst/amova/etc., which are population-gated and
  # excluded entirely when metadata is absent) -- but their Q-matrix output
  # labeling step previously hard-required a population column anyway,
  # crashing the whole pipeline and discarding an already-completed real
  # ADMIXTURE result on a real 50-sample, no-metadata cohort.
  qdt <- data.table::data.table(sample = c("s1", "s2"), cluster_1 = c(0.8, 0.2), cluster_2 = c(0.2, 0.8))
  metadata <- data.table::data.table(sample = c("s1", "s2"))

  out <- popgenVCF:::attach_q_population(qdt, metadata)

  expect_false("population" %in% names(out))
  expect_identical(out$sample, c("s1", "s2"))
})

test_that("attach_q_population attaches population when metadata provides one", {
  qdt <- data.table::data.table(sample = c("s2", "s1"), cluster_1 = c(0.1, 0.9), cluster_2 = c(0.9, 0.1))
  metadata <- data.table::data.table(sample = c("s1", "s2"), population = c("A", "B"))

  out <- popgenVCF:::attach_q_population(qdt, metadata)

  expect_identical(out$population, c("B", "A"))
})

test_that("attach_q_population rejects duplicate metadata sample identities only when population is actually used", {
  qdt <- data.table::data.table(sample = "s1", cluster_1 = 1)
  duplicated_metadata <- data.table::data.table(sample = c("s1", "s1"), population = c("A", "B"))

  expect_error(
    popgenVCF:::attach_q_population(qdt, duplicated_metadata),
    "duplicate sample identifiers"
  )

  # The same duplicate metadata is harmless if it has no population column
  # at all -- nothing is joined against it.
  no_population_metadata <- data.table::data.table(sample = c("s1", "s1"))
  out <- popgenVCF:::attach_q_population(qdt, no_population_metadata)
  expect_false("population" %in% names(out))
})

test_that("attach_q_population errors when a Q-matrix sample is absent from retained metadata", {
  qdt <- data.table::data.table(sample = c("s1", "s2"), cluster_1 = c(0.5, 0.5))
  metadata <- data.table::data.table(sample = "s1", population = "A")

  expect_error(
    popgenVCF:::attach_q_population(qdt, metadata),
    "absent from retained metadata"
  )
})
