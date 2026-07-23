test_that("ADMIXTURE CV output parses", {
  x <- popgenVCF:::parse_admixture_cv("CV error (K=4): 0.123456")
  expect_equal(x$K, 4L)
  expect_equal(x$cv_error, 0.123456)
})

test_that("ADMIXTURE Q matrices join retained metadata explicitly", {
  root <- tempfile("admixture-q-")
  dir.create(root)
  q_file <- file.path(root, "cohort.2.Q")
  sample_file <- file.path(root, "samples.txt")

  writeLines(c("0.8 0.2", "0.1 0.9"), q_file)
  writeLines(c("sample_2", "sample_1"), sample_file)
  metadata <- data.table::data.table(
    sample = c("sample_1", "sample_2"),
    population = c("population_A", "population_B")
  )

  q <- popgenVCF:::read_admixture_q(q_file, sample_file, metadata)

  expect_equal(q$sample, c("sample_2", "sample_1"))
  expect_equal(q$population, c("population_B", "population_A"))
  expect_equal(
    rowSums(as.matrix(q[, c("cluster_1", "cluster_2"), with = FALSE])),
    c(1, 1)
  )
})

test_that("ADMIXTURE Q metadata joins reject duplicate identities", {
  root <- tempfile("admixture-q-duplicate-")
  dir.create(root)
  q_file <- file.path(root, "cohort.2.Q")
  sample_file <- file.path(root, "samples.txt")

  writeLines(c("0.8 0.2", "0.1 0.9"), q_file)
  writeLines(c("sample_1", "sample_2"), sample_file)
  metadata <- data.table::data.table(
    sample = c("sample_1", "sample_1"),
    population = c("population_A", "population_B")
  )

  expect_error(
    popgenVCF:::read_admixture_q(q_file, sample_file, metadata),
    "duplicate sample identifiers"
  )
})
