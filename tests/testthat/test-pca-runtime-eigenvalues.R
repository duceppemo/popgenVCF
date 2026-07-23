test_that("PCA eigenvalue normalization clips only numerical residue", {
  normalized <- popgenVCF:::normalize_pca_eigenvalues(c(4, 2, 1e-16, -1e-12))

  expect_equal(normalized$values, c(4, 2, 0, 0))
  expect_equal(normalized$adjusted_negative, 1L)
  expect_gt(normalized$tolerance, 1e-12)

  small_scale <- popgenVCF:::normalize_pca_eigenvalues(
    c(4e-12, 2e-12, -1e-24)
  )
  expect_equal(small_scale$values[1:2], c(4e-12, 2e-12))
  expect_equal(small_scale$values[[3L]], 0)

  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, 2, -0.1)),
    "materially negative"
  )
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(0, 0)),
    "no positive genetic variance"
  )
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, NA_real_)),
    "non-finite eigenvalue"
  )
})

test_that("PCA component requests respect matrix rank bounds", {
  samples <- paste0("s", seq_len(23L))
  snps <- seq_len(100L)

  expect_identical(
    popgenVCF:::pca_component_count(10L, samples, snps),
    10L
  )
  expect_identical(
    popgenVCF:::pca_component_count(32L, samples, snps),
    22L
  )
  expect_identical(
    popgenVCF:::pca_component_count(10L, paste0("s", seq_len(50L)), seq_len(4L)),
    4L
  )
  expect_error(
    popgenVCF:::pca_component_count(10L, c("s1", "s2"), seq_len(100L)),
    "at least two estimable components"
  )
})

test_that("PCA covariance fallback recovers a finite eigensystem", {
  pca <- list(
    sample.id = paste0("s", seq_len(4L)),
    eigenval = c(4, NaN, NaN),
    eigenvect = matrix(NaN, nrow = 4L, ncol = 3L),
    genmat = diag(c(4, 2, 1, 0))
  )

  expect_false(popgenVCF:::pca_eigensystem_is_finite(pca, 3L))
  recovered <- popgenVCF:::recover_pca_eigensystem(pca, 3L)

  expect_true(popgenVCF:::pca_eigensystem_is_finite(recovered, 3L))
  expect_equal(recovered$eigenval, c(4, 2, 1))
  expect_equal(sum(recovered$varprop), 1, tolerance = 1e-12)
  expect_equal(
    unname(crossprod(recovered$eigenvect)),
    diag(3L),
    tolerance = 1e-12
  )

  pca$genmat[1, 1] <- NaN
  expect_error(
    popgenVCF:::recover_pca_eigensystem(pca, 3L),
    "covariance matrix contains 1 non-finite"
  )
})

test_that("PCA and downstream SNPRelate calls pin runtime-sensitive arguments", {
  pca_body <- paste(deparse(body(popgenVCF:::run_pca)), collapse = "\n")
  ibs_body <- paste(deparse(body(popgenVCF:::run_ibs)), collapse = "\n")
  fst_body <- paste(deparse(body(popgenVCF:::run_fst)), collapse = "\n")
  fst_pair_body <- paste(deparse(body(popgenVCF:::fst_pair)), collapse = "\n")

  expect_match(pca_body, "eigen.cnt = requested_components", fixed = TRUE)
  expect_match(pca_body, "missing.rate = NaN", fixed = TRUE)
  expect_match(pca_body, "need.genmat = need_genmat", fixed = TRUE)
  expect_match(pca_body, "recover_pca_eigensystem", fixed = TRUE)
  expect_match(ibs_body, "missing.rate = NaN", fixed = TRUE)
  expect_match(fst_body, "missing.rate = NaN", fixed = TRUE)
  expect_match(fst_pair_body, "missing.rate = NaN", fixed = TRUE)
})

test_that("PCA registry publishes and records bounded runtime eigenvalues", {
  module_body <- paste(
    deparse(body(popgenVCF:::run_module_pca)),
    collapse = "\n"
  )

  expect_match(module_body, "eigenvalues = pca$eigenvalues", fixed = TRUE)
  expect_match(module_body, "pca$requested_components", fixed = TRUE)
  expect_match(module_body, "pca$eigensystem_source", fixed = TRUE)
  expect_false(grepl("eigenvalues = pca$object$eigenval", module_body, fixed = TRUE))
})
