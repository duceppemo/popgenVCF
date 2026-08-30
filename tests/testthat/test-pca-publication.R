test_that("PCA publication artifacts are complete and reproducible", {
  out <- tempfile("pca-publication-")
  dir.create(out)
  coordinates <- data.frame(
    sample_id = paste0("s", 1:6),
    PC1 = c(-2, -1.5, -1, 1, 1.5, 2),
    PC2 = c(-0.2, 0.1, 0.2, -0.1, 0.1, -0.1),
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    sample_id = paste0("s", 1:6),
    population = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )
  manifest <- popgenVCF::write_pca_publication_artifacts(
    coordinates, c(4, 2, 1), metadata, out
  )
  expect_s3_class(manifest, "PopgenVCFArtifactManifest")
  table <- popgenVCF::artifact_manifest_table(manifest)
  expect_setequal(table$name, c(
    "coordinates", "variance", "pc1_pc2_pdf", "pc1_pc2_svg",
    "pc1_pc2_png", "methods", "caption", "validation", "figure_source"
  ))
  expect_true(all(file.exists(table$path)))
  variance <- data.table::fread(file.path(out, "tables", "PCA_variance.tsv"))
  expect_equal(sum(variance$variance_percent), 100, tolerance = 1e-8)
  checks <- data.table::fread(file.path(out, "validation", "PCA_validation.tsv"))
  expect_true(all(checks$passed))
})

test_that("PCA publication artifacts use the true percent-of-total-variance when supplied, not the truncated-eigenvalue re-derivation", {
  # Real regression: run_module_pca() (R/pca_registry_integration.R) used to
  # pass only the RETAINED-only eigenvalue subset (SNPRelate::snpgdsPCA's
  # eigen.cnt-truncated output) with no variance_percent, so this function's
  # old unconditional `100 * eigenvalues / sum(eigenvalues)` silently
  # inflated every percentage -- e.g. requesting 3 of a much larger true
  # spectrum (here representing ~10% of total genetic variance) previously
  # would have reported the three retained components as summing to 100%.
  # Supplying the correctly pre-computed variance_percent (as
  # run_pca()/pca$variance$percent, SNPRelate's own varprop-based
  # computation, now does) must produce the true, un-inflated numbers.
  out <- tempfile("pca-publication-truncated-")
  dir.create(out)
  coordinates <- data.frame(
    sample_id = paste0("s", 1:6),
    PC1 = c(-2, -1.5, -1, 1, 1.5, 2),
    PC2 = c(-0.2, 0.1, 0.2, -0.1, 0.1, -0.1),
    stringsAsFactors = FALSE
  )
  # Same relative eigenvalue shape as the first test (4, 2, 1) but
  # representing only 10% of the true total genetic variance.
  true_percent <- c(4, 2, 1) / sum(c(4, 2, 1)) * 10

  manifest <- popgenVCF::write_pca_publication_artifacts(
    coordinates, c(4, 2, 1), output_dir = out, variance_percent = true_percent
  )
  expect_s3_class(manifest, "PopgenVCFArtifactManifest")

  variance <- data.table::fread(file.path(out, "tables", "PCA_variance.tsv"))
  expect_equal(variance$variance_percent, true_percent, tolerance = 1e-8)
  expect_equal(sum(variance$variance_percent), 10, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(sum(variance$variance_percent), 100)))

  checks <- data.table::fread(file.path(out, "validation", "PCA_validation.tsv"))
  expect_true(all(checks$passed))

  methods_text <- readLines(file.path(out, "methods", "PCA_methods.md"), warn = FALSE)
  expect_true(any(grepl(sprintf("%.2f", true_percent[1]), methods_text, fixed = TRUE)))
})

test_that("write_pca_publication_artifacts rejects an invalid variance_percent", {
  out <- tempfile("pca-publication-")
  coordinates <- data.frame(sample_id = c("s1", "s2"), PC1 = c(-1, 1), PC2 = c(0, 0))
  expect_error(
    popgenVCF::write_pca_publication_artifacts(
      coordinates, c(4, 2), output_dir = out, variance_percent = c(50, 60)
    ),
    "variance_percent"
  )
  expect_error(
    popgenVCF::write_pca_publication_artifacts(
      coordinates, c(4, 2), output_dir = out, variance_percent = c(50)
    ),
    "variance_percent"
  )
})

test_that("PCA publication input validation is strict", {
  out <- tempfile("pca-publication-")
  expect_error(
    popgenVCF::write_pca_publication_artifacts(
      data.frame(sample_id = "s1", PC1 = 1), c(1), output_dir = out
    ),
    "PC1 and PC2"
  )
  expect_error(
    popgenVCF::write_pca_publication_artifacts(
      data.frame(sample_id = c("s1", "s1"), PC1 = 1:2, PC2 = 2:1),
      c(1, 0.5), output_dir = out
    ),
    "unique"
  )
})
