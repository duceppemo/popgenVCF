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
