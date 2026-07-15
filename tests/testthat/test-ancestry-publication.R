test_that("ancestry publication artifacts are complete without metadata", {
  sample_ids <- paste0("s", 1:6)
  q <- rbind(
    c(.9, .1), c(.8, .2), c(.7, .3),
    c(.2, .8), c(.1, .9), c(.3, .7)
  )
  reps <- list(
    new_ancestry_replicate(sample_ids, q, "admixture", 2, 1, metrics = c(cv_error = .42)),
    new_ancestry_replicate(sample_ids, q[, 2:1], "admixture", 2, 2, metrics = c(cv_error = .421))
  )
  consensus <- consensus_ancestry(reps)
  out <- tempfile("ancestry-publication-")
  manifest <- write_ancestry_publication_artifacts(consensus, out)
  tab <- artifact_manifest_table(manifest)
  ids <- paste(tab$module, tab$name, sep = "::")

  expect_true(all(file.exists(tab$path)))
  expect_true(all(c(
    "ancestry::q_table", "ancestry::barplot_pdf",
    "ancestry::uncertainty_pdf", "ancestry::stability_pdf",
    "ancestry::validation"
  ) %in% ids))
  source <- data.table::fread(file.path(out, "source_data", "ancestry_admixture_K2_figure_source.tsv"))
  expect_equal(sort(source$sample_id), sample_ids)
  expect_false("population" %in% names(source))
})

test_that("population metadata and K selection add grouped and model-selection artifacts", {
  sample_ids <- paste0("s", 1:6)
  q2 <- rbind(c(.9,.1), c(.8,.2), c(.7,.3), c(.2,.8), c(.1,.9), c(.3,.7))
  make_rep <- function(k, rep, metric) {
    q <- if (k == 2L) q2 else cbind(q2 * .9, .1)
    q <- q / rowSums(q)
    new_ancestry_replicate(sample_ids, q, "admixture", k, rep,
      metrics = c(cv_error = metric))
  }
  reps <- list(
    make_rep(2, 1, .50), make_rep(2, 2, .49),
    make_rep(3, 1, .42), make_rep(3, 2, .421)
  )
  selection <- select_ancestry_k(reps)
  consensus <- consensus_ancestry(reps[3:4])
  metadata <- data.frame(sample_id = sample_ids, population = rep(c("A", "B"), each = 3))
  out <- tempfile("ancestry-publication-metadata-")
  manifest <- write_ancestry_publication_artifacts(
    consensus, out, metadata = metadata, k_selection = selection
  )
  tab <- artifact_manifest_table(manifest)
  ids <- paste(tab$module, tab$name, sep = "::")

  expect_true(all(c(
    "ancestry::k_selection_source", "ancestry::k_selection_pdf",
    "ancestry::k_selection_svg", "ancestry::k_selection_png"
  ) %in% ids))
  source <- data.table::fread(file.path(out, "source_data", "ancestry_admixture_K3_figure_source.tsv"))
  expect_true("population" %in% names(source))
  expect_equal(sort(unique(source$population)), c("A", "B"))
})

test_that("ancestry result selection is deterministic and validated", {
  ids <- c("a", "b")
  q <- rbind(c(.8,.2), c(.2,.8))
  result <- new_ancestry_result(list(
    new_ancestry_replicate(ids, q, "snmf", 2, 2),
    new_ancestry_replicate(ids, q, "snmf", 2, 1)
  ))
  out <- tempfile("ancestry-result-publication-")
  manifest <- write_ancestry_publication_artifacts(result, out, backend = "snmf", k = 2)
  expect_s3_class(manifest, "PopgenVCFArtifactManifest")
  expect_error(
    write_ancestry_publication_artifacts(result, tempfile(), backend = "admixture"),
    "no ancestry replicate matches"
  )
  expect_error(
    write_ancestry_publication_artifacts(result, tempfile(), metadata = data.frame(x = 1)),
    "metadata must contain sample_id"
  )
})
