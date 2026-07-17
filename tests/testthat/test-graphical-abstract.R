test_that("graphical abstract specifications are deterministic", {
  project <- new_popgenvcf_project("graphical-abstract-test")
  manuscript <- new_manuscript(project, title = "Population structure", abstract = "Abstract", authors = list())
  asset <- tempfile(fileext = ".png")
  writeBin(charToRaw("immutable-figure"), asset)
  panels <- list(list(artifact_id = "figure:pca", path = asset, label = "A", role = "input"))

  x <- new_graphical_abstract(manuscript, panels = panels, title = "Structure at a glance",
                              message = "Author supplied message", alt_text = "Accessible description")
  y <- new_graphical_abstract(manuscript, panels = panels, title = "Structure at a glance",
                              message = "Author supplied message", alt_text = "Accessible description")
  expect_s3_class(x, "PopgenVCFGraphicalAbstract")
  expect_identical(x$id, y$id)
  expect_true(validate_graphical_abstract(x))
})

test_that("strict validation exposes incomplete author inputs", {
  project <- new_popgenvcf_project("graphical-abstract-test")
  manuscript <- new_manuscript(project, title = "Population structure", abstract = "Abstract", authors = list())
  x <- new_graphical_abstract(manuscript)
  expect_true(validate_graphical_abstract(x, strict = FALSE))
  expect_error(validate_graphical_abstract(x, strict = TRUE), "Incomplete graphical abstract")
})

test_that("duplicate identities and checksum changes are rejected", {
  project <- new_popgenvcf_project("graphical-abstract-test")
  manuscript <- new_manuscript(project, title = "Population structure", abstract = "Abstract", authors = list())
  asset <- tempfile(fileext = ".svg")
  writeLines("<svg/>", asset)
  panels <- list(
    list(artifact_id = "figure:one", path = asset),
    list(artifact_id = "figure:one", path = asset)
  )
  expect_error(new_graphical_abstract(manuscript, panels = panels), "unique")

  x <- new_graphical_abstract(manuscript, panels = panels[1], title = "Title", message = "Message", alt_text = "Alt")
  writeLines("changed", asset)
  expect_error(validate_graphical_abstract(x), "checksum mismatch")
})

test_that("graphical abstract bundles are written and protected", {
  project <- new_popgenvcf_project("graphical-abstract-test")
  manuscript <- new_manuscript(project, title = "Population structure", abstract = "Abstract", authors = list())
  asset <- tempfile(fileext = ".png")
  writeBin(charToRaw("asset"), asset)
  x <- new_graphical_abstract(manuscript,
    panels = list(list(artifact_id = "figure:pca", path = asset, caption = "PCA panel")),
    title = "Title", message = "Message", alt_text = "Alt text")
  root <- tempfile()
  out <- write_graphical_abstract(x, root, strict = TRUE)
  expect_true(file.exists(file.path(out, "graphical-abstract-record.json")))
  expect_true(file.exists(file.path(out, "graphical-abstract-manifest.tsv")))
  expect_true(file.exists(file.path(out, "graphical-abstract-brief.md")))
  expect_error(write_graphical_abstract(x, root), "already exists")
})