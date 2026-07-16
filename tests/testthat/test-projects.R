test_that("projects validate and capture component identities", {
  input <- tempfile(fileext = ".vcf")
  writeLines("fixture", input)
  project <- new_popgenvcf_project(
    "fixture",
    results = list(pca = matrix(1:4, 2L)),
    inputs = c(vcf = input),
    parameters = list(maf = 0.05),
    modules = list(pca = "completed"),
    rng = new_project_rng(42L, streams = list(pca = 101L)),
    project_id = "00000000-0000-0000-0000-000000000001",
    package_version = "0.10.0", git_sha = "abc"
  )
  expect_s3_class(project, "PopgenVCFProject")
  expect_equal(project$inputs$role, "vcf")
  expect_true(project$inputs$exists)
  expect_match(project$inputs$sha256, "^[0-9a-f]{64}$")
  expect_silent(validate_popgenvcf_project(project))
  expect_equal(project_table(project)$result_count, 1L)
})

test_that("portable project bundles verify and reopen", {
  project <- new_popgenvcf_project(
    "roundtrip", results = list(ibs = diag(2L)),
    project_id = "00000000-0000-0000-0000-000000000002",
    package_version = "0.10.0", git_sha = "def"
  )
  path <- tempfile(fileext = ".popgenvcf")
  written <- write_popgenvcf_project(project, path)
  expect_true(file.exists(written))
  expect_true(verify_popgenvcf_project(written))
  restored <- read_popgenvcf_project(written)
  expect_identical(restored, project)
  expect_error(write_popgenvcf_project(project, written), "already exists")
})

test_that("project comparison reports identity, input, and result changes", {
  baseline <- new_popgenvcf_project(
    "analysis", results = list(pca = c(1, 2)), parameters = list(maf = .05),
    project_id = "00000000-0000-0000-0000-000000000003",
    package_version = "0.10.0", git_sha = "a"
  )
  current <- new_popgenvcf_project(
    "analysis", results = list(pca = c(1, 3), fst = .1), parameters = list(maf = .1),
    project_id = "00000000-0000-0000-0000-000000000004",
    package_version = "0.10.1", git_sha = "b"
  )
  comparison <- compare_popgenvcf_projects(current, baseline)
  expect_s3_class(comparison, "PopgenVCFProjectComparison")
  expect_true(comparison$changed)
  expect_true(comparison$changes[category == "result" & item == "pca", changed])
  expect_true(comparison$changes[category == "result" & item == "fst", changed])
  expect_equal(names(project_table(comparison)),
               c("category", "item", "baseline", "current", "changed"))
})

test_that("tampered bundles fail verification", {
  project <- new_popgenvcf_project(
    "tamper", project_id = "00000000-0000-0000-0000-000000000005"
  )
  path <- tempfile(fileext = ".popgenvcf")
  write_popgenvcf_project(project, path)
  root <- tempfile("tamper-"); dir.create(root)
  utils::untar(path, exdir = root)
  writeLines("changed", file.path(root, "project.json"))
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  utils::tar(path, files = list.files(".", all.files = TRUE, no.. = TRUE),
             compression = "gzip", tar = "internal")
  setwd(old)
  expect_error(verify_popgenvcf_project(path), "checksum mismatch")
})

test_that("projects support an empty typed input manifest", {
  project <- new_popgenvcf_project(
    "no-inputs",
    project_id = "00000000-0000-0000-0000-000000000006"
  )
  expect_equal(nrow(project$inputs), 0L)
  expect_identical(
    names(project$inputs),
    c("role", "path", "exists", "size_bytes", "sha256")
  )
})

test_that("invalid project inputs fail clearly", {
  expect_error(new_popgenvcf_project("x", inputs = data.frame(path = "x")),
               "inputs must contain")
  expect_error(validate_popgenvcf_project(list()), "PopgenVCFProject")
  expect_error(read_popgenvcf_project("missing.popgenvcf"), "does not exist")
})
