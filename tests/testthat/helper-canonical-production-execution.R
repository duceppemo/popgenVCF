canonical_production_module_paths <- function() {
  modules <- c(
    "canonical_production_execution.R",
    "canonical_production_bcftools.R",
    "canonical_production_checksum.R",
    "canonical_autosomal_baseline.R"
  )
  installed <- system.file("scripts", package = "popgenVCF")
  if (nzchar(installed) && all(file.exists(file.path(installed, modules)))) {
    return(file.path(installed, modules))
  }
  file.path(testthat::test_path("..", "..", "inst", "scripts"), modules)
}

canonical_production_test_env <- local({
  env <- new.env(parent = globalenv())
  for (module in canonical_production_module_paths()) {
    sys.source(module, envir = env)
  }
  env
})

canonical_production_fixture <- function() {
  mirror <- tempfile("canonical-mirror-")
  dir.create(mirror, recursive = TRUE)
  filenames <- c("fixture.vcf.gz", "fixture.vcf.gz.tbi", "fixture.panel")
  writeBin(charToRaw("synthetic compressed VCF fixture\n"), file.path(mirror, filenames[[1L]]))
  writeBin(charToRaw("synthetic tabix fixture\n"), file.path(mirror, filenames[[2L]]))
  writeLines(
    c("sample\tpop\tsuper_pop\tgender", "S1\tP1\tSP1\tmale", "S2\tP2\tSP1\tmale"),
    file.path(mirror, filenames[[3L]]),
    useBytes = TRUE
  )
  md5 <- vapply(
    file.path(mirror, filenames),
    function(path) tolower(unname(tools::md5sum(path))),
    character(1)
  )
  source <- list(
    schema_version = "1.0",
    id = "production_fixture",
    version = "1.0",
    title = "Canonical production execution fixture",
    organism = "Test organism",
    assembly = "test-assembly",
    doi = "10.0000/example.fixture",
    license = "CC0-1.0",
    citation = "Canonical production execution fixture.",
    reviewed_by = "Fixture reviewer",
    reviewed_at = "2026-07-22",
    chromosome_scope = "chrY",
    sample_sex_policy = "male_only",
    analyses = c("pca", "fst"),
    files = data.frame(
      filename = filenames,
      upstream_md5 = unname(md5),
      source = paste0("https://example.invalid/", filenames),
      stringsAsFactors = FALSE
    )
  )
  inspect <- function(source, directory) {
    list(
      summary = data.frame(
        dataset_id = source$id,
        dataset_version = source$version,
        vcf_file = source$files$filename[[1L]],
        index_file = source$files$filename[[2L]],
        panel_file = source$files$filename[[3L]],
        variant_count = 3,
        vcf_sample_count = 2L,
        panel_sample_count = 2L,
        exact_sample_set = TRUE,
        complete_metadata = TRUE,
        chromosome_scope = source$chromosome_scope,
        sample_sex_policy = source$sample_sex_policy,
        sex_policy_satisfied = TRUE,
        bcftools_version = "fixture-1.0",
        stringsAsFactors = FALSE
      ),
      sample_metadata = data.frame(
        sample_id = c("S1", "S2"),
        population = c("P1", "P2"),
        superpopulation = c("SP1", "SP1"),
        sex = c("male", "male"),
        stringsAsFactors = FALSE
      ),
      commands = list(
        sample_inventory = "bcftools query -l fixture.vcf.gz",
        variant_count = "bcftools index --nrecords fixture.vcf.gz"
      )
    )
  }
  list(mirror = mirror, source = source, inspect = inspect)
}
