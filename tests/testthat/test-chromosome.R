test_that("run_chromosome_analyses groups SNPs by chromosome and matches direct FST/PCA calls", {
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)

  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  sample_ids <- as.character(ids$sample)
  snp_ids <- as.character(ids$snp)
  expect_identical(as.integer(table(ids$chromosome)[c("1", "2")]), c(6L, 3L))

  cfg <- popgenVCF::default_config()
  cfg$analyses$chromosome_min_snps <- 3L
  cfg$analyses$n_pcs <- 10L
  cfg$compute$threads <- 1L

  out <- popgenVCF:::run_chromosome_analyses(
    gds, snp_ids, snp_ids, ids, sample_ids, metadata, cfg
  )

  expect_identical(names(out), c("1", "2"))

  for (chr in names(out)) {
    chr_snps <- snp_ids[ids$chromosome[match(snp_ids, ids$snp)] == chr]
    expected_fst <- popgenVCF:::run_fst(gds, chr_snps, metadata)
    expected_pca <- popgenVCF:::run_pca(
      gds, sample_ids, chr_snps, metadata, min(3L, cfg$analyses$n_pcs), cfg$compute$threads
    )

    summary <- out[[chr]]$summary
    expect_identical(summary$chromosome, chr)
    expect_identical(summary$qc_snps, length(chr_snps))
    expect_identical(summary$ld_snps, length(chr_snps))
    expect_equal(summary$global_fst, expected_fst$global)
    expect_equal(summary$pc1_percent, expected_pca$variance$percent[1])

    expect_identical(out[[chr]]$fst, expected_fst$long)
    expect_identical(out[[chr]]$pca, expected_pca$scores)
  }
})

test_that("run_chromosome_analyses excludes chromosomes below the configured SNP threshold", {
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)

  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  sample_ids <- as.character(ids$sample)
  snp_ids <- as.character(ids$snp)

  cfg <- popgenVCF::default_config()
  cfg$analyses$chromosome_min_snps <- 4L
  cfg$analyses$n_pcs <- 10L
  cfg$compute$threads <- 1L

  out <- popgenVCF:::run_chromosome_analyses(
    gds, snp_ids, snp_ids, ids, sample_ids, metadata, cfg
  )

  expect_identical(names(out), "1")
})

test_that("run_chromosome_analyses requires at least two LD-pruned SNPs per chromosome", {
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)

  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  sample_ids <- as.character(ids$sample)
  snp_ids <- as.character(ids$snp)
  chromosome_of <- ids$chromosome[match(snp_ids, ids$snp)]
  chr1_snps <- snp_ids[chromosome_of == "1"]
  chr2_snp <- snp_ids[chromosome_of == "2"][[1L]]

  cfg <- popgenVCF::default_config()
  cfg$analyses$chromosome_min_snps <- 1L
  cfg$analyses$n_pcs <- 10L
  cfg$compute$threads <- 1L

  # chr1 keeps its full LD-pruned set (>= 2); chr2 is left with only one
  # LD-pruned SNP, below the hard-coded minimum of two.
  out <- popgenVCF:::run_chromosome_analyses(
    gds, snp_ids, c(chr1_snps, chr2_snp), ids, sample_ids, metadata, cfg
  )

  expect_identical(names(out), "1")
})

test_that("run_chromosome_analyses returns an empty list when no chromosome qualifies", {
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)

  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  sample_ids <- as.character(ids$sample)
  snp_ids <- as.character(ids$snp)

  cfg <- popgenVCF::default_config()
  cfg$analyses$chromosome_min_snps <- 100L
  cfg$analyses$n_pcs <- 10L
  cfg$compute$threads <- 1L

  out <- popgenVCF:::run_chromosome_analyses(
    gds, snp_ids, snp_ids, ids, sample_ids, metadata, cfg
  )

  expect_identical(out, list())
})

test_that("write_chromosome_results writes per-chromosome tables and returns the summary", {
  fake <- list(
    "1" = list(
      summary = data.table::data.table(
        chromosome = "1", qc_snps = 6L, ld_snps = 6L,
        global_fst = 0.5, pc1_percent = 40
      ),
      fst = data.table::data.table(population_1 = "PopA", population_2 = "PopB", fst = 0.5),
      pca = data.table::data.table(sample = c("A1", "B1"), PC1 = c(-1, 1))
    ),
    "2" = list(
      summary = data.table::data.table(
        chromosome = "2", qc_snps = 3L, ld_snps = 3L,
        global_fst = -0.1, pc1_percent = 66.7
      ),
      fst = data.table::data.table(population_1 = "PopA", population_2 = "PopB", fst = -0.1),
      pca = data.table::data.table(sample = c("A1", "B1"), PC1 = c(0.5, -0.5))
    )
  )

  root <- withr::local_tempdir()
  dirs <- popgenVCF:::make_dirs(root)

  summary <- popgenVCF:::write_chromosome_results(fake, dirs)

  expect_identical(summary$chromosome, c("1", "2"))
  expect_true(file.exists(file.path(dirs$tables, "chromosome_summary.tsv")))
  expect_true(file.exists(file.path(dirs$chromosomes, "1_pairwise_FST.tsv")))
  expect_true(file.exists(file.path(dirs$chromosomes, "1_PCA.tsv")))
  expect_true(file.exists(file.path(dirs$chromosomes, "2_pairwise_FST.tsv")))
  expect_true(file.exists(file.path(dirs$chromosomes, "2_PCA.tsv")))

  written_summary <- data.table::fread(file.path(dirs$tables, "chromosome_summary.tsv"))
  expect_equal(written_summary$global_fst, c(0.5, -0.1))
})

test_that("write_chromosome_results returns an empty table without writing files for no chromosomes", {
  root <- withr::local_tempdir()
  dirs <- popgenVCF:::make_dirs(root)

  summary <- popgenVCF:::write_chromosome_results(list(), dirs)

  expect_s3_class(summary, "data.table")
  expect_equal(nrow(summary), 0L)
  expect_false(file.exists(file.path(dirs$tables, "chromosome_summary.tsv")))
})

test_that("write_chromosome_results sanitizes chromosome names used as file stems", {
  fake <- list(
    "chr/weird name" = list(
      summary = data.table::data.table(
        chromosome = "chr/weird name", qc_snps = 4L, ld_snps = 4L,
        global_fst = 0.1, pc1_percent = 20
      ),
      fst = data.table::data.table(population_1 = "PopA", population_2 = "PopB", fst = 0.1),
      pca = data.table::data.table(sample = c("A1", "B1"), PC1 = c(-1, 1))
    )
  )

  root <- withr::local_tempdir()
  dirs <- popgenVCF:::make_dirs(root)

  popgenVCF:::write_chromosome_results(fake, dirs)

  expect_true(file.exists(file.path(dirs$chromosomes, "chr_weird_name_pairwise_FST.tsv")))
  expect_true(file.exists(file.path(dirs$chromosomes, "chr_weird_name_PCA.tsv")))
})
