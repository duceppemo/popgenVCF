# A synthetic 30-unrelated-sample base (2000 SNPs, enough marker density for
# KING to behave sensibly -- the tiny 9-SNP CI validation fixture was shown
# empirically, in the same session that built this feature, to give
# nonsensical kinship values) plus one exact duplicate (true kinship = 0.5)
# and one simulated Mendelian parent-offspring (true kinship = 0.25) gives
# real, verifiable ground truth without requiring network access or bcftools.
kinship_fixture_gds <- function() {
  n_unrelated <- 30L
  n_snps <- 2000L
  base <- popgenVCF:::synthetic_genotypes(samples = n_unrelated, snps = n_snps, seed = 123L)
  duplicate <- base[1L, , drop = FALSE]
  rownames(duplicate) <- "dup_of_sample1"

  parent1 <- base[2L, ]; parent2 <- base[3L, ]
  transmit <- function(genotype) stats::rbinom(length(genotype), 1L, genotype / 2)
  set.seed(7L)
  child <- transmit(parent1) + transmit(parent2)
  child <- matrix(child, nrow = 1L, dimnames = list("child_of_2_3", colnames(base)))

  genmat <- rbind(base, duplicate, child)
  sample_id <- rownames(genmat)
  snp_id <- seq_len(ncol(genmat))
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps) * 100L,
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  list(path = gds_path, sample_id = sample_id)
}

kinship_fixture <- function() {
  built <- kinship_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(built$path)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = built$sample_id, population = rep("POP", length(built$sample_id))
  ))
  list(gds = gds, sample_id = built$sample_id, metadata = metadata)
}

test_that("kinship_relationship_degree classifies every KING threshold boundary", {
  k <- c(0.5, 0.354, 0.3, 0.177, 0.1, 0.0884, 0.05, 0.0442, 0.02, 0, -0.3, NA_real_)
  out <- popgenVCF:::kinship_relationship_degree(k)
  expect_identical(out, c(
    "duplicate/MZ twin", "1st-degree", "1st-degree", "2nd-degree", "2nd-degree",
    "3rd-degree", "3rd-degree", "unrelated", "unrelated", "unrelated", "unrelated",
    NA_character_
  ))
})

test_that("run_kinship recovers a known duplicate and a simulated parent-offspring pair", {
  fx <- kinship_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  snp_id <- popgenVCF:::get_gds_ids(fx$gds)$snp

  result <- popgenVCF:::run_kinship(fx$gds, fx$sample_id, snp_id, fx$metadata, 1L)
  expect_named(result, c("kinship", "ibs0", "pairs"))
  expect_true(is.matrix(result$kinship))
  expect_identical(dim(result$kinship), c(length(fx$sample_id), length(fx$sample_id)))
  expect_true(isTRUE(all.equal(result$kinship, t(result$kinship))))

  pairs <- result$pairs
  expect_setequal(
    names(pairs),
    c("sample_1", "sample_2", "population_1", "population_2", "IBS0", "kinship", "relationship_degree")
  )
  expect_identical(nrow(pairs), as.integer(choose(length(fx$sample_id), 2)))
  # setorder(-kinship) means the very first row is the strongest relationship.
  expect_gte(pairs$kinship[1], pairs$kinship[nrow(pairs)])

  find_pair <- function(a, b) pairs[(sample_1 == a & sample_2 == b) | (sample_1 == b & sample_2 == a)]
  duplicate_pair <- find_pair("sample1", "dup_of_sample1")
  expect_equal(duplicate_pair$kinship, 0.5, tolerance = 1e-6)
  expect_equal(duplicate_pair$IBS0, 0, tolerance = 1e-6)
  expect_identical(duplicate_pair$relationship_degree, "duplicate/MZ twin")

  child_parent1 <- find_pair("sample2", "child_of_2_3")
  child_parent2 <- find_pair("sample3", "child_of_2_3")
  expect_gt(child_parent1$kinship, 0.15)
  expect_gt(child_parent2$kinship, 0.15)
  expect_identical(child_parent1$relationship_degree, "1st-degree")
  expect_identical(child_parent2$relationship_degree, "1st-degree")
})

test_that("plot_kinship draws the heatmap and diagnostic scatter with correct titles", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))

  kinship <- matrix(
    c(0.5, 0.25, 0.02, 0.25, 0.5, 0.03, 0.02, 0.03, 0.5),
    nrow = 3L, dimnames = list(c("s1", "s2", "s3"), c("s1", "s2", "s3"))
  )
  pairs <- data.table::data.table(
    sample_1 = c("s1", "s1", "s2"), sample_2 = c("s2", "s3", "s3"),
    population_1 = "POP", population_2 = "POP",
    IBS0 = c(0, 0.05, 0.06), kinship = c(0.25, 0.02, 0.03),
    relationship_degree = c("1st-degree", "unrelated", "unrelated")
  )
  result <- list(kinship = kinship, ibs0 = kinship, pairs = pairs)

  popgenVCF:::plot_kinship(result, cfg, dirs)

  expect_true("21_kinship_heatmap" %in% names(plots))
  expect_true("22_kinship_IBS0_vs_kinship" %in% names(plots))
  expect_identical(plots[["21_kinship_heatmap"]]$labels$title, "Pairwise kinship (KING-robust)")
  expect_identical(plots[["22_kinship_IBS0_vs_kinship"]]$labels$title, "KING-robust relatedness diagnostic")
})

test_that("plot_kinship omits the heatmap above the 300-sample gate but still draws the scatter", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))

  n <- 301L
  ids <- paste0("s", seq_len(n))
  kinship <- diag(0.5, n); dimnames(kinship) <- list(ids, ids)
  pairs <- data.table::data.table(
    sample_1 = "s1", sample_2 = "s2", population_1 = "POP", population_2 = "POP",
    IBS0 = 0.1, kinship = 0.01, relationship_degree = "unrelated"
  )
  result <- list(kinship = kinship, ibs0 = kinship, pairs = pairs)

  popgenVCF:::plot_kinship(result, cfg, dirs)

  expect_false("21_kinship_heatmap" %in% names(plots))
  expect_true("22_kinship_IBS0_vs_kinship" %in% names(plots))
})

test_that("kinship_heatmap_plot tolerates non-finite kinship values without crashing hclust", {
  kinship <- matrix(
    c(0.5, NaN, 0.3, NaN, 0.5, NaN, 0.3, NaN, 0.5),
    nrow = 3L, dimnames = list(c("s1", "s2", "s3"), c("s1", "s2", "s3"))
  )
  p <- popgenVCF:::kinship_heatmap_plot(kinship)
  expect_s3_class(p, "ggplot")
})

test_that("kinship module descriptor owns the complete registry contract", {
  spec <- popgenVCF::kinship_module_spec()

  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "kinship")
  expect_identical(spec$requires, character())
  expect_identical(spec$outputs, "kinship")
  expect_identical(spec$references, "Manichaikul et al. 2010")
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$contract_version, "1.0")
  expect_identical(spec$run, popgenVCF:::run_module_kinship)
  expect_identical(spec$validate, popgenVCF:::validate_kinship_result)
})

test_that("built-in registry reflects the kinship module descriptor", {
  spec <- popgenVCF::kinship_module_spec()
  registry <- popgenVCF::default_analysis_registry()
  module <- registry$modules$kinship

  expect_identical(module$name, spec$name)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$run, spec$run)
  expect_identical(module$validate, spec$validate)
})

test_that("validate_kinship_result flags missing components and impossible kinship values", {
  ok <- list(
    close_relatives = data.table::data.table(
      sample_1 = "s1", sample_2 = "s2", kinship = 0.3, relationship_degree = "1st-degree"
    ),
    matrix_file = "x", ibs0_file = "y", pairs_file = "z"
  )
  expect_true(popgenVCF:::validate_kinship_result(ok, NULL, NULL)$valid)

  incomplete <- list(close_relatives = data.table::data.table())
  expect_false(popgenVCF:::validate_kinship_result(incomplete, NULL, NULL)$valid)

  impossible <- ok
  impossible$close_relatives$kinship <- 0.9
  expect_false(popgenVCF:::validate_kinship_result(impossible, NULL, NULL)$valid)

  # Real chr22 1000 Genomes data showed off-diagonal KING-robust kinship well
  # below -0.5 for highly differentiated population pairs -- this must NOT
  # fail validation.
  very_negative <- ok
  very_negative$close_relatives$kinship <- -8
  expect_true(popgenVCF:::validate_kinship_result(very_negative, NULL, NULL)$valid)
})
