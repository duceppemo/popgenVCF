# Dataset-tier construction for scripts/build_release_benchmark_archive.R's
# continuous performance benchmark. Kept under inst/scripts/ (not the
# top-level scripts/ directory) so it is bundled into the installed package
# and reachable via system.file() -- the top-level scripts/ directory is not
# part of the built/installed package, so tests that need these functions
# (e.g. under R CMD check's isolated install) cannot reach a copy living
# there.

# This file is always sys.source()d, never invoked as a top-level Rscript,
# so it cannot resolve its own path via commandArgs()'s --file= trick the
# way scripts/*.R files do. Its own sibling inst/scripts/ modules are
# resolved by checking the checked-out source tree first (reachable in every
# realistic invocation -- this always runs from within a popgenVCF checkout)
# and system.file() only as a fallback: this is *more* robust than
# system.file()-only resolution, since it guarantees testing the exact
# commit being benchmarked rather than whatever happens to be separately
# installed, while remaining exactly as correct under R CMD check's isolated
# install (no source tree reachable there, so it falls through unchanged).
resolve_benchmark_helper_script <- function(module) {
  source_root <- Sys.getenv("POPGENVCF_SOURCE_ROOT", unset = "")
  candidates <- unique(c(
    if (nzchar(source_root)) file.path(source_root, "inst", "scripts", module),
    file.path(getwd(), "inst", "scripts", module),
    file.path(dirname(getwd()), "inst", "scripts", module)
  ))
  matches <- candidates[file.exists(candidates)]
  if (length(matches)) return(normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE))

  installed <- system.file("scripts", module, package = "popgenVCF")
  if (nzchar(installed) && file.exists(installed)) return(installed)

  stop("Missing helper script: ", module, call. = FALSE)
}

# Real, already-approved chromosome 22 acquisition for the "canonical" tier,
# reusing the exact source, acquisition path, and bounded 1Mb region
# (22:20000000-21000000) the production_baseline/external_concordance/
# ancestry_three_backend gates already use -- never a new, unreviewed data
# subset. run_canonical_production_execution()'s normal gate-evidence output
# is written to a throwaway scratch directory; only the verified raw source
# in data_dir is used here. A benchmark run is not a canonical_validation
# gate execution.
canonical_benchmark_dataset <- function(git_sha) {
  bcftools <- Sys.which("bcftools")
  if (!nzchar(bcftools)) {
    stop("bcftools is required for the canonical benchmark tier", call. = FALSE)
  }
  for (module in c(
    "canonical_production_execution.R", "canonical_production_bcftools.R",
    "canonical_production_checksum.R", "canonical_autosomal_baseline.R"
  )) {
    sys.source(resolve_benchmark_helper_script(module), envir = environment())
  }

  work_root <- tempfile("popgenvcf-canonical-benchmark-")
  dir.create(work_root, recursive = TRUE)
  on.exit(unlink(work_root, recursive = TRUE, force = TRUE), add = TRUE)
  data_dir <- file.path(work_root, "source")
  dir.create(data_dir, recursive = TRUE)
  scratch_evidence_dir <- file.path(work_root, "scratch-evidence")

  source <- popgenVCF::canonical_1000g_chr22_source()
  valid_sha <- grepl("^[0-9a-f]{40}$", git_sha)
  run_canonical_production_execution(
    output_dir = scratch_evidence_dir, data_dir = data_dir,
    candidate_id = paste0("benchmark-", if (valid_sha) substr(git_sha, 1L, 12L) else "local"),
    git_commit = if (valid_sha) git_sha else strrep("0", 40L),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source = source, allow_download = TRUE, quiet = TRUE,
    # chr22 requires the same bcftools-compatible inspection override
    # run-approved-canonical-validation.R already uses; the default
    # inspection function is chrY-oriented and misclassifies chr22's index.
    inspect = canonical_production_inspect_bcftools_compatible
  )

  files <- source$files$filename
  vcf_name <- files[grepl("\\.vcf\\.gz$", files)]
  panel_name <- files[grepl("\\.panel$", files)]
  if (length(vcf_name) != 1L || length(panel_name) != 1L) {
    stop("chromosome 22 source inventory is ambiguous", call. = FALSE)
  }
  source_vcf <- file.path(data_dir, vcf_name)
  panel_path <- file.path(data_dir, panel_name)
  contract <- canonical_autosomal_baseline_contract()

  derived_vcf <- file.path(work_root, "chr22-benchmark-region.vcf.gz")
  view_args <- c(
    "view", "--regions", shQuote(contract$region), "--min-alleles", "2",
    "--max-alleles", "2", "--types", "snps", "--output-type", "z",
    "--output-file", shQuote(derived_vcf), shQuote(source_vcf)
  )
  canonical_production_system2(bcftools, view_args, "canonical benchmark VCF derivation")

  gds_path <- file.path(work_root, "chr22-benchmark-region.gds")
  SNPRelate::snpgdsVCF2GDS(derived_vcf, gds_path, method = "biallelic.only", verbose = FALSE)
  gds <- SNPRelate::snpgdsOpen(gds_path, readonly = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  geno <- SNPRelate::snpgdsGetGeno(
    gds, sample.id = ids$sample, snp.id = ids$snp, snpfirstdim = FALSE, verbose = FALSE
  )
  SNPRelate::snpgdsClose(gds)
  dimnames(geno) <- list(as.character(ids$sample), as.character(ids$snp))

  panel <- data.table::fread(panel_path, data.table = FALSE, check.names = FALSE)
  panel_metadata <- canonical_autosomal_sample_metadata(panel, ids$sample)

  list(
    genotype = geno, sample_ids = as.character(ids$sample), snp_ids = as.character(ids$snp),
    metadata = data.table::data.table(
      sample = panel_metadata$sample_id, population = panel_metadata$population
    ),
    chromosome = ids$chromosome, position = ids$position
  )
}

# Sizes for "medium"/"large" are larger *synthetic* datasets (per an explicit
# scoping decision -- no medium/large real dataset is registered/approved
# anywhere in this repo, and sourcing one is a data-governance decision, not
# something to invent here), chosen from real local timing measurements:
# synthetic 60x2000 ~0.23s/rep, medium 300x20000 ~1.9s/rep, large
# 1000x100000 ~37s/rep (measured against the actual run_pca/run_ibs/
# compute_diversity/run_fst pipeline functions, GDS creation included).
benchmark_tier_dataset <- function(tier, git_sha) {
  if (identical(tier, "canonical")) return(canonical_benchmark_dataset(git_sha))
  size <- switch(tier,
    synthetic = list(samples = 60L, snps = 2000L),
    medium = list(samples = 300L, snps = 20000L),
    large = list(samples = 1000L, snps = 100000L),
    stop("Unknown dataset tier: ", tier, call. = FALSE)
  )
  geno <- popgenVCF:::synthetic_genotypes(samples = size$samples, snps = size$snps, seed = 1L)
  sample_ids <- rownames(geno)
  snp_ids <- colnames(geno)
  list(
    genotype = geno, sample_ids = sample_ids, snp_ids = snp_ids,
    metadata = data.table::data.table(
      sample = sample_ids,
      population = rep(c("A", "B", "C"), length.out = length(sample_ids))
    ),
    chromosome = rep(1L, ncol(geno)), position = seq_len(ncol(geno))
  )
}

# Every tier is timed through the exact same runner shape: build a fresh GDS
# from the tier's genotype matrix (so GDS creation overhead is counted
# identically everywhere), then run the actual analysis functions
# run_pipeline() itself calls (PCA, IBS, diversity, FST).
benchmark_tier_spec <- function(tier, dataset) {
  popgenVCF::new_performance_benchmark_spec(
    id = "pipeline-core-analyses",
    runner = function(threads) {
      gds_path <- tempfile(fileext = ".gds")
      on.exit(unlink(gds_path), add = TRUE)
      SNPRelate::snpgdsCreateGeno(
        gds_path, genmat = dataset$genotype, sample.id = dataset$sample_ids,
        snp.id = dataset$snp_ids, snp.chromosome = dataset$chromosome,
        snp.position = dataset$position,
        snp.allele = rep("A/G", ncol(dataset$genotype)), snpfirstdim = FALSE
      )
      gds <- SNPRelate::snpgdsOpen(gds_path)
      on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
      ids <- list(chromosome = dataset$chromosome, position = dataset$position, snp = dataset$snp_ids)
      popgenVCF:::run_pca(gds, dataset$sample_ids, dataset$snp_ids, dataset$metadata, n_pcs = 5L, threads = threads)
      popgenVCF:::run_ibs(gds, dataset$sample_ids, dataset$snp_ids, dataset$metadata, threads = threads)
      popgenVCF:::compute_diversity(gds, dataset$sample_ids, dataset$snp_ids, dataset$metadata, ids)
      popgenVCF:::run_fst(gds, dataset$snp_ids, dataset$metadata)
      invisible(NULL)
    },
    threads = 1L,
    warmup = 1L,
    iterations = 5L,
    gating = FALSE,
    metadata = list(dataset_tier = tier, analyses = "pca,ibs,diversity,fst")
  )
}
