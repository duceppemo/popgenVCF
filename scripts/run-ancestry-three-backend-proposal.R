#!/usr/bin/env Rscript
#
# Candidate-bound execution of the ancestry_three_backend release gate (#24):
# runs ADMIXTURE, fastStructure, and sNMF across a declared K range and
# replicate schedule against the approved 1000 Genomes Phase 3 chromosome 22
# canonical source, then assembles a canonical_ancestry_three_backend
# evidence proposal (R/canonical_ancestry_three_backend.R).
#
# Uses a wider genomic interval than the 1Mb production_baseline window
# (22:20000000-21000000, ~350 LD-pruned SNPs): proven-out testing this
# session found 350 SNPs gives only moderate cross-backend Q-matrix
# agreement (~0.83-0.89 alignment), while ~3,500 SNPs from a 10Mb window
# gives near-complete convergence (~0.97-0.99) across all three backends.
# The full default sweep (K=2:10, 5 replicates, 3 backends) takes several
# hours at this scale -- this is a manual/scheduled production execution,
# not routine CI, matching the existing canonical-real-data.yml pattern for
# production_baseline and external_concordance.
#
# Usage: run-ancestry-three-backend-proposal.R <output-dir> <work-dir>
#          <source-dir> <candidate-id> <git-commit> <generated-at>
#          [--k-range=2:10] [--replicates=5] [--region=22:15000000-25000000]
#
# <source-dir> must already contain the approved, verified chromosome 22
# source files (see scripts/run-approved-canonical-validation.R).

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
source_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
module_dir <- normalizePath(file.path(source_root, "inst", "scripts"), mustWork = TRUE)
for (module in c(
  "canonical_production_execution.R", "canonical_production_bcftools.R",
  "canonical_production_checksum.R"
)) sys.source(file.path(module_dir, module), envir = environment())

raw_args <- commandArgs(trailingOnly = TRUE)
flag <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), raw_args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[[length(hit)]])
}
k_range_arg <- flag("k-range", "2:10")
replicates_arg <- as.integer(flag("replicates", "5"))
region <- flag("region", "22:15000000-25000000")
args <- raw_args[!grepl("^--", raw_args)]
if (length(args) != 6L) {
  stop(paste(
    "Usage: run-ancestry-three-backend-proposal.R",
    "<output-dir> <work-dir> <source-dir> <candidate-id> <git-commit> <generated-at>",
    "[--k-range=2:10] [--replicates=5] [--region=22:15000000-25000000]"
  ), call. = FALSE)
}
output_dir <- args[[1L]]
work_dir <- canonical_production_dir(args[[2L]], "work_dir", create = TRUE, empty = TRUE)
source_dir <- canonical_production_dir(args[[3L]], "source_dir")
candidate_id <- canonical_production_scalar(args[[4L]], "candidate_id")
git_commit <- canonical_production_commit(args[[5L]])
generated_at <- canonical_production_timestamp(args[[6L]])
k_values <- eval(parse(text = k_range_arg))
if (!is.numeric(k_values) || anyNA(k_values) || any(k_values < 2L)) {
  stop("--k-range must resolve to integers of at least two, e.g. 2:10", call. = FALSE)
}
k_values <- sort(unique(as.integer(k_values)))
if (is.na(replicates_arg) || replicates_arg < 1L) stop("--replicates must be a positive integer", call. = FALSE)

if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
  stop("output_dir must be absent or empty", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- canonical_production_dir(output_dir, "ancestry-three-backend output", empty = TRUE)

bcftools <- Sys.which("bcftools")
if (!nzchar(bcftools)) stop("bcftools is required for chromosome 22 subset derivation", call. = FALSE)

# ---- 1. Verify the approved source and derive the wider interval ----
source <- popgenVCF::canonical_1000g_chr22_source()
verification <- popgenVCF::verify_canonical_source(source, source_dir)
if (!all(verification$passed)) stop("approved chromosome 22 source verification failed", call. = FALSE)
files <- source$files$filename
source_vcf <- file.path(source_dir, files[grepl("\\.vcf\\.gz$", files)])
panel_path <- file.path(source_dir, files[grepl("\\.panel$", files)])

derived_vcf <- file.path(work_dir, "chr22-ancestry-three-backend.vcf.gz")
canonical_production_system2(bcftools, c(
  "view", "--regions", shQuote(region), "--min-alleles", "2",
  "--max-alleles", "2", "--types", "snps", "--output-type", "z",
  "--output-file", shQuote(derived_vcf), shQuote(source_vcf)
), "chr22 ancestry subset derivation")
canonical_production_system2(
  bcftools, c("index", "--tbi", "--force", shQuote(derived_vcf)),
  "chr22 ancestry subset indexing"
)
inventory <- canonical_production_variant_inventory(bcftools, derived_vcf)
cat("Derived interval (", region, ") variant count:", inventory$variant_count, "\n")

# ---- 2. QC / LD-pruning only (PCA module selected purely to produce the
# retained sample/SNP sets and GDS; its own output is not part of this
# gate's evidence) ----
pipeline_dir <- file.path(work_dir, "pipeline")
cfg <- popgenVCF::default_config()
cfg$input$vcf <- derived_vcf
cfg$input$metadata <- panel_path
cfg$output$directory <- pipeline_dir
cfg$output$figure_formats <- character()
cfg$compute$threads <- 4L
cfg$compute$seed <- 42L
cfg$qc$maf <- 0.05
cfg$qc$max_variant_missing <- 0.20
cfg$qc$max_sample_missing <- 0.20
cfg$qc$ld_r2 <- 0.20
cfg$analyses$n_pcs <- 10L
cfg$report$enabled <- FALSE

cat("Running QC/LD-pruning on the derived interval...\n")
analysis <- popgenVCF::run_pipeline(cfg, selected = "pca")
if (!identical(analysis$status, "complete")) stop("pipeline did not complete", call. = FALSE)

sample_ids <- analysis$samples$ids
ld_ids <- analysis$variants$ld_ids
cat("Retained:", length(sample_ids), "samples,", length(ld_ids), "LD-pruned SNPs\n")

gds <- SNPRelate::snpgdsOpen(analysis$inputs$gds_path, readonly = TRUE)
on.exit(try(SNPRelate::snpgdsClose(gds), silent = TRUE), add = TRUE)

# ---- 3. Prepare backend inputs and run all three backends ----
plink <- popgenVCF:::prepare_structure_plink_input(
  gds, sample_ids, ld_ids, cache_dir = file.path(work_dir, "cache"),
  converter = popgenVCF:::portable_gds_to_bed
)
snmf_in <- popgenVCF:::prepare_snmf_input(
  gds, sample_ids, ld_ids, cache_dir = file.path(work_dir, "cache")
)
input <- list(
  plink_prefix = plink$prefix, output_dir = file.path(work_dir, "runs"),
  geno_file = snmf_in$geno_file, cv_folds = 5L, threads = 4L, entropy = TRUE
)

cat(
  "Running all three backends: K=", paste(range(k_values), collapse = ":"),
  ", ", replicates_arg, " replicate(s) each\n", sep = ""
)
run <- popgenVCF::run_ancestry(
  input, sample_ids, backend = "all", k_values = k_values,
  replicates = replicates_arg, seed = 42L
)
if (!all(run$records$status == "success")) {
  stop("one or more ancestry backend runs did not succeed", call. = FALSE)
}
cat("All", nrow(run$records), "backend runs succeeded\n")

# ---- 4. Per-backend evidence ----
backends <- c("admixture", "faststructure", "snmf")
extract_tool_version <- function(executable, args = "--version") {
  out <- tryCatch(
    suppressWarnings(system2(executable, args, stdout = TRUE, stderr = TRUE)),
    error = function(e) character()
  )
  m <- regmatches(out, regexpr("(?i)version[[:space:]]+[^[:space:]]+", out, perl = TRUE))
  m <- m[nzchar(m)]
  if (!length(m)) return("unknown")
  sub("(?i)^version[[:space:]]+", "", m[[1L]], perl = TRUE)
}
tool_versions <- list(
  admixture = extract_tool_version("admixture", "--version"),
  faststructure = extract_tool_version(Sys.which("structure.py"), character()),
  snmf = as.character(utils::packageVersion("LEA"))
)
commands <- list(
  admixture = "admixture --cv=5 --seed=<seed> <plink-prefix>.bed <k> -j<threads>",
  faststructure = "structure.py -K <k> --input <plink-prefix> --output <prefix> --seed <seed> --format bed",
  snmf = "LEA::snmf(<geno-file>, K=<k>, repetitions=1, entropy=TRUE, seed=<seed>)"
)

stability_by_k <- function(backend_name) {
  backend_reps <- run$results[[backend_name]]$replicates
  ks <- vapply(backend_reps, `[[`, integer(1L), "k")
  metric_name <- if (length(backend_reps[[1L]]$metrics)) names(backend_reps[[1L]]$metrics)[[1L]] else NA_character_
  rows <- lapply(sort(unique(ks)), function(k) {
    subset <- backend_reps[ks == k]
    metric_values <- if (!is.na(metric_name)) {
      vapply(subset, function(r) unname(r$metrics[[metric_name]]), numeric(1L))
    } else NA_real_
    consensus <- popgenVCF:::consensus_ancestry(subset)
    data.frame(
      k = k, metric_mean = mean(metric_values, na.rm = TRUE),
      global_stability = consensus$global_stability,
      mean_alignment_score = mean(consensus$alignment_table$alignment_score, na.rm = TRUE)
    )
  })
  list(table = do.call(rbind, rows), metric_name = metric_name)
}

backend_evidence <- lapply(backends, function(b) {
  s <- stability_by_k(b)
  popgenVCF::new_ancestry_backend_evidence(
    backend = b, tool_version = tool_versions[[b]], command = commands[[b]],
    k_values = k_values, replicates = replicates_arg, seed = 42L,
    metric_name = s$metric_name, stability_by_k = s$table
  )
})
cat("Backend evidence built for:", paste(backends, collapse = ", "), "\n")

# ---- 5. Cross-backend K-selection consensus ----
all_reps <- unlist(lapply(run$results, `[[`, "replicates"), recursive = FALSE)
k_selection <- popgenVCF::select_ancestry_k(all_reps)
selected_k <- as.integer(k_selection$overall_k)
cat("Cross-backend K-selection consensus: K=", selected_k, " (agreement=", signif(k_selection$agreement, 3), ")\n", sep = "")

# ---- 6. Cross-backend Q-matrix comparisons at the selected K ----
# `role` and `minimum_alignment_score` are left as an explicit, documented
# placeholder policy pending named scientific review -- see the interpretation
# text and docs/user/ancestry-backends.md's "Cross-backend release evidence"
# section. They are not scoped decisions this script is authorized to make.
consensus_at_k <- stats::setNames(lapply(backends, function(b) {
  backend_reps <- run$results[[b]]$replicates
  ks <- vapply(backend_reps, `[[`, integer(1L), "k")
  popgenVCF:::consensus_ancestry(backend_reps[ks == selected_k])
}), backends)
pairs <- utils::combn(backends, 2, simplify = FALSE)
cross_backend_comparisons <- lapply(pairs, function(pp) {
  alignment <- popgenVCF:::align_ancestry_replicate(
    consensus_at_k[[pp[1]]]$mean_q, consensus_at_k[[pp[2]]]$mean_q
  )
  popgenVCF::new_ancestry_cross_backend_comparison(
    backend_a = pp[1], backend_b = pp[2], k = selected_k, alignment = alignment,
    minimum_alignment_score = 0.8, role = "diagnostic",
    interpretation = paste(
      "PLACEHOLDER pending named scientific review: role and",
      "minimum_alignment_score are not yet a reviewed release policy.",
      "ADMIXTURE, fastStructure, and sNMF are independent algorithms;",
      "agreement is evidence about numerical and structural consistency,",
      "not proof that the inferred K or ancestry components are",
      "biologically correct."
    )
  )
})
cat("Cross-backend comparisons built:", length(cross_backend_comparisons), "\n")

# ---- 7. Assemble and write the canonical evidence proposal ----
evidence <- popgenVCF::new_canonical_ancestry_three_backend_evidence(
  dataset_id = source$id, dataset_version = source$version,
  sample_ids = sample_ids, region = region,
  backend_evidence = backend_evidence,
  cross_backend_comparisons = cross_backend_comparisons,
  k_selection = k_selection, selected_k = selected_k,
  generated_by = "run-ancestry-three-backend-proposal.R", generated_at = generated_at,
  source_commit = git_commit, approval = "proposed"
)

evidence_path <- file.path(output_dir, "ancestry-three-backend-proposal.json")
written <- popgenVCF::write_canonical_ancestry_three_backend_evidence(evidence, evidence_path)
checksum_path <- canonical_production_write_checksums(
  output_dir, file.path(output_dir, "ancestry-three-backend-SHA256SUMS.txt")
)

cat("\nAncestry three-backend proposal completed\n")
cat("Dataset:", evidence$dataset_id, evidence$dataset_version, "\n")
cat("Region:", evidence$region, "\n")
cat("Selected K:", evidence$selected_k, "\n")
cat("Approval:", evidence$approval, "\n")
cat("Evidence:", written, "\n")
cat("Checksums:", checksum_path, "\n")
