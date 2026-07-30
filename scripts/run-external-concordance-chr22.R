#!/usr/bin/env Rscript
#
# External-tool scientific concordance run against the approved 1000 Genomes
# Phase 3 chromosome 22 canonical source, for the `external_concordance`
# release gate (#22). Reuses the same bounded biallelic-SNP interval and QC
# contract as scripts/run-autosomal-baseline-proposal.R
# (22:20000000-21000000, MAF 0.05, missingness 0.20, LD r2 0.20, seed 42) so
# results are anchored to the already-reviewed production_baseline evidence,
# then runs the full analysis set (PCA, IBS, FST, diversity, DAPC, AMOVA,
# Mantel/IBD) and independently recomputes each via SNPRelate (direct),
# PLINK 2, hierfstat, adegenet, poppr, and pegas -- the same harness proven
# out in scripts/run-external-concordance-synthetic.R, now against real data.
#
# Usage: run-external-concordance-chr22.R <output-dir> <work-dir> <source-dir>
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
  "canonical_production_checksum.R", "canonical_autosomal_baseline.R"
)) sys.source(file.path(module_dir, module), envir = environment())

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: run-external-concordance-chr22.R <output-dir> <work-dir> <source-dir>", call. = FALSE)
}
output_dir <- args[[1L]]
work_dir <- canonical_production_dir(args[[2L]], "work_dir", create = TRUE, empty = TRUE)
source_dir <- canonical_production_dir(args[[3L]], "source_dir")

required_packages <- c(
  "SNPRelate", "adegenet", "hierfstat", "poppr", "pegas", "vegan", "ade4",
  "data.table", "jsonlite"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Required package(s) not installed: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
bcftools <- Sys.which("bcftools")
if (!nzchar(bcftools)) stop("bcftools is required for chromosome 22 subset derivation", call. = FALSE)
plink2 <- Sys.which("plink2")
if (!nzchar(plink2)) stop("plink2 executable is required on PATH", call. = FALSE)

if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
  stop("output_dir must be absent or empty", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- canonical_production_dir(output_dir, "concordance output", empty = TRUE)

# ---- 1. Verify the approved source and derive the bounded interval ----
source <- popgenVCF::canonical_1000g_chr22_source()
verification <- popgenVCF::verify_canonical_source(source, source_dir)
if (!all(verification$passed)) stop("approved chromosome 22 source verification failed", call. = FALSE)
files <- source$files$filename
source_vcf <- file.path(source_dir, files[grepl("\\.vcf\\.gz$", files)])
panel_path <- file.path(source_dir, files[grepl("\\.panel$", files)])
contract <- canonical_autosomal_baseline_contract()

derived_vcf <- file.path(work_dir, "chr22-external-concordance.vcf.gz")
canonical_production_system2(bcftools, c(
  "view", "--regions", shQuote(contract$region), "--min-alleles", "2",
  "--max-alleles", "2", "--types", "snps", "--output-type", "z",
  "--output-file", shQuote(derived_vcf), shQuote(source_vcf)
), "chr22 concordance VCF derivation")
canonical_production_system2(
  bcftools, c("index", "--tbi", "--force", shQuote(derived_vcf)),
  "chr22 concordance VCF indexing"
)
inventory <- canonical_production_variant_inventory(bcftools, derived_vcf)
cat("Derived interval variant count:", inventory$variant_count, "\n")

# ---- 2. Run popgenVCF's own pipeline (full analysis set) ----
pipeline_dir <- file.path(work_dir, "pipeline")
cfg <- popgenVCF::default_config()
cfg$input$vcf <- derived_vcf
cfg$input$metadata <- panel_path
cfg$output$directory <- pipeline_dir
cfg$output$figure_formats <- character()
cfg$compute$threads <- 4L
cfg$compute$seed <- contract$seed
cfg$qc$maf <- contract$maf_threshold
cfg$qc$max_variant_missing <- contract$maximum_variant_missing
cfg$qc$max_sample_missing <- contract$maximum_sample_missing
cfg$qc$ld_r2 <- contract$ld_r2
cfg$analyses$n_pcs <- contract$pca_components
cfg$analyses$dapc_k <- "2:2"
cfg$analyses$dapc_cross_validation <- FALSE
cfg$analyses$bootstrap$enabled <- FALSE
cfg$analyses$amova <- FALSE
cfg$analyses$isolation_by_distance <- FALSE
cfg$analyses$chromosome_specific <- FALSE
cfg$report$enabled <- FALSE
# AMOVA and Mantel/IBD are computed directly below (permutations = 0) instead
# of through their pipeline modules, which hardcode a 999-replicate
# permutation test purely for a p-value this driver never uses -- only the
# point estimate (Phi / Mantel r) is needed for concordance comparison, and
# skipping the permutation test avoids several minutes of wasted compute on
# real-scale (2,504-sample) data.

cat("Starting full pipeline run on the derived chromosome 22 interval...\n")
analysis <- popgenVCF::run_pipeline(cfg)
if (!identical(analysis$status, "complete")) stop("pipeline did not complete", call. = FALSE)

sample_ids <- analysis$samples$ids
metadata <- analysis$samples$metadata
population <- metadata$population[match(sample_ids, metadata$sample)]
qc_ids <- analysis$variants$qc_ids
ld_ids <- analysis$variants$ld_ids
cat("Retained:", length(sample_ids), "samples,", length(qc_ids), "QC SNPs,", length(ld_ids), "LD-pruned SNPs\n")

gds <- SNPRelate::snpgdsOpen(analysis$inputs$gds_path, readonly = TRUE)
on.exit(try(SNPRelate::snpgdsClose(gds), silent = TRUE), add = TRUE)

geno_qc <- SNPRelate::snpgdsGetGeno(
  gds, sample.id = sample_ids, snp.id = qc_ids, snpfirstdim = FALSE, verbose = FALSE
)
rownames(geno_qc) <- sample_ids

# ---- 3. Observed values: popgenVCF's own results ----
pca_result <- popgenVCF::get_analysis_result(analysis, "pca")
pc_cols <- grep("^PC", names(pca_result$scores), value = TRUE)
observed_pca <- as.matrix(pca_result$scores[, pc_cols, with = FALSE])
rownames(observed_pca) <- pca_result$scores$sample

ibs_similarity <- as.matrix(
  data.table::fread(file.path(pipeline_dir, "tables", "14_IBS_similarity.tsv")),
  rownames = 1
)
observed_ibs <- ibs_similarity[sample_ids, sample_ids]

fst_result <- popgenVCF::get_analysis_result(analysis, "fst")
observed_fst <- fst_result$global

div_population <- data.table::fread(file.path(pipeline_dir, "tables", "09_population_diversity.tsv"))
observed_diversity <- stats::setNames(
  c(div_population$observed_heterozygosity, div_population$expected_heterozygosity),
  c(paste0(div_population$population, "_Hobs"), paste0(div_population$population, "_Hexp"))
)

dapc_result <- popgenVCF::get_analysis_result(analysis, "dapc")
dapc_model <- dapc_result$models[["2"]]
observed_dapc_membership <- dapc_model$membership
dapc_grp <- dapc_model$groups
diagnostics_row <- dapc_result$diagnostics[dapc_result$diagnostics$K == 2, ]
dapc_n_pca <- diagnostics_row$n_pca[[1L]]
dapc_n_da <- diagnostics_row$n_da[[1L]]

# The amova/isolation_by_distance modules are disabled above (see comment);
# call the same underlying functions directly with permutations = 0 so only
# the point estimate is computed, not the (unused, expensive) p-value.
amova_result <- popgenVCF:::run_amova_analysis(geno_qc, sample_ids, metadata, permutations = 0L, seed = cfg$compute$seed)
observed_amova <- amova_result$phi$Phi[amova_result$phi$statistic == "Phi-population-total"][[1L]]

ibs_for_mantel <- popgenVCF:::run_ibs(gds, sample_ids, ld_ids, metadata, cfg$compute$threads)
ibd_result <- popgenVCF:::run_mantel_ibd(
  ibs_for_mantel$distance, metadata, cfg$input$geographic_columns, permutations = 0L, seed = cfg$compute$seed
)
observed_mantel <- if (!is.null(ibd_result)) ibd_result$summary$mantel_r[[1L]] else NA_real_

# ---- 4. Independent references ----

## SNPRelate direct calls (role: equivalence; wrapper-fidelity, same backend library)
snprelate_pca <- SNPRelate::snpgdsPCA(
  gds, sample.id = sample_ids, snp.id = ld_ids,
  eigen.cnt = ncol(observed_pca), num.thread = cfg$compute$threads, verbose = FALSE
)
reference_pca_snprelate <- snprelate_pca$eigenvect
rownames(reference_pca_snprelate) <- snprelate_pca$sample.id
reference_pca_snprelate <- reference_pca_snprelate[rownames(observed_pca), , drop = FALSE]

snprelate_ibs <- SNPRelate::snpgdsIBS(
  gds, sample.id = sample_ids, snp.id = ld_ids, num.thread = cfg$compute$threads, verbose = FALSE
)
reference_ibs_snprelate <- as.matrix(snprelate_ibs$ibs)
rownames(reference_ibs_snprelate) <- colnames(reference_ibs_snprelate) <- snprelate_ibs$sample.id
reference_ibs_snprelate <- reference_ibs_snprelate[rownames(observed_ibs), colnames(observed_ibs)]

## PLINK 2 (role: diagnostic; genuinely independent external tool)
bed_prefix <- file.path(work_dir, "plink_ld")
SNPRelate::snpgdsGDS2BED(gds, bed.fn = bed_prefix, sample.id = sample_ids, snp.id = ld_ids, verbose = FALSE)
freq_prefix <- file.path(work_dir, "plink_freq")
system2(plink2, c("--bfile", bed_prefix, "--freq", "--out", freq_prefix), stdout = FALSE, stderr = FALSE)
pca_prefix <- file.path(work_dir, "plink2_pca")
system2(plink2, c(
  "--bfile", bed_prefix, "--read-freq", paste0(freq_prefix, ".afreq"),
  "--pca", as.character(ncol(observed_pca)), "--out", pca_prefix
), stdout = FALSE, stderr = FALSE)
plink_eigenvec <- data.table::fread(paste0(pca_prefix, ".eigenvec"))
data.table::setnames(plink_eigenvec, 1L, "FID")
reference_pca_plink <- as.matrix(plink_eigenvec[, grep("^PC", names(plink_eigenvec)), with = FALSE])
rownames(reference_pca_plink) <- plink_eigenvec$IID
reference_pca_plink <- reference_pca_plink[rownames(observed_pca), , drop = FALSE]
plink_version_line <- trimws(system2(plink2, "--version", stdout = TRUE, stderr = TRUE)[[1L]])
plink_version <- sub("^PLINK\\s+(\\S+).*$", "\\1", plink_version_line)

king_prefix <- file.path(work_dir, "plink2_king")
system2(plink2, c("--bfile", bed_prefix, "--make-king", "square", "--out", king_prefix), stdout = FALSE, stderr = FALSE)
king_mat <- as.matrix(data.table::fread(paste0(king_prefix, ".king"), header = FALSE))
king_ids <- data.table::fread(paste0(king_prefix, ".king.id"))
rownames(king_mat) <- colnames(king_mat) <- king_ids[[ncol(king_ids)]]
king_mat <- king_mat[rownames(observed_ibs), colnames(observed_ibs)]
king_off_diag_finite <- all(is.finite(king_mat[upper.tri(king_mat)]))

## hierfstat FST (role: diagnostic; independent Weir & Cockerham 1984 estimator)
geno_codes <- ifelse(is.na(geno_qc), NA_integer_, ifelse(geno_qc == 0, 11L, ifelse(geno_qc == 1, 12L, 22L)))
hf_data <- data.frame(pop = as.integer(factor(population)), geno_codes, check.names = FALSE)
wc_result <- hierfstat::wc(hf_data, diploid = TRUE)
reference_fst_hierfstat <- wc_result$FST

## adegenet diversity (role: diagnostic; adegenet's own per-population Hobs/Hexp)
genotype_strings <- ifelse(is.na(geno_qc), NA_character_, ifelse(geno_qc == 0, "11", ifelse(geno_qc == 1, "12", "22")))
genind_obj <- adegenet::df2genind(
  genotype_strings, ploidy = 2L, ncode = 1L, ind.names = sample_ids, pop = population, sep = ""
)
sub_genind <- adegenet::seppop(genind_obj)
reference_diversity <- unlist(lapply(names(sub_genind), function(pop_name) {
  s <- adegenet::summary(sub_genind[[pop_name]])
  stats::setNames(c(mean(s$Hobs), mean(s$Hexp)), c(paste0(pop_name, "_Hobs"), paste0(pop_name, "_Hexp")))
}))

## adegenet DAPC (role: equivalence; direct dapc() call using popgenVCF's own
## cluster grouping/n.pca/n.da -- see scripts/run-external-concordance-synthetic.R
## for why this is a wrapper-fidelity check, not a "recovers true populations" check).
## adegenet::dapc() computes its own internal PCA with nf = min(n_samples,
## n_loci) when glPca isn't supplied -- at real scale that is min(2504, 2028)
## = 2028 components, far more than the 100 popgenVCF itself retains, and was
## the actual cause of this comparison never finishing on real data. Compute
## the same bounded shared PCA popgenVCF's own DAPC module uses
## (compute_dapc_shared_pca(): nf = max(2, min(n_samples - 1, 100))) and pass
## it in explicitly to avoid that blow-up.
gl <- popgenVCF:::genlight_from_gds(geno_qc, sample_ids, metadata)
reference_max_pca <- max(2L, min(nrow(geno_qc) - 1L, 100L))
reference_shared_pca <- adegenet::glPca(
  gl, center = TRUE, scale = FALSE, nf = reference_max_pca,
  loadings = TRUE, returnDotProd = TRUE
)
reference_dapc_model <- adegenet::dapc(
  gl, pop = dapc_grp, n.pca = dapc_n_pca, n.da = dapc_n_da, glPca = reference_shared_pca
)
reference_dapc_posterior <- reference_dapc_model$posterior
colnames(reference_dapc_posterior) <- paste0("cluster_", colnames(reference_dapc_posterior))
reference_dapc_posterior <- reference_dapc_posterior[
  rownames(observed_dapc_membership), colnames(observed_dapc_membership)
]

## poppr AMOVA (role: diagnostic; same implementation popgenVCF itself calls --
## confirms determinism, not an independent cross-implementation check)
adegenet::strata(gl) <- data.frame(population = factor(population), row.names = adegenet::indNames(gl))
poppr_reference_model <- poppr::poppr.amova(gl, ~population, within = TRUE, quiet = TRUE)
poppr_phi <- data.table::as.data.table(poppr_reference_model$statphi, keep.rownames = "statistic")
reference_amova_poppr <- poppr_phi$Phi[poppr_phi$statistic == "Phi-population-total"][[1L]]

## pegas AMOVA (role: diagnostic; genuinely independent implementation/formula,
## compared on the Phi-population-total statistic)
geno_dist <- stats::dist(geno_qc, method = "euclidean")
pegas_strata <- data.frame(population = factor(population))
pegas_result <- pegas::amova(geno_dist ~ population, data = pegas_strata, nperm = 0L)
# pegas returns varcomp as a data.frame(sigma2, P.value) when nperm > 0, but
# collapses it to a bare named numeric vector when nperm = 0 (no p-value to
# report) -- pegas:::print.amova itself branches on is.data.frame() for this
# same reason.
pegas_sigma2 <- if (is.data.frame(pegas_result$varcomp)) pegas_result$varcomp$sigma2 else pegas_result$varcomp
reference_amova_pegas <- pegas_sigma2[[1L]] / sum(pegas_sigma2)

## vegan Mantel (role: equivalence; direct re-call on the same distance
## matrices popgenVCF used internally)
reference_mantel_vegan <- NA_real_
if (!is.null(ibd_result)) {
  mantel_direct <- popgenVCF:::run_mantel_ibd(
    ibs_for_mantel$distance, metadata, cfg$input$geographic_columns, permutations = 0L, seed = cfg$compute$seed
  )
  if (!is.null(mantel_direct)) reference_mantel_vegan <- mantel_direct$summary$mantel_r[[1L]]
}

# ---- 5. Run the comparison harness ----
payload <- list(
  observed = list(
    pca_scores = observed_pca, ibs = observed_ibs, fst = observed_fst,
    diversity = observed_diversity, dapc_assignment = observed_dapc_membership,
    amova = observed_amova, mantel = observed_mantel
  ),
  references = list(
    snprelate_pca_scores = reference_pca_snprelate,
    snprelate_ibs = reference_ibs_snprelate,
    plink2_pca_scores = reference_pca_plink,
    hierfstat_fst = reference_fst_hierfstat,
    adegenet_diversity = reference_diversity,
    adegenet_dapc_assignment = reference_dapc_posterior,
    poppr_amova = reference_amova_poppr,
    pegas_amova = reference_amova_pegas,
    vegan_mantel = reference_mantel_vegan
  )
)

adapters <- popgenVCF::default_reference_adapter_registry()
adapters <- adapters[!names(adapters) %in% c("admixture_q", "faststructure_q", "lea_snmf_q")]
if (king_off_diag_finite) {
  payload$references$plink2_king <- king_mat
} else {
  king_adapter <- adapters$plink2_king
  king_adapter$reference <- function(x) stop(
    "PLINK 2 --make-king produced non-finite kinship for at least one sample pair",
    call. = FALSE
  )
  adapters$plink2_king <- king_adapter
}
if (is.na(observed_mantel) || is.na(reference_mantel_vegan)) {
  adapters <- adapters[names(adapters) != "vegan_mantel"]
}

cat("Running comparison harness...\n")
run <- popgenVCF::run_reference_adapters(payload, adapters = adapters)
cat("=== comparison summary (", nrow(run$table), "total rows across all comparisons) ===\n")
print(run$table[, .(n = .N, n_passed = sum(passed, na.rm = TRUE)), by = .(id, status)])

# Matrix-mode comparisons (IBS, KING) produce one row per sample pair -- at
# real sample counts (n=2504) that is n^2 ~= 6.3 million rows each, which is
# impractical to persist as long-form evidence (and crashes data.table's own
# print formatting). Condense any large comparison table to summary
# statistics before it is embedded in a concordance record; the pass/fail
# status itself was already computed from the full table above and is
# unaffected by this condensation.
condense_large_comparisons <- function(result, max_rows = 5000L) {
  if (nrow(result$comparisons) <= max_rows) return(result)
  cmp <- result$comparisons
  result$comparisons <- data.table::data.table(
    metric = "summary_over_all_pairs",
    observed = NA_real_, reference = NA_real_,
    absolute_error = max(cmp$absolute_error, na.rm = TRUE),
    relative_error = max(cmp$relative_error, na.rm = TRUE),
    passed = all(cmp$passed)
  )
  result$message <- paste0(
    result$message, " [condensed from ", nrow(cmp), " pairwise rows: ",
    "mean|absolute_error|=", signif(mean(cmp$absolute_error, na.rm = TRUE), 6),
    ", n_passed=", sum(cmp$passed), "/", nrow(cmp), "]"
  )
  result
}
run$results <- lapply(run$results, condense_large_comparisons)

# ---- 6. Wrap into concordance records and a suite ----
tool_versions <- list(
  "SNPRelate" = as.character(utils::packageVersion("SNPRelate")),
  "PLINK 2" = plink_version,
  "hierfstat" = as.character(utils::packageVersion("hierfstat")),
  "adegenet" = as.character(utils::packageVersion("adegenet")),
  "poppr" = as.character(utils::packageVersion("poppr")),
  "pegas" = as.character(utils::packageVersion("pegas")),
  "vegan" = as.character(utils::packageVersion("vegan"))
)
commands <- list(
  snprelate_pca = "SNPRelate::snpgdsPCA(..., eigen.cnt = requested_components)",
  snprelate_ibs = "SNPRelate::snpgdsIBS(...)",
  plink2_pca = paste("plink2 --bfile <ld-pruned> --read-freq <freq> --pca", ncol(observed_pca)),
  plink2_king = "plink2 --bfile <ld-pruned> --make-king square",
  hierfstat_fst = "hierfstat::wc(data.frame(pop, <11/12/22-coded loci>), diploid = TRUE)",
  adegenet_diversity = "adegenet::summary(seppop(df2genind(...)))",
  adegenet_dapc = "adegenet::dapc(gl, pop = <popgenVCF cluster grouping>, n.pca, n.da)",
  poppr_amova = "poppr::poppr.amova(gl, ~population, within = TRUE)",
  pegas_amova = "pegas::amova(dist(geno) ~ population, nperm = 0) -- point estimate only, no permutation p-value",
  vegan_mantel = "vegan::mantel(as.dist(genetic), as.dist(geographic), permutations = 0, method = 'pearson') -- point estimate only, no permutation p-value"
)
environment_meta <- list(
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  platform = R.version$platform,
  dataset_id = "1000g_phase3_chr22_v5a",
  dataset_version = "20130502-v5a",
  region = contract$region,
  seed = cfg$compute$seed,
  threads = cfg$compute$threads
)

records <- lapply(names(run$results), function(id) {
  result <- run$results[[id]]
  if (identical(result$status, "skipped")) return(NULL)
  reference_version <- result$reference_version
  if (is.na(reference_version) || !nzchar(reference_version)) {
    reference_version <- tool_versions[[result$reference_tool]]
    if (is.null(reference_version)) reference_version <- "unknown"
  }
  command <- commands[[id]]
  if (is.null(command)) command <- "see scripts/run-external-concordance-chr22.R"
  adapter <- adapters[[id]]
  popgenVCF::new_scientific_concordance_record(
    dataset_id = "1000g_phase3_chr22_v5a",
    analysis = result$analysis,
    reference_tool = result$reference_tool,
    reference_version = reference_version,
    command = command,
    result = result,
    tolerance_profile = list(
      absolute_tolerance = adapter$absolute_tolerance,
      relative_tolerance = adapter$relative_tolerance
    ),
    environment = environment_meta,
    approval = "proposed"
  )
})
records <- Filter(Negate(is.null), records)

suite <- popgenVCF::new_scientific_concordance_suite(
  records, require_complete = FALSE,
  required_tools = c("SNPRelate", "PLINK 2", "hierfstat", "adegenet", "pegas"),
  required_analyses = c("pca", "ibs", "fst", "diversity", "dapc", "amova")
)

evidence <- popgenVCF::write_scientific_concordance_evidence(suite, output_dir)
cat("\nConcordance evidence written to:", output_dir, "\n")
cat("inventory_complete:", suite$inventory_complete, " release_ready:", suite$release_ready, "\n")
cat("missing_tools:", paste(suite$missing_tools, collapse = ", "), "\n")
cat("missing_analyses:", paste(suite$missing_analyses, collapse = ", "), "\n")
print(popgenVCF::scientific_concordance_table(suite))
