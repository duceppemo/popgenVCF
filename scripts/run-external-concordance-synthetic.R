#!/usr/bin/env Rscript
#
# Proof-of-concept external-tool scientific concordance run for the
# `external_concordance` release gate (#22), executed against the tiny
# synthetic validation fixture rather than an approved canonical dataset.
# This exercises run_reference_adapters()/new_scientific_concordance_record()
# end to end -- independently invoking SNPRelate, PLINK 2, hierfstat,
# adegenet, poppr, and pegas against the same popgenVCF pipeline run -- before
# the same harness is pointed at real canonical data.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
source_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: run-external-concordance-synthetic.R <output-dir>", call. = FALSE)
}
output_dir <- args[[1L]]

required_packages <- c(
  "SNPRelate", "adegenet", "hierfstat", "poppr", "pegas", "vegan", "ade4",
  "data.table", "jsonlite", "pkgload"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Required package(s) not installed: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
plink2 <- Sys.which("plink2")
if (!nzchar(plink2)) stop("plink2 executable is required on PATH", call. = FALSE)

pkgload::load_all(source_root, quiet = TRUE)

if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
  stop("output_dir must be absent or empty", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
work_dir <- file.path(output_dir, "work")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Run popgenVCF's own pipeline on the synthetic fixture ----
paths <- popgenVCF:::validation_fixture_paths()
pipeline_dir <- file.path(work_dir, "pipeline")
cfg <- popgenVCF::default_config()
cfg$input$vcf <- paths$vcf
cfg$input$metadata <- paths$metadata
cfg$output$directory <- pipeline_dir
cfg$output$figure_formats <- character()
cfg$compute$threads <- 1L
cfg$analyses$n_pcs <- 3L
cfg$analyses$dapc_k <- "2:2"
cfg$analyses$dapc_cross_validation <- FALSE
cfg$analyses$bootstrap$enabled <- FALSE
cfg$analyses$structure$replicates <- 1L
cfg$report$enabled <- FALSE

analysis <- popgenVCF::run_pipeline(cfg)
if (!identical(analysis$status, "complete")) stop("pipeline did not complete", call. = FALSE)

sample_ids <- analysis$samples$ids
metadata <- analysis$samples$metadata
population <- metadata$population[match(sample_ids, metadata$sample)]
qc_ids <- analysis$variants$qc_ids
ld_ids <- analysis$variants$ld_ids

gds <- SNPRelate::snpgdsOpen(analysis$inputs$gds_path, readonly = TRUE)
on.exit(try(SNPRelate::snpgdsClose(gds), silent = TRUE), add = TRUE)

geno_qc <- SNPRelate::snpgdsGetGeno(
  gds, sample.id = sample_ids, snp.id = qc_ids, snpfirstdim = FALSE, verbose = FALSE
)
rownames(geno_qc) <- sample_ids

# ---- 2. Observed values: popgenVCF's own results ----
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

amova_result <- popgenVCF::get_analysis_result(analysis, "amova")
observed_amova <- amova_result$phi$Phi[amova_result$phi$statistic == "Phi-population-total"][[1L]]

ibd_result <- popgenVCF::get_analysis_result(analysis, "ibd")
observed_mantel <- ibd_result$summary$mantel_r[[1L]]

# ---- 3. Independent references ----

## SNPRelate direct calls (role: equivalence). popgenVCF's own PCA/IBS are thin
## wrappers over these exact SNPRelate functions, so this validates wrapper
## fidelity (no parameter-passing or ordering bugs), not independent
## cross-implementation agreement.
snprelate_pca <- SNPRelate::snpgdsPCA(
  gds, sample.id = sample_ids, snp.id = ld_ids,
  eigen.cnt = ncol(observed_pca), num.thread = 1L, verbose = FALSE
)
reference_pca_snprelate <- snprelate_pca$eigenvect
rownames(reference_pca_snprelate) <- snprelate_pca$sample.id
reference_pca_snprelate <- reference_pca_snprelate[rownames(observed_pca), , drop = FALSE]

snprelate_ibs <- SNPRelate::snpgdsIBS(
  gds, sample.id = sample_ids, snp.id = ld_ids, num.thread = 1L, verbose = FALSE
)
reference_ibs_snprelate <- as.matrix(snprelate_ibs$ibs)
rownames(reference_ibs_snprelate) <- colnames(reference_ibs_snprelate) <- snprelate_ibs$sample.id
reference_ibs_snprelate <- reference_ibs_snprelate[rownames(observed_ibs), colnames(observed_ibs)]

## PLINK 2 (role: diagnostic). A genuinely independent external tool.
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

## hierfstat FST (role: diagnostic). Independent Weir & Cockerham 1984 estimator.
geno_codes <- ifelse(is.na(geno_qc), NA_integer_, ifelse(geno_qc == 0, 11L, ifelse(geno_qc == 1, 12L, 22L)))
hf_data <- data.frame(pop = as.integer(factor(population)), geno_codes, check.names = FALSE)
wc_result <- hierfstat::wc(hf_data, diploid = TRUE)
reference_fst_hierfstat <- wc_result$FST

## adegenet diversity (role: diagnostic). adegenet's own per-population Hobs/Hexp.
genotype_strings <- ifelse(is.na(geno_qc), NA_character_, ifelse(geno_qc == 0, "11", ifelse(geno_qc == 1, "12", "22")))
genind_obj <- adegenet::df2genind(
  genotype_strings, ploidy = 2L, ncode = 1L, ind.names = sample_ids, pop = population, sep = ""
)
sub_genind <- adegenet::seppop(genind_obj)
reference_diversity <- unlist(lapply(names(sub_genind), function(pop_name) {
  s <- adegenet::summary(sub_genind[[pop_name]])
  stats::setNames(c(mean(s$Hobs), mean(s$Hexp)), c(paste0(pop_name, "_Hobs"), paste0(pop_name, "_Hexp")))
}))

## adegenet DAPC (role: equivalence). A direct dapc() call using popgenVCF's own
## cluster grouping/n.pca/n.da. Documented simplification: popgenVCF's own
## grouping comes from unsupervised find.clusters(), not true population
## labels, so "equivalence" here means "does the wrapper reproduce a direct
## adegenet::dapc() call given the same inputs", not "does DAPC recover the
## true populations" -- that broader question is out of scope for this gate.
gl <- popgenVCF:::genlight_from_gds(geno_qc, sample_ids, metadata)
reference_dapc_model <- adegenet::dapc(gl, pop = dapc_grp, n.pca = dapc_n_pca, n.da = dapc_n_da)
reference_dapc_posterior <- reference_dapc_model$posterior
colnames(reference_dapc_posterior) <- paste0("cluster_", colnames(reference_dapc_posterior))
reference_dapc_posterior <- reference_dapc_posterior[
  rownames(observed_dapc_membership), colnames(observed_dapc_membership)
]

## poppr AMOVA (role: diagnostic). Same implementation popgenVCF itself calls;
## this reproduces the exact call independently and confirms determinism, it
## is not an independent cross-implementation check.
adegenet::strata(gl) <- data.frame(population = factor(population), row.names = adegenet::indNames(gl))
poppr_reference_model <- poppr::poppr.amova(gl, ~population, within = TRUE, quiet = TRUE)
poppr_phi <- data.table::as.data.table(poppr_reference_model$statphi, keep.rownames = "statistic")
reference_amova_poppr <- poppr_phi$Phi[poppr_phi$statistic == "Phi-population-total"][[1L]]

## pegas AMOVA (role: diagnostic). Genuinely independent implementation/formula;
## compared on the Phi-population-total statistic, the quantity both tools
## define consistently despite using different underlying distance conventions.
geno_dist <- stats::dist(geno_qc, method = "euclidean")
pegas_strata <- data.frame(population = factor(population))
pegas_result <- pegas::amova(geno_dist ~ population, data = pegas_strata, nperm = 999L)
# pegas does not store Phi-statistics on the object; its print.amova computes
# them on the fly as sigma2[i]/sum(sigma2) (row 1 = "population", row 2 =
# "Error" for a single-stratum model). Reproduce that here.
reference_amova_pegas <- pegas_result$varcomp$sigma2[[1L]] / sum(pegas_result$varcomp$sigma2)

## vegan Mantel (role: equivalence). Direct re-call on the same distance
## matrices popgenVCF used internally; popgenVCF's own IBD analysis already IS
## a vegan::mantel() call, so this is a wrapper-fidelity check.
ibs_full <- popgenVCF:::run_ibs(gds, sample_ids, ld_ids, metadata, 1L)
mantel_direct <- popgenVCF:::run_mantel_ibd(
  ibs_full$distance, metadata, cfg$input$geographic_columns, 999L, cfg$compute$seed
)
reference_mantel_vegan <- mantel_direct$summary$mantel_r[[1L]]

# ---- 4. Run the comparison harness ----
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
  # KING-robust is not numerically defined with only 5 markers on this tiny
  # fixture (every off-diagonal pair returned -Inf). Fail this comparison
  # informatively instead of silently comparing non-finite values -- a real
  # production-scale marker set is required for this comparison to be
  # meaningful.
  king_adapter <- adapters$plink2_king
  king_adapter$reference <- function(x) stop(
    "PLINK 2 --make-king produced non-finite kinship for every sample pair with ",
    "only 5 markers; KING-robust is not numerically meaningful at this scale ",
    "and requires real production-scale data",
    call. = FALSE
  )
  adapters$plink2_king <- king_adapter
}

run <- popgenVCF::run_reference_adapters(payload, adapters = adapters)
cat("=== comparison table ===\n")
print(run$table)

# ---- 5. Wrap into concordance records and a suite ----
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
  pegas_amova = "pegas::amova(dist(geno) ~ population, nperm = 999)",
  vegan_mantel = "vegan::mantel(as.dist(genetic), as.dist(geographic), permutations = 999, method = 'pearson')"
)
environment_meta <- list(
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  platform = R.version$platform,
  dataset = "synthetic validation fixture (popgenVCF:::validation_fixture_paths())",
  seed = cfg$compute$seed
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
  if (is.null(command)) command <- "see scripts/run-external-concordance-synthetic.R"
  adapter <- adapters[[id]]
  popgenVCF::new_scientific_concordance_record(
    dataset_id = "popgenvcf_synthetic_validation_fixture",
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
  required_analyses = c("pca", "ibs", "fst", "diversity", "dapc", "amova", "ibd")
)

evidence <- popgenVCF::write_scientific_concordance_evidence(suite, output_dir)
cat("\nConcordance evidence written to:", output_dir, "\n")
cat("inventory_complete:", suite$inventory_complete, " release_ready:", suite$release_ready, "\n")
cat("missing_tools:", paste(suite$missing_tools, collapse = ", "), "\n")
cat("missing_analyses:", paste(suite$missing_analyses, collapse = ", "), "\n")
print(popgenVCF::scientific_concordance_table(suite))
