#!/usr/bin/env Rscript
#
# Derives the bundled real quickstart dataset (inst/extdata/quickstart/) from
# the same already-approved chromosome 22 1000 Genomes source and bounded
# region (22:20000000-21000000) that production_baseline, the continuous
# benchmark tiers, and this session's kinship/ROH verification already use --
# not a new data-governance decision. Dev-time only: not part of the
# installed package, not run in CI. Re-run to regenerate the bundled dataset
# if the selection below is ever revised.
#
# Usage: Rscript scripts/derive-quickstart-dataset.R [source-data-dir]
#   source-data-dir defaults to the path below, wherever the canonical chr22
#   source (VCF + panel) has already been downloaded/verified locally.

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1L) args[[1L]] else stop(
  "Usage: derive-quickstart-dataset.R <source-data-dir>", call. = FALSE
)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

bcftools <- Sys.which("bcftools")
if (!nzchar(bcftools)) stop("bcftools is required", call. = FALSE)

source <- popgenVCF::canonical_1000g_chr22_source()
files <- source$files$filename
source_vcf <- file.path(data_dir, files[grepl("\\.vcf\\.gz$", files)])
panel_path <- file.path(data_dir, files[grepl("\\.panel$", files)])
if (!file.exists(source_vcf) || !file.exists(panel_path)) {
  stop("Canonical chr22 source not found under ", data_dir, call. = FALSE)
}

# 8 real populations spanning continental diversity, plus both real
# duplicate/MZ-twin pairs confirmed against this exact source earlier this
# session (a genuine cross-population duplicate, ITU/STU, and a
# same-population duplicate, LWK) -- deliberately included so the shipped
# kinship demo has real, interesting, verifiable signal.
populations <- c("GBR", "YRI", "LWK", "CHB", "ITU", "STU", "PUR", "PEL")
required_samples <- c("HG03873", "HG03998", "NA19331", "NA19334")
n_per_population <- 20L

panel <- data.table::fread(panel_path, data.table = TRUE, check.names = FALSE, fill = TRUE)
data.table::setnames(panel, tolower(names(panel)))
panel <- panel[pop %in% populations]

set.seed(42L)
selected <- panel[, {
  pool <- sample
  forced <- intersect(pool, required_samples)
  remaining <- setdiff(pool, forced)
  take <- max(0L, n_per_population - length(forced))
  chosen <- c(forced, sample(remaining, min(take, length(remaining))))
  .(sample = chosen)
}, by = pop]
stopifnot(all(required_samples %in% selected$sample))
selected <- selected[order(pop, sample)]
cat("Selected", nrow(selected), "samples across", data.table::uniqueN(selected$pop), "populations\n")
print(selected[, .N, by = pop])

work_dir <- tempfile("popgenvcf-quickstart-derive-")
dir.create(work_dir)
samples_file <- file.path(work_dir, "samples.txt")
writeLines(selected$sample, samples_file)

out_dir <- file.path(repo_root, "inst", "extdata", "quickstart")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_vcf <- file.path(out_dir, "chr22_quickstart.vcf.gz")
contract_region <- "22:20000000-21000000"

view_args <- c(
  "view", "--regions", shQuote(contract_region),
  "--min-alleles", "2", "--max-alleles", "2", "--types", "snps",
  "-S", shQuote(samples_file),
  "--output-type", "z", "--output", shQuote(out_vcf), shQuote(source_vcf)
)
status <- system2(bcftools, view_args)
if (!identical(status, 0L)) stop("bcftools view failed", call. = FALSE)
status <- system2(bcftools, c("index", "--tbi", "--force", shQuote(out_vcf)))
if (!identical(status, 0L)) stop("bcftools index failed", call. = FALSE)

n_sites <- as.integer(system2(bcftools, c("view", "-H", shQuote(out_vcf)), stdout = TRUE) |> length())
vcf_sample_ids <- system2(bcftools, c("query", "-l", shQuote(out_vcf)), stdout = TRUE)
stopifnot(setequal(vcf_sample_ids, selected$sample))

metadata <- selected[match(vcf_sample_ids, sample)]
data.table::setnames(metadata, c("population", "sample"))
data.table::setcolorder(metadata, c("sample", "population"))
metadata_path <- file.path(out_dir, "chr22_quickstart_metadata.tsv")
data.table::fwrite(metadata, metadata_path, sep = "\t", quote = FALSE)

cat("Wrote", out_vcf, "(", n_sites, "biallelic SNP sites,", length(vcf_sample_ids), "samples)\n")
cat("Wrote", metadata_path, "\n")
