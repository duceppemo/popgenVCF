#!/usr/bin/env Rscript
#
# Derives the bundled real quickstart dataset (inst/extdata/quickstart/) from
# the same already-approved chromosome 22 1000 Genomes source and bounded
# region (22:20000000-21000000) that production_baseline, the continuous
# benchmark tiers, and this session's kinship/ROH verification already use --
# not a new data-governance decision. Also includes a bounded, non-PAR
# chromosome X region from the same Zenodo record (an additional file within
# an already-approved record, not a new external source), giving the sex-
# check module real data to demonstrate. Dev-time only: not part of the
# installed package, not run in CI. Re-run to regenerate the bundled dataset
# if the selection below is ever revised.
#
# Usage: Rscript scripts/derive-quickstart-dataset.R [source-data-dir]
#   source-data-dir defaults to the path below, wherever the canonical chr22
#   source (VCF + panel) and the chrX VCF/index below have already been
#   downloaded/verified locally, all co-located in the same directory.

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

# 1000 Genomes Phase 3 chromosome X genotypes, the same already-approved
# Zenodo record (10.5281/zenodo.3359882) canonical_1000g_chr22_source()/
# canonical_1000g_chrY_source() already use -- an additional file within an
# already-approved record, not a new external data source. MD5-verified
# against the record's own declared checksums before use (not just trusted).
chrx_vcf_filename <- "ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz"
chrx_vcf_md5 <- "0e44780613a245c84eedd4776021cfe5"
chrx_tbi_md5 <- "e5546b4ffea432a37835bbe3f6f6cbfc"
chrx_source_vcf <- file.path(data_dir, chrx_vcf_filename)
chrx_source_tbi <- file.path(data_dir, paste0(chrx_vcf_filename, ".tbi"))
if (!file.exists(chrx_source_vcf) || !file.exists(chrx_source_tbi)) {
  stop("chrX source VCF/index not found under ", data_dir, call. = FALSE)
}
stopifnot(
  identical(tools::md5sum(chrx_source_vcf)[[1L]], chrx_vcf_md5),
  identical(tools::md5sum(chrx_source_tbi)[[1L]], chrx_tbi_md5)
)

# 8 real populations spanning continental diversity, plus a real
# duplicate/MZ-twin pair (NA19331/NA19334, same-population LWK, confirmed
# consistent on chromosome X too -- both genuinely male) deliberately
# included so the shipped kinship demo has real, interesting, verifiable
# signal. HG03873/HG03998 (labelled as two different populations, ITU/STU)
# is also kept: real, high chr22-only kinship, but chromosome X data proved
# they are genuinely different sexes, so despite the kinship value this pair
# is NOT actually a duplicate/twin -- a real, deliberately-kept cautionary
# example of why a single autosomal window's kinship needs corroborating
# evidence. See inst/extdata/quickstart/README.md for the full story.
populations <- c("GBR", "YRI", "LWK", "CHB", "ITU", "STU", "PUR", "PEL")
required_samples <- c("HG03873", "HG03998", "NA19331", "NA19334")
n_per_population <- 20L

# Real, documented population collection-site coordinates, source:
# igsr/1000Genomes_data_indexes README_populations.md (GitHub). GBR = England
# and Scotland; YRI = Ibadan, Nigeria; LWK = Webuye, Kenya; CHB = Beijing,
# China; ITU = Indian (Telugu) samples collected in the UK; STU = Sri Lankan
# (Tamil) samples collected in the UK; PUR = Puerto Rico; PEL = Lima, Peru.
# GBR/ITU/STU share one representative London point since the source gives no
# more specific city for any of the three. These are population-level
# representative coordinates, not individual-sample GPS -- per-sample
# locations are never published for de-identified 1000 Genomes samples.
population_coordinates <- data.table::data.table(
  population = c("GBR", "YRI",    "LWK",    "CHB",     "ITU",   "STU",   "PUR",     "PEL"),
  latitude   = c(51.5074, 7.3776,  0.6075,  39.9042,  51.5074, 51.5074, 18.4655,  -12.0464),
  longitude  = c(-0.1278, 3.9059, 34.7697, 116.4074,  -0.1278, -0.1278, -66.1057, -77.0428)
)

# header="auto" misdetects this file as headerless: the header row has two
# trailing empty tab-separated fields that data rows don't, so fread's
# column-count heuristic guesses no header. header=TRUE forces correct
# parsing.
panel <- data.table::fread(panel_path, data.table = TRUE, check.names = FALSE, fill = TRUE, header = TRUE)
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
chr22_vcf <- file.path(work_dir, "chr22_subset.vcf.gz")

view_args <- c(
  "view", "--regions", shQuote(contract_region),
  "--min-alleles", "2", "--max-alleles", "2", "--types", "snps",
  "-S", shQuote(samples_file),
  "--output-type", "z", "--output", shQuote(chr22_vcf), shQuote(source_vcf)
)
status <- system2(bcftools, view_args)
if (!identical(status, 0L)) stop("bcftools view failed", call. = FALSE)
status <- system2(bcftools, c("index", "--tbi", "--force", shQuote(chr22_vcf)))
if (!identical(status, 0L)) stop("bcftools index failed", call. = FALSE)

chr22_sample_ids <- system2(bcftools, c("query", "-l", shQuote(chr22_vcf)), stdout = TRUE)
stopifnot(setequal(chr22_sample_ids, selected$sample))

# Chromosome X: a bounded, non-PAR 1Mb region (PAR1 ends at 2,699,520; PAR2
# starts at 154,931,044, GRCh37), same 1Mb size convention as the chr22
# region, same 160 samples in chr22's exact sample order (required for
# bcftools concat).
chrx_region <- "X:70000000-71000000"
chrx_view_vcf <- file.path(work_dir, "chrX_subset_raw.vcf.gz")
view_args <- c(
  "view", "--regions", shQuote(chrx_region),
  "--min-alleles", "2", "--max-alleles", "2", "--types", "snps",
  "-S", shQuote(chr22_vcf), # reuse chr22's own sample order via -S <vcf>
  "--output-type", "z", "--output", shQuote(chrx_view_vcf), shQuote(chrx_source_vcf)
)
# -S accepts a VCF/BCF as a source of both the sample list AND their order;
# bcftools resolves this from chr22_vcf's header, so the chrX subset comes
# out in the exact same sample order without a separate samples file.
status <- system2(bcftools, view_args)
if (!identical(status, 0L)) stop("bcftools view (chrX) failed", call. = FALSE)
status <- system2(bcftools, c("index", "--tbi", "--force", shQuote(chrx_view_vcf)))
if (!identical(status, 0L)) stop("bcftools index (chrX raw) failed", call. = FALSE)

# This 1000 Genomes chrX release represents males' non-PAR genotypes as
# genuinely haploid GT fields (a single allele, e.g. "0" or "1", not "0/0"/
# "1/1"), confirmed by direct inspection. SNPRelate::snpgdsVCF2GDS() does not
# parse a haploid GT field as "duplicate the observed allele" -- empirically,
# it pads the missing second allele with the ALT allele index, silently
# turning every male REF hemizygous call into a false heterozygous dosage and
# every male ALT hemizygous call into a homozygous-ALT dosage. Left
# unfixed, this would corrupt every module's genotypes at these sites, not
# just sex-check. `bcftools +fixploidy -- -f 2` rewrites every genotype in
# this region to explicit diploid form (0 -> 0/0, 1 -> 1/1), which is exactly
# correct here because the extracted region is entirely non-PAR (real X
# hemizygosity, not a representation choice this script is free to make
# differently) and already-diploid female calls are left untouched (verified
# byte-for-byte unchanged before shipping this).
chrx_vcf <- file.path(work_dir, "chrX_subset.vcf.gz")
fixploidy_out <- system2(
  bcftools, c("+fixploidy", shQuote(chrx_view_vcf), "--", "-f", "2"),
  stdout = TRUE
)
writeLines(fixploidy_out, file.path(work_dir, "chrX_subset_fixploidy.vcf"))
status <- system2(
  bcftools,
  c("view", "--output-type", "z", "--output", shQuote(chrx_vcf),
    shQuote(file.path(work_dir, "chrX_subset_fixploidy.vcf")))
)
if (!identical(status, 0L)) stop("bcftools view (chrX fixploidy compress) failed", call. = FALSE)
status <- system2(bcftools, c("index", "--tbi", "--force", shQuote(chrx_vcf)))
if (!identical(status, 0L)) stop("bcftools index (chrX fixed) failed", call. = FALSE)
stopifnot(identical(
  system2(bcftools, c("query", "-f", "'[%GT\\n]'", shQuote(chrx_vcf)), stdout = TRUE) |>
    (\(x) any(grepl("\\|", x)))(),
  TRUE
))

status <- system2(
  bcftools,
  c("concat", "--allow-overlaps", "--output-type", "z", "--output", shQuote(out_vcf),
    shQuote(chr22_vcf), shQuote(chrx_vcf))
)
if (!identical(status, 0L)) stop("bcftools concat failed", call. = FALSE)
status <- system2(bcftools, c("index", "--tbi", "--force", shQuote(out_vcf)))
if (!identical(status, 0L)) stop("bcftools index (combined) failed", call. = FALSE)

n_sites <- as.integer(system2(bcftools, c("view", "-H", shQuote(out_vcf)), stdout = TRUE) |> length())
vcf_sample_ids <- system2(bcftools, c("query", "-l", shQuote(out_vcf)), stdout = TRUE)
stopifnot(identical(vcf_sample_ids, chr22_sample_ids))

metadata <- panel[match(vcf_sample_ids, sample), .(sample, population = pop, sex = gender)]
metadata <- population_coordinates[metadata, on = "population"]
data.table::setcolorder(metadata, c("sample", "population", "latitude", "longitude", "sex"))
stopifnot(all(is.finite(metadata$latitude)), all(is.finite(metadata$longitude)))
# Real, self-reported sex from the same authoritative panel file already used
# for population assignment (integrated_call_samples_v3.20130502.ALL.panel's
# "gender" column) -- not a separate lookup or an inferred/imputed value.
stopifnot(all(metadata$sex %in% c("male", "female")))
metadata_path <- file.path(out_dir, "chr22_quickstart_metadata.tsv")
data.table::fwrite(metadata, metadata_path, sep = "\t", quote = FALSE)

cat("Wrote", out_vcf, "(", n_sites, "biallelic SNP sites,", length(vcf_sample_ids), "samples)\n")
cat("Wrote", metadata_path, "\n")
