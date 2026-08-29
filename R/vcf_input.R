vcf_command_status <- function(command, args) {
  output <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status") %||% 0L
  list(status = as.integer(status), output = output)
}

require_vcf_tool <- function(tool) {
  path <- Sys.which(tool)
  if (!nzchar(path)) {
    stop(
      "Required VCF utility '", tool, "' was not found on PATH. ",
      "Install bcftools/htslib or use the published popgenVCF container.",
      call. = FALSE
    )
  }
  unname(path)
}

vcf_index_path <- function(vcf) {
  if (file.exists(paste0(vcf, ".tbi"))) return(paste0(vcf, ".tbi"))
  if (file.exists(paste0(vcf, ".csi"))) return(paste0(vcf, ".csi"))
  NA_character_
}

vcf_index_is_valid <- function(vcf, bcftools = require_vcf_tool("bcftools")) {
  if (is.na(vcf_index_path(vcf))) return(FALSE)
  result <- vcf_command_status(bcftools, c("index", "--nrecords", shQuote(vcf)))
  identical(result$status, 0L)
}

#' Prepare a VCF input for analysis
#'
#' Accepts an uncompressed `.vcf` or compressed `.vcf.gz`. Plain VCF files,
#' ordinary gzip files, and compressed files that cannot be indexed are sorted
#' and converted to an indexed BGZF copy in `cache_dir`. A valid existing
#' Tabix/CSI index is reused. When an existing BGZF input is writable and lacks
#' an index, a Tabix index is created beside the original file.
#'
#' @param vcf Path to a `.vcf` or `.vcf.gz` file.
#' @param cache_dir Directory for normalized cached inputs.
#' @param force Recreate the normalized cached copy and index.
#' @return A list with `path`, `index`, `source`, and `normalized` fields.
#' @export
prepare_vcf_input <- function(vcf, cache_dir, force = FALSE) {
  if (!is.character(vcf) || length(vcf) != 1L || is.na(vcf) || !nzchar(vcf)) {
    stop("vcf must be one non-empty path", call. = FALSE)
  }
  vcf <- normalizePath(vcf, winslash = "/", mustWork = TRUE)
  if (!grepl("\\.vcf(?:\\.gz)?$", vcf, ignore.case = TRUE, perl = TRUE)) {
    stop("VCF input must end in .vcf or .vcf.gz: ", vcf, call. = FALSE)
  }
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("force must be TRUE or FALSE", call. = FALSE)
  }

  bcftools <- require_vcf_tool("bcftools")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)

  compressed <- grepl("\\.vcf\\.gz$", vcf, ignore.case = TRUE)
  if (compressed && !isTRUE(force) && vcf_index_is_valid(vcf, bcftools)) {
    return(list(
      path = vcf,
      index = vcf_index_path(vcf),
      source = vcf,
      normalized = FALSE
    ))
  }

  if (compressed && !isTRUE(force) && file.access(dirname(vcf), 2L) == 0L) {
    indexed <- vcf_command_status(
      bcftools,
      c("index", "--tbi", "--force", shQuote(vcf))
    )
    if (identical(indexed$status, 0L) && vcf_index_is_valid(vcf, bcftools)) {
      return(list(
        path = vcf,
        index = vcf_index_path(vcf),
        source = vcf,
        normalized = FALSE
      ))
    }
  }

  normalized <- file.path(cache_dir, "input.normalized.vcf.gz")
  index <- paste0(normalized, ".tbi")
  # Cache validity is decided by the source file's *content*, not its mtime:
  # mtime alone is unreliable (e.g. restoring an updated source from backup
  # or via `git checkout` can leave a newer-content file with an
  # equal-or-older mtime than the existing cache), and would otherwise
  # silently serve stale cached genotypes for a source that has genuinely
  # changed. Hashing the whole source file costs far less than the sort/index
  # this cache exists to avoid, so there is no fast-path short-circuit here.
  fingerprint <- paste0(normalized, ".source-sha256")
  source_hash <- digest::digest(vcf, algo = "sha256", file = TRUE)
  cached_hash <- if (file.exists(fingerprint)) trimws(readLines(fingerprint, warn = FALSE)) else character()
  cache_current <- file.exists(normalized) &&
    length(cached_hash) == 1L && identical(cached_hash, source_hash) &&
    vcf_index_is_valid(normalized, bcftools)

  if (isTRUE(force) || !cache_current) {
    unlink(c(normalized, index, paste0(normalized, ".csi"), fingerprint), force = TRUE)
    sorted <- vcf_command_status(
      bcftools,
      c("sort", "--output-type", "z", "--output", shQuote(normalized), shQuote(vcf))
    )
    if (!identical(sorted$status, 0L) || !file.exists(normalized)) {
      stop(
        "Failed to sort and BGZF-compress VCF with bcftools: ",
        paste(sorted$output, collapse = "\n"),
        call. = FALSE
      )
    }
    indexed <- vcf_command_status(
      bcftools,
      c("index", "--tbi", "--force", shQuote(normalized))
    )
    if (!identical(indexed$status, 0L) || !file.exists(index)) {
      stop(
        "Failed to create Tabix index for normalized VCF: ",
        paste(indexed$output, collapse = "\n"),
        call. = FALSE
      )
    }
    writeLines(source_hash, fingerprint)
  }

  if (!vcf_index_is_valid(normalized, bcftools)) {
    stop("The normalized VCF index could not be validated: ", normalized, call. = FALSE)
  }
  list(path = normalized, index = vcf_index_path(normalized), source = vcf, normalized = TRUE)
}

# Counts VCF records by variant type via `bcftools stats`'s SN summary
# section (a single fast streaming pass, ~0.1s even at ~100,000 records),
# and derives how many are the biallelic, polymorphic SNPs
# SNPRelate::snpgdsVCF2GDS(method = "biallelic.only") actually retains
# (prepare_gds(), R/io.R) -- real transparency for a "raw" VCF straight off
# a variant caller, which this pipeline accepts without requiring the user
# to pre-filter (see wiki/Getting-Started.md). Verified directly against a
# synthetic VCF mixing a true biallelic SNP, an insertion, a deletion, a
# multiallelic SNP, an MNP, and a structural variant: `bcftools stats`
# reports "number of SNPs" including multiallelic ones, so
# biallelic_snps_retained = snps - multiallelic_snp_sites matches the real
# GDS-retained count exactly (confirmed 2 of 7 records retained, both
# calculations agreeing) -- monomorphic-at-VCF-scale records are NOT
# additionally excluded here despite snpgdsVCF2GDS()'s own documentation
# claiming so (confirmed empirically against the installed SNPRelate: a
# monomorphic biallelic SNP was retained, not dropped).
vcf_variant_type_summary <- function(vcf, bcftools = require_vcf_tool("bcftools")) {
  result <- vcf_command_status(bcftools, c("stats", shQuote(vcf)))
  if (!identical(result$status, 0L)) {
    stop("bcftools stats failed on ", vcf, ": ", paste(result$output, collapse = "\n"), call. = FALSE)
  }
  sn <- grep("^SN\t", result$output, value = TRUE)
  fields <- strsplit(sn, "\t", fixed = TRUE)
  label <- vapply(fields, function(x) trimws(sub(":$", "", x[[3L]])), character(1L))
  value <- vapply(fields, function(x) suppressWarnings(as.integer(x[[4L]])), integer(1L))
  get_count <- function(target) {
    idx <- match(target, label)
    if (is.na(idx)) 0L else value[idx]
  }
  records <- get_count("number of records")
  snps <- get_count("number of SNPs")
  mnps <- get_count("number of MNPs")
  indels <- get_count("number of indels")
  others <- get_count("number of others")
  multiallelic_sites <- get_count("number of multiallelic sites")
  multiallelic_snp_sites <- get_count("number of multiallelic SNP sites")
  biallelic_snps_retained <- snps - multiallelic_snp_sites
  data.table::data.table(
    total_records = records,
    biallelic_snps_retained = biallelic_snps_retained,
    dropped_non_biallelic_snp = records - biallelic_snps_retained,
    snps = snps,
    multiallelic_snp_sites = multiallelic_snp_sites,
    indels = indels,
    mnps = mnps,
    multiallelic_sites_all_types = multiallelic_sites,
    other_variant_types = others
  )
}
