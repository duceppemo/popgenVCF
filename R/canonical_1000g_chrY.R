#' Describe the approved 1000 Genomes Phase 3 chromosome Y source
#'
#' @return A named source specification.
#' @export
canonical_1000g_chrY_source <- function() {
  list(
    schema_version = "1.0",
    id = "1000g_phase3_chry_v2a",
    version = "20130502-v2a",
    title = "1000 Genomes Project Phase 3 chromosome Y genotypes",
    organism = "Homo sapiens",
    assembly = "GRCh37",
    doi = "10.5281/zenodo.3359882",
    license = "Zenodo open dataset; use subject to record rights",
    citation = paste(
      "The 1000 Genomes Project Consortium (2015).",
      "A global reference for human genetic variation. Nature 526:68-74.",
      "doi:10.1038/nature15393"
    ),
    reviewed_by = "popgenVCF scientific validation maintainers",
    reviewed_at = "2026-07-22",
    chromosome_scope = "chrY",
    sample_sex_policy = "male_only",
    analyses = c("diversity", "fst", "pca", "ibs", "tree"),
    files = data.frame(
      filename = c(
        "ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz",
        "ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz.tbi",
        "integrated_call_male_samples_v3.20130502.ALL.panel"
      ),
      upstream_md5 = c(
        "388fb466c983d4bec2082941647409f3",
        "fa37e14805cce3221f6f9d3a4cd749a4",
        "d1e59867b4d4ce43092a45a479496b80"
      ),
      source = c(
        "https://zenodo.org/records/3359882/files/ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz?download=1",
        "https://zenodo.org/records/3359882/files/ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz.tbi?download=1",
        "https://zenodo.org/records/3359882/files/integrated_call_male_samples_v3.20130502.ALL.panel?download=1"
      ),
      stringsAsFactors = FALSE
    )
  )
}

#' Describe the approved 1000 Genomes Phase 3 chromosome 22 source
#'
#' Chromosome 22 is the smallest autosome in the archived Phase 3 callset and
#' retains all 2,504 samples, making it the bounded production input for
#' diploid, mixed-sex population-genetic validation.
#'
#' @return A named source specification.
#' @export
canonical_1000g_chr22_source <- function() {
  filenames <- c(
    "ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz",
    "ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz.tbi",
    "integrated_call_samples_v3.20130502.ALL.panel"
  )
  list(
    schema_version = "1.0", id = "1000g_phase3_chr22_v5a",
    version = "20130502-v5a",
    title = "1000 Genomes Project Phase 3 chromosome 22 genotypes",
    organism = "Homo sapiens", assembly = "GRCh37",
    doi = "10.5281/zenodo.3359882",
    license = "Zenodo open dataset; use subject to record rights",
    citation = paste(
      "The 1000 Genomes Project Consortium (2015).",
      "A global reference for human genetic variation. Nature 526:68-74.",
      "doi:10.1038/nature15393"
    ),
    reviewed_by = "popgenVCF scientific validation maintainers",
    reviewed_at = "2026-07-23", chromosome_scope = "chr22",
    sample_sex_policy = "mixed",
    analyses = c("diversity", "fst", "pca", "ibs", "mds", "dapc", "amova", "tree"),
    files = data.frame(
      filename = filenames,
      upstream_md5 = c(
        "ad7d6e0c05edafd7faed7601f7f3eaba",
        "4202e9a481aa8103b471531a96665047",
        "7ee5675553088230530a7fe88c22f201"
      ),
      source = paste0("https://zenodo.org/records/3359882/files/", filenames, "?download=1"),
      stringsAsFactors = FALSE
    )
  )
}

#' Validate an approved canonical source specification
#' @param source Canonical source specification.
#' @return `source`, invisibly.
#' @export
validate_canonical_source <- function(source) {
  required <- c("id", "version", "title", "license", "citation", "doi",
                "reviewed_by", "reviewed_at", "chromosome_scope",
                "sample_sex_policy", "files")
  if (!is.list(source) || !all(required %in% names(source)))
    stop("invalid canonical source specification", call. = FALSE)
  files <- as.data.frame(source$files, stringsAsFactors = FALSE)
  columns <- c("filename", "upstream_md5", "source")
  if (!nrow(files) || !all(columns %in% names(files)))
    stop("canonical source requires a complete file inventory", call. = FALSE)
  if (anyDuplicated(files$filename) || any(!grepl("^[a-f0-9]{32}$", files$upstream_md5)))
    stop("canonical source MD5 inventory is invalid", call. = FALSE)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", source$reviewed_at))
    stop("canonical source review date must be ISO-8601", call. = FALSE)
  if (!source$sample_sex_policy %in% c("male_only", "mixed"))
    stop("canonical source sample sex policy is invalid", call. = FALSE)
  invisible(source)
}

#' Verify locally staged files against the approved upstream inventory
#' @param source Canonical source specification.
#' @param directory Directory containing all source files.
#' @return Deterministic verification table.
#' @export
verify_canonical_source <- function(source, directory) {
  validate_canonical_source(source)
  rows <- lapply(seq_len(nrow(source$files)), function(i) {
    spec <- source$files[i, , drop = FALSE]
    path <- file.path(directory, spec$filename)
    exists <- file.exists(path)
    size <- if (exists) unname(file.info(path)$size) else NA_real_
    md5 <- if (exists) unname(tools::md5sum(path)) else NA_character_
    sha256 <- if (exists) tolower(digest::digest(path, algo = "sha256", file = TRUE)) else NA_character_
    data.frame(filename = spec$filename, exists = exists,
      observed_size = size, expected_md5 = spec$upstream_md5,
      observed_md5 = md5,
      md5_ok = exists && identical(tolower(md5), spec$upstream_md5),
      sha256 = sha256, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$passed <- out$exists & out$md5_ok & grepl("^[a-f0-9]{64}$", out$sha256)
  rownames(out) <- NULL
  out
}

#' Promote an approved source into a SHA-256 canonical dataset
#' @param source Canonical source specification.
#' @param directory Verified local source directory.
#' @return A `PopgenVCFCanonicalDataset`.
#' @export
canonical_dataset_from_source <- function(source, directory) {
  verification <- verify_canonical_source(source, directory)
  if (!all(verification$passed))
    stop("approved canonical source verification failed", call. = FALSE)
  files <- merge(source$files[c("filename", "source")],
                 verification[c("filename", "observed_size", "sha256")],
                 by = "filename", sort = TRUE)
  names(files)[names(files) == "observed_size"] <- "size_bytes"
  new_canonical_dataset(
    id = source$id, version = source$version, title = source$title,
    license = source$license, citation = source$citation,
    organism = source$organism, analyses = source$analyses,
    files = files[c("filename", "sha256", "size_bytes", "source")],
    metadata = list(assembly = source$assembly, doi = source$doi,
      reviewed_by = source$reviewed_by, reviewed_at = source$reviewed_at,
      upstream_digest = "MD5", upstream_archive = "Zenodo")
  )
}

#' Create an approved canonical registry from a verified source
#' @param directory Directory containing verified source files.
#' @param source Canonical source specification. Defaults to the approved
#'   1000 Genomes chromosome Y source.
#' @return A registry with one approved SHA-256 descriptor.
#' @export
approved_1000g_chrY_registry <- function(
    directory,
    source = canonical_1000g_chrY_source()) {
  validate_canonical_source(source)
  descriptor <- canonical_dataset_from_source(source, directory)
  register_canonical_dataset(new_canonical_dataset_registry(), descriptor,
    approval = "approved", reviewed_by = source$reviewed_by,
    reviewed_at = source$reviewed_at,
    notes = paste("Approved upstream archive", source$doi,
                  "verified by MD5 and promoted to SHA-256."))
}

#' Write evidence for the first approved canonical source
#' @param source Canonical source specification.
#' @param directory Verified local source directory.
#' @param output_dir Evidence output directory.
#' @return Named evidence paths.
#' @export
write_approved_canonical_source_evidence <- function(source, directory, output_dir) {
  verification <- verify_canonical_source(source, directory)
  if (!all(verification$passed)) stop("canonical source verification failed", call. = FALSE)
  descriptor <- canonical_dataset_from_source(source, directory)
  registry <- approved_1000g_chrY_registry(directory, source = source)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  source_path <- file.path(output_dir, "canonical_source_verification.tsv")
  registry_path <- file.path(output_dir, "canonical_dataset_registry.tsv")
  data.table::fwrite(verification, source_path, sep = "\t", quote = FALSE, na = "NA")
  write_canonical_dataset_registry(registry, registry_path)
  descriptor_paths <- write_canonical_validation_evidence(
    descriptor, directory, file.path(output_dir, "dataset"))
  c(source_verification = normalizePath(source_path),
    registry = normalizePath(registry_path), unlist(descriptor_paths, use.names = TRUE))
}
