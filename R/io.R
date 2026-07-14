read_metadata <- function(path, header = "auto") {
  first <- readLines(path, n = 1L, warn = FALSE)
  sep <- if (grepl("\t", first)) "\t" else if (grepl(",", first)) "," else ""
  tokens <- strsplit(trimws(first), if (sep == "") "[[:space:]]+" else sep)[[1]]
  detected <- any(tolower(tokens) %in% c("sample", "sample_id", "id", "individual", "population", "pop"))
  use_header <- switch(tolower(as.character(header)), auto = detected, yes = TRUE, true = TRUE,
                       no = FALSE, false = FALSE, stopf("Invalid metadata_header: %s", header))
  x <- data.table::fread(path, sep = sep, header = use_header, data.table = TRUE, showProgress = FALSE)
  if (!use_header) {
    if (ncol(x) < 1L) stop("Headerless metadata requires at least one column", call. = FALSE)
    data.table::setnames(x, 1L, "sample")
    if (ncol(x) >= 2L) data.table::setnames(x, 2L, "population")
  } else {
    nm <- tolower(gsub("[^a-z0-9]+", "_", names(x)))
    data.table::setnames(x, nm)
    sc <- intersect(c("sample", "sample_id", "id", "individual", "individual_id"), names(x))[1]
    if (is.na(sc)) stop("Metadata must contain a sample column", call. = FALSE)
    data.table::setnames(x, sc, "sample")
    pc <- intersect(c("population", "pop"), names(x))[1]
    if (!is.na(pc) && !identical(pc, "population")) data.table::setnames(x, pc, "population")
  }
  x[, sample := as.character(sample)]
  x <- x[nzchar(sample)]
  if ("population" %in% names(x)) {
    x[, population := as.character(population)]
    x[!nzchar(population), population := NA_character_]
  }
  for (nm in intersect(c("latitude", "longitude"), names(x))) {
    x[, (nm) := suppressWarnings(as.numeric(get(nm)))]
  }
  if (anyDuplicated(x$sample)) {
    stopf("Duplicate metadata sample IDs: %s", paste(unique(x$sample[duplicated(x$sample)]), collapse = ", "))
  }
  x
}

metadata_from_samples <- function(sample_ids) {
  data.table::data.table(sample = as.character(sample_ids))
}

validate_metadata_sample_ids <- function(metadata, vcf_sample_ids) {
  metadata_ids <- as.character(metadata$sample)
  vcf_sample_ids <- as.character(vcf_sample_ids)
  unknown <- setdiff(metadata_ids, vcf_sample_ids)
  missing <- setdiff(vcf_sample_ids, metadata_ids)
  if (length(unknown) || length(missing)) {
    parts <- character()
    if (length(unknown)) {
      parts <- c(parts, paste0(
        "metadata IDs absent from VCF: ", paste(utils::head(unknown, 20L), collapse = ", "),
        if (length(unknown) > 20L) " ..." else ""
      ))
    }
    if (length(missing)) {
      parts <- c(parts, paste0(
        "VCF samples absent from metadata: ", paste(utils::head(missing, 20L), collapse = ", "),
        if (length(missing) > 20L) " ..." else ""
      ))
    }
    stop(
      "Metadata sample IDs must match the VCF sample IDs exactly; ",
      paste(parts, collapse = "; "),
      call. = FALSE
    )
  }
  metadata[match(vcf_sample_ids, sample)]
}

cache_manifest <- function(vcf, conversion = list(method = "biallelic.only")) {
  info <- file.info(vcf)
  list(path = normalizePath(vcf), size = unname(info$size), modified = as.character(info$mtime),
       sha256 = hash_file(vcf), conversion = conversion)
}

prepare_gds <- function(vcf, gds_path, force = FALSE) {
  manifest_path <- paste0(gds_path, ".manifest.rds")
  wanted <- cache_manifest(vcf)
  stale <- TRUE
  if (file.exists(gds_path) && file.exists(manifest_path) && !force) {
    old <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
    stale <- !identical(old, wanted)
  }
  if (force || stale || !file.exists(gds_path)) {
    unlink(c(gds_path, manifest_path), force = TRUE)
    tmp <- paste0(gds_path, ".tmp-", Sys.getpid())
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    log_msg("Converting VCF to GDS")
    SNPRelate::snpgdsVCF2GDS(vcf, tmp, method = "biallelic.only", verbose = TRUE)
    test <- SNPRelate::snpgdsOpen(tmp, readonly = TRUE)
    on.exit(try(SNPRelate::snpgdsClose(test), silent = TRUE), add = TRUE)
    required <- c("sample.id", "snp.id", "snp.chromosome", "snp.position", "genotype")
    invisible(lapply(required, function(nm) gdsfmt::index.gdsn(test, nm)))
    SNPRelate::snpgdsClose(test)
    if (!file.rename(tmp, gds_path)) stop("Could not atomically install GDS cache", call. = FALSE)
    saveRDS(wanted, manifest_path)
  } else log_msg("Using validated GDS cache")
  SNPRelate::snpgdsOpen(gds_path, readonly = TRUE)
}

get_gds_ids <- function(gds) {
  list(sample = gdsfmt::read.gdsn(gdsfmt::index.gdsn(gds, "sample.id")),
       snp = gdsfmt::read.gdsn(gdsfmt::index.gdsn(gds, "snp.id")),
       chromosome = gdsfmt::read.gdsn(gdsfmt::index.gdsn(gds, "snp.chromosome")),
       position = gdsfmt::read.gdsn(gdsfmt::index.gdsn(gds, "snp.position")),
       allele = gdsfmt::read.gdsn(gdsfmt::index.gdsn(gds, "snp.allele")))
}

harmonize_samples <- function(gds, ids, metadata, max_missing,
                              metadata_supplied = TRUE) {
  vcf_samples <- as.character(ids$sample)
  if (isTRUE(metadata_supplied)) {
    metadata <- validate_metadata_sample_ids(metadata, vcf_samples)
  } else {
    metadata <- metadata_from_samples(vcf_samples)
  }

  geno <- SNPRelate::snpgdsGetGeno(
    gds, sample.id = vcf_samples, snpfirstdim = FALSE, verbose = FALSE
  )
  missing <- rowMeans(is.na(geno))
  rm(geno)
  population <- if ("population" %in% names(metadata)) {
    metadata$population
  } else rep(NA_character_, length(vcf_samples))
  qc <- data.table::data.table(
    sample = vcf_samples,
    population = population,
    missing_rate = missing,
    retained = missing <= max_missing
  )
  keep <- qc[retained, sample]
  if (length(keep) < 2L) stop("Sample QC retained fewer than two samples", call. = FALSE)
  retained_metadata <- metadata[match(keep, sample)]
  list(
    sample_ids = keep,
    metadata = retained_metadata,
    qc = qc,
    metadata_match = data.table::data.table(
      sample = vcf_samples,
      present_in_vcf = TRUE,
      present_in_metadata = isTRUE(metadata_supplied),
      retained_after_qc = vcf_samples %in% keep
    )
  )
}
