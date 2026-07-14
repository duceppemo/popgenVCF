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
  if (anyDuplicated(x$sample)) stopf("Duplicate metadata sample IDs: %s", paste(unique(x$sample[duplicated(x$sample)]), collapse = ", "))
  x
}

metadata_from_samples <- function(sample_ids) {
  data.table::data.table(sample = as.character(sample_ids))
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

harmonize_samples <- function(gds, ids, metadata, max_missing) {
  common <- ids$sample[ids$sample %in% metadata$sample]
  if (length(common) < 2L) stop("Fewer than two matched VCF/metadata samples", call. = FALSE)
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = common, snpfirstdim = FALSE, verbose = FALSE)
  missing <- rowMeans(is.na(geno)); rm(geno)
  meta <- metadata[match(common, sample)]
  population <- if ("population" %in% names(meta)) meta$population else rep(NA_character_, length(common))
  qc <- data.table::data.table(sample = common, population = population,
                               missing_rate = missing, retained = missing <= max_missing)
  keep <- qc[retained, sample]
  if (length(keep) < 2L) stop("Sample QC retained fewer than two samples", call. = FALSE)
  list(sample_ids = keep, metadata = meta[sample %in% keep][match(keep, sample)], qc = qc)
}
