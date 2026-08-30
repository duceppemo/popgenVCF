# Header/column-name normalization used throughout metadata parsing: lowercase
# first, THEN collapse non-alphanumeric runs to "_". Applying gsub() before
# tolower() (as this line originally did, and as a copy elsewhere in this
# file did too) is a real, previously-undiscovered bug: the character class
# below is deliberately lowercase-only, so under the wrong order any
# uppercase letter -- not just ones next to punctuation -- fails to match,
# gets replaced with "_" on its own, and silently breaks auto-detection for
# any ordinarily-capitalized header ("Population", "SampleID", ...).
normalize_metadata_name <- function(x) {
  gsub("[^a-z0-9]+", "_", tolower(x))
}

# Renames the column matching `requested` (normalized the same way headers
# are, so "Pathotype"/"pathotype"/"patho-type" all match a header literally
# spelled any of those ways) to `target` ("sample" or "population"),
# bypassing the fixed built-in synonym list entirely -- an explicit
# input.sample_column/population_column always wins over auto-detection.
resolve_named_column <- function(x, requested, target) {
  wanted <- normalize_metadata_name(requested)
  hit <- which(names(x) == wanted)
  if (!length(hit)) {
    stopf(
      "input.%s_column = \"%s\" not found in metadata columns: %s",
      target, requested, paste(names(x), collapse = ", ")
    )
  }
  if (!identical(names(x)[hit], target) && target %in% names(x)) {
    stopf(
      "input.%s_column = \"%s\" conflicts with an existing \"%s\" column already present in the metadata",
      target, requested, target
    )
  }
  data.table::setnames(x, hit, target)
  invisible(NULL)
}

read_metadata <- function(path, header = "auto", sample_column = NULL, population_column = NULL) {
  first <- readLines(path, n = 1L, warn = FALSE)
  sep <- if (grepl("\t", first)) "\t" else if (grepl(",", first)) "," else ""
  tokens <- strsplit(trimws(first), if (sep == "") "[[:space:]]+" else sep)[[1]]
  detected <- any(tolower(tokens) %in% c("sample", "sample_id", "id", "individual", "population", "pop"))
  use_header <- switch(tolower(as.character(header)), auto = detected, yes = TRUE, true = TRUE,
                       no = FALSE, false = FALSE, stopf("Invalid metadata_header: %s", header))
  if (!use_header && (!is.null(sample_column) || !is.null(population_column))) {
    stop(
      "input.sample_column/population_column require headered metadata (input.metadata_header must not be \"no\")",
      call. = FALSE
    )
  }
  x <- data.table::fread(
    path, sep = sep, header = use_header, fill = TRUE,
    data.table = TRUE, showProgress = FALSE
  )
  generated_empty <- grepl("^V[0-9]+$", names(x)) & vapply(x, function(column) {
    all(is.na(column) | !nzchar(trimws(as.character(column))))
  }, logical(1))
  if (any(generated_empty)) {
    x[, (names(x)[generated_empty]) := NULL]
  }
  if (!use_header) {
    if (ncol(x) < 1L) stop("Headerless metadata requires at least one column", call. = FALSE)
    data.table::setnames(x, 1L, "sample")
    if (ncol(x) >= 2L) data.table::setnames(x, 2L, "population")
  } else {
    nm <- normalize_metadata_name(names(x))
    # Two differently-punctuated raw headers (e.g. "Sample.ID" and
    # "Sample_ID", merged from two data sources) can normalize to the same
    # name -- data.table::setnames() happily accepts duplicate names, and
    # the auto-detect path below (data.table::setnames(x, sc, "sample")
    # etc.) silently renames only the FIRST match, discarding the second
    # column's data with nothing but an easy-to-miss R-level warning. Every
    # other column-name ambiguity in this file (a duplicate final sample ID,
    # an explicit *_column colliding with an existing column) is a loud
    # stop(), not a silent pick; this matches that convention instead of
    # being the one silent exception.
    dup_names <- unique(nm[duplicated(nm)])
    collisions <- split(names(x), nm)[dup_names]
    if (length(collisions)) {
      stopf(
        "Metadata columns collide after header normalization (lowercased, punctuation collapsed to \"_\"): %s -- rename them to be unambiguous",
        paste(vapply(names(collisions), function(key) {
          sprintf("%s all normalize to \"%s\"", paste(sprintf('"%s"', collisions[[key]]), collapse = " and "), key)
        }, character(1L)), collapse = "; ")
      )
    }
    data.table::setnames(x, nm)
    if (!is.null(sample_column)) {
      resolve_named_column(x, sample_column, "sample")
    } else {
      sc <- intersect(c("sample", "sample_id", "id", "individual", "individual_id"), names(x))[1]
      if (is.na(sc)) stop("Metadata must contain a sample column", call. = FALSE)
      data.table::setnames(x, sc, "sample")
    }
    if (!is.null(population_column)) {
      resolve_named_column(x, population_column, "population")
    } else {
      pc <- intersect(c("population", "pop"), names(x))[1]
      if (!is.na(pc) && !identical(pc, "population")) data.table::setnames(x, pc, "population")
    }
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
  normalize_sample_aliases(x)
}

metadata_from_samples <- function(sample_ids) {
  normalize_sample_aliases(data.table::data.table(sample = as.character(sample_ids)))
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
                              metadata_supplied = TRUE, snp_ids = NULL) {
  vcf_samples <- as.character(ids$sample)
  if (isTRUE(metadata_supplied)) {
    metadata <- validate_metadata_sample_ids(metadata, vcf_samples)
  } else {
    metadata <- metadata_from_samples(vcf_samples)
  }
  metadata <- normalize_sample_aliases(metadata)
  public_samples <- public_sample_ids(metadata, vcf_samples)

  # Sex chromosomes are not uniformly present across samples the way
  # autosomes are (e.g. chromosome Y has no calls at all for a non-Y sex),
  # so including them here would inflate per-sample missingness for one
  # sex only and fail them out of sample QC entirely -- a much larger, more
  # severe version of the same pooling problem `qc.autosome_only` already
  # fixes for kinship/PCA/etc. `snp_ids`, when supplied, restricts this
  # missingness calculation the same way.
  geno <- SNPRelate::snpgdsGetGeno(
    gds, sample.id = vcf_samples, snp.id = snp_ids, snpfirstdim = FALSE, verbose = FALSE
  )
  missing <- rowMeans(is.na(geno))
  rm(geno)
  population <- if ("population" %in% names(metadata)) {
    metadata$population
  } else rep(NA_character_, length(vcf_samples))
  qc <- data.table::data.table(
    sample = public_samples,
    vcf_sample = vcf_samples,
    alias = metadata$alias,
    population = population,
    missing_rate = missing,
    retained = missing <= max_missing
  )
  keep_vcf <- qc[retained == TRUE, vcf_sample]
  if (length(keep_vcf) < 2L) stop("Sample QC retained fewer than two samples", call. = FALSE)
  retained_metadata <- metadata[match(keep_vcf, sample)]
  list(
    sample_ids = keep_vcf,
    sample_labels = public_sample_ids(retained_metadata, keep_vcf),
    metadata = retained_metadata,
    qc = qc,
    metadata_match = data.table::data.table(
      sample = public_samples,
      vcf_sample = vcf_samples,
      alias = metadata$alias,
      present_in_vcf = TRUE,
      present_in_metadata = isTRUE(metadata_supplied),
      retained_after_qc = vcf_samples %in% keep_vcf
    )
  )
}
