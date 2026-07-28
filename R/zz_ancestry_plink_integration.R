plink_bundle_paths <- function(prefix) {
  stats::setNames(
    paste0(prefix, c(".bed", ".bim", ".fam")),
    c("bed", "bim", "fam")
  )
}

inspect_plink_bundle <- function(prefix, sample_ids = NULL, snp_ids = NULL) {
  paths <- plink_bundle_paths(prefix)
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    return(list(valid = FALSE, reason = paste("missing", paste0(".", missing, collapse = ", "))))
  }
  sizes <- file.info(paths)$size
  if (any(is.na(sizes) | sizes <= 0)) {
    return(list(valid = FALSE, reason = "one or more PLINK files are empty"))
  }

  fam <- tryCatch(
    data.table::fread(paths[["fam"]], header = FALSE, fill = TRUE, showProgress = FALSE),
    error = function(e) e
  )
  if (inherits(fam, "error") || ncol(fam) < 2L) {
    return(list(valid = FALSE, reason = "the .fam file is unreadable"))
  }
  ids <- as.character(fam[[2L]])
  if (!is.null(sample_ids) && !identical(ids, as.character(sample_ids))) {
    return(list(valid = FALSE, reason = "the .fam sample order does not match retained samples"))
  }

  bim <- tryCatch(
    data.table::fread(paths[["bim"]], header = FALSE, fill = TRUE, showProgress = FALSE),
    error = function(e) e
  )
  if (inherits(bim, "error") || ncol(bim) < 6L) {
    return(list(valid = FALSE, reason = "the .bim file is unreadable"))
  }
  if (!is.null(snp_ids) && nrow(bim) != length(snp_ids)) {
    return(list(valid = FALSE, reason = "the .bim variant count does not match retained SNPs"))
  }

  list(
    valid = TRUE,
    reason = NULL,
    paths = paths,
    sample_ids = ids,
    n_samples = length(ids),
    n_snps = nrow(bim)
  )
}

write_structure_sample_order <- function(sample_ids, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(sample_ids), path, useBytes = TRUE)
  path
}

prepare_structure_plink_input <- function(gds, sample_ids, snp_ids,
                                          preferred_prefix = NULL,
                                          cache_dir,
                                          converter = SNPRelate::snpgdsGDS2BED) {
  selected_sample_ids <- sample_ids
  selected_snp_ids <- snp_ids
  sample_keys <- as.character(selected_sample_ids)
  snp_keys <- as.character(selected_snp_ids)
  if (length(selected_sample_ids) < 2L) stop("PLINK export requires at least two retained samples", call. = FALSE)
  if (!length(selected_snp_ids)) stop("PLINK export requires retained SNPs", call. = FALSE)

  preferred_prefix <- as.character(preferred_prefix %||% "")[1L]
  if (nzchar(preferred_prefix)) {
    preferred_prefix <- path.expand(preferred_prefix)
    preferred <- inspect_plink_bundle(preferred_prefix, sample_keys, snp_keys)
    if (isTRUE(preferred$valid)) {
      sample_file <- write_structure_sample_order(
        preferred$sample_ids,
        file.path(cache_dir, "ancestry", "configured_plink.samples.txt")
      )
      log_msg(
        "Using configured PLINK bundle: ", preferred_prefix,
        " (", preferred$n_samples, " samples; ", preferred$n_snps, " SNPs)",
        level = "INFO"
      )
      return(list(
        prefix = preferred_prefix,
        sample_file = sample_file,
        source = "configured",
        n_samples = preferred$n_samples,
        n_snps = preferred$n_snps
      ))
    }
    log_msg(
      "Configured PLINK prefix is unavailable or incompatible (", preferred$reason,
      "); generating a canonical bundle from the retained GDS data",
      level = "WARNING"
    )
  }

  ancestry_dir <- file.path(cache_dir, "ancestry")
  dir.create(ancestry_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- file.path(ancestry_dir, "popgenVCF_structure")
  paths <- plink_bundle_paths(prefix)
  manifest_file <- paste0(prefix, ".manifest.rds")
  sample_file <- paste0(prefix, ".samples.txt")
  signature <- digest::digest(
    list(sample_ids = sample_keys, snp_ids = snp_keys),
    algo = "sha256",
    serialize = TRUE
  )

  manifest <- if (file.exists(manifest_file)) {
    tryCatch(readRDS(manifest_file), error = function(e) NULL)
  } else NULL
  cached <- inspect_plink_bundle(prefix, sample_keys, snp_keys)
  if (isTRUE(cached$valid) && identical(manifest$signature, signature)) {
    write_structure_sample_order(cached$sample_ids, sample_file)
    log_msg(
      "Reusing canonical PLINK bundle: ", prefix,
      " (", cached$n_samples, " samples; ", cached$n_snps, " SNPs)",
      level = "INFO"
    )
    return(list(
      prefix = prefix,
      sample_file = sample_file,
      source = "cache",
      n_samples = cached$n_samples,
      n_snps = cached$n_snps
    ))
  }

  temporary_prefix <- tempfile("popgenVCF-plink-", tmpdir = ancestry_dir)
  temporary_paths <- plink_bundle_paths(temporary_prefix)
  on.exit(unlink(c(temporary_paths, paste0(temporary_prefix, ".log")), force = TRUE), add = TRUE)

  converter(
    gds,
    bed.fn = temporary_prefix,
    sample.id = selected_sample_ids,
    snp.id = selected_snp_ids,
    verbose = FALSE
  )
  generated <- inspect_plink_bundle(temporary_prefix, sample_keys, snp_keys)
  if (!isTRUE(generated$valid)) {
    stop("Canonical PLINK export failed: ", generated$reason, call. = FALSE)
  }

  unlink(c(paths, manifest_file, sample_file), force = TRUE)
  moved <- file.rename(unname(temporary_paths), unname(paths))
  if (!all(moved)) {
    unlink(paths[moved], force = TRUE)
    stop("Unable to finalize the canonical PLINK bundle", call. = FALSE)
  }
  write_structure_sample_order(generated$sample_ids, sample_file)
  saveRDS(
    list(
      signature = signature,
      n_samples = generated$n_samples,
      n_snps = generated$n_snps,
      sample_ids = generated$sample_ids
    ),
    manifest_file,
    version = 3
  )

  log_msg(
    "Generated canonical PLINK bundle: ", prefix,
    " (", generated$n_samples, " samples; ", generated$n_snps, " SNPs)",
    level = "SUCCESS"
  )
  list(
    prefix = prefix,
    sample_file = sample_file,
    source = "generated",
    n_samples = generated$n_samples,
    n_snps = generated$n_snps
  )
}

inspect_snmf_geno <- function(geno_file, n_samples = NULL, n_snps = NULL) {
  if (!file.exists(geno_file)) {
    return(list(valid = FALSE, reason = "missing .geno file"))
  }
  size <- file.info(geno_file)$size
  if (is.na(size) || size <= 0) {
    return(list(valid = FALSE, reason = "the .geno file is empty"))
  }

  connection <- file(geno_file, open = "rt")
  on.exit(close(connection), add = TRUE)
  observed_snps <- 0L
  repeat {
    lines <- readLines(connection, n = 10000L, warn = FALSE)
    if (!length(lines)) break
    observed_snps <- observed_snps + length(lines)
    if (!is.null(n_samples) &&
        any(nchar(lines, type = "bytes") != as.integer(n_samples))) {
      return(list(valid = FALSE, reason = "the .geno sample count does not match retained samples"))
    }
    if (any(grepl("[^0129]", lines))) {
      return(list(valid = FALSE, reason = "the .geno file contains invalid genotype codes"))
    }
  }
  if (!observed_snps) {
    return(list(valid = FALSE, reason = "the .geno file has no SNP rows"))
  }
  if (!is.null(n_snps) && observed_snps != as.integer(n_snps)) {
    return(list(valid = FALSE, reason = "the .geno SNP count does not match retained SNPs"))
  }
  list(valid = TRUE, reason = NULL, n_samples = as.integer(n_samples),
       n_snps = observed_snps)
}

inspect_snmf_input <- function(geno_file, sample_file, sample_ids, snp_ids) {
  paths <- c(geno = geno_file, samples = sample_file)
  missing <- names(paths)[!nzchar(paths) | !file.exists(paths)]
  if (length(missing)) {
    labels <- ifelse(missing == "geno", ".geno file", "sample-order file")
    return(list(valid = FALSE,
                reason = paste("missing", paste(labels, collapse = " and "))))
  }

  ids <- tryCatch(
    readLines(sample_file, warn = FALSE),
    error = function(e) e
  )
  if (inherits(ids, "error")) {
    return(list(valid = FALSE, reason = "the sample-order file is unreadable"))
  }
  if (!identical(ids, as.character(sample_ids))) {
    return(list(valid = FALSE,
                reason = "the sample order does not match retained samples"))
  }

  geno <- inspect_snmf_geno(geno_file, length(sample_ids), length(snp_ids))
  if (!isTRUE(geno$valid)) return(geno)
  list(
    valid = TRUE,
    reason = NULL,
    geno_file = geno_file,
    sample_file = sample_file,
    sample_ids = ids,
    n_samples = length(ids),
    n_snps = geno$n_snps
  )
}

write_snmf_geno <- function(genotypes, path, chunk_size = 10000L) {
  genotypes <- as.matrix(genotypes)
  if (length(dim(genotypes)) != 2L) {
    stop("sNMF genotype input must be a matrix", call. = FALSE)
  }
  genotypes[is.na(genotypes)] <- 9L
  if (any(!genotypes %in% c(0L, 1L, 2L, 9L))) {
    stop(
      "sNMF genotypes must contain only reference-allele counts 0, 1, 2, or NA",
      call. = FALSE
    )
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  chunk_size <- max(1L, as.integer(chunk_size))
  starts <- seq.int(1L, ncol(genotypes), by = chunk_size)
  for (start in starts) {
    indices <- seq.int(start, min(ncol(genotypes), start + chunk_size - 1L))
    block <- genotypes[, indices, drop = FALSE]
    lines <- apply(block, 2L, paste0, collapse = "")
    writeLines(lines, connection, sep = "\n", useBytes = TRUE)
  }
  invisible(path)
}

prepare_snmf_input <- function(gds, sample_ids, snp_ids,
                               preferred_geno_file = NULL,
                               preferred_sample_file = NULL,
                               cache_dir,
                               extractor = SNPRelate::snpgdsGetGeno) {
  sample_keys <- as.character(sample_ids)
  snp_keys <- as.character(snp_ids)
  if (length(sample_keys) < 2L) stop("sNMF export requires at least two retained samples", call. = FALSE)
  if (!length(snp_keys)) stop("sNMF export requires retained SNPs", call. = FALSE)

  preferred_geno_file <- as.character(preferred_geno_file %||% "")[1L]
  preferred_sample_file <- as.character(preferred_sample_file %||% "")[1L]
  if (is.na(preferred_geno_file)) preferred_geno_file <- ""
  if (is.na(preferred_sample_file)) preferred_sample_file <- ""
  if (nzchar(preferred_geno_file)) {
    preferred_geno_file <- path.expand(preferred_geno_file)
  }
  if (nzchar(preferred_sample_file)) preferred_sample_file <- path.expand(preferred_sample_file)
  if (nzchar(preferred_geno_file) || nzchar(preferred_sample_file)) {
    preferred <- inspect_snmf_input(
      preferred_geno_file, preferred_sample_file, sample_keys, snp_keys
    )
    if (isTRUE(preferred$valid)) {
      log_msg(
        "Using configured sNMF input: ", preferred_geno_file,
        " (", preferred$n_samples, " samples; ", preferred$n_snps, " SNPs)",
        level = "INFO"
      )
      return(list(
        geno_file = preferred_geno_file,
        sample_file = preferred_sample_file,
        source = "configured",
        n_samples = preferred$n_samples,
        n_snps = preferred$n_snps
      ))
    }
    log_msg(
      "Configured sNMF input is unavailable or incompatible (", preferred$reason,
      "); generating a canonical .geno file from the retained GDS data",
      level = "WARNING"
    )
  }

  ancestry_dir <- file.path(cache_dir, "ancestry")
  dir.create(ancestry_dir, recursive = TRUE, showWarnings = FALSE)
  geno_file <- file.path(ancestry_dir, "popgenVCF_snmf.geno")
  sample_file <- file.path(ancestry_dir, "popgenVCF_snmf.samples.txt")
  manifest_file <- file.path(ancestry_dir, "popgenVCF_snmf.manifest.rds")
  signature <- digest::digest(
    list(sample_ids = sample_keys, snp_ids = snp_keys),
    algo = "sha256",
    serialize = TRUE
  )

  manifest <- if (file.exists(manifest_file)) {
    tryCatch(readRDS(manifest_file), error = function(e) NULL)
  } else NULL
  cached <- inspect_snmf_input(geno_file, sample_file, sample_keys, snp_keys)
  if (isTRUE(cached$valid) && identical(manifest$signature, signature)) {
    log_msg(
      "Reusing canonical sNMF input: ", geno_file,
      " (", cached$n_samples, " samples; ", cached$n_snps, " SNPs)",
      level = "INFO"
    )
    return(list(
      geno_file = geno_file,
      sample_file = sample_file,
      source = "cache",
      n_samples = cached$n_samples,
      n_snps = cached$n_snps
    ))
  }

  extracted <- extractor(
    gds,
    sample.id = sample_ids,
    snp.id = snp_ids,
    snpfirstdim = FALSE,
    with.id = TRUE,
    verbose = FALSE
  )
  if (!is.list(extracted) || is.null(extracted$genotype)) {
    stop("SNPRelate did not return an identifiable genotype matrix", call. = FALSE)
  }
  if (!identical(as.character(extracted$sample.id), sample_keys)) {
    stop("sNMF export sample order does not match retained samples", call. = FALSE)
  }
  if (!identical(as.character(extracted$snp.id), snp_keys)) {
    stop("sNMF export SNP order does not match retained SNPs", call. = FALSE)
  }
  genotypes <- extracted$genotype
  expected <- c(length(sample_keys), length(snp_keys))
  if (!identical(dim(genotypes), expected)) {
    stop("sNMF export genotype dimensions do not match retained data", call. = FALSE)
  }

  temporary <- tempfile("popgenVCF-snmf-", tmpdir = ancestry_dir, fileext = ".geno")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  write_snmf_geno(genotypes, temporary)
  generated <- inspect_snmf_geno(temporary, length(sample_keys), length(snp_keys))
  if (!isTRUE(generated$valid)) {
    stop("Canonical sNMF export failed: ", generated$reason, call. = FALSE)
  }

  unlink(c(geno_file, sample_file, manifest_file), force = TRUE)
  if (!file.rename(temporary, geno_file)) stop("Unable to finalize the canonical sNMF input", call. = FALSE)
  write_structure_sample_order(sample_keys, sample_file)
  saveRDS(
    list(
      signature = signature,
      n_samples = length(sample_keys),
      n_snps = length(snp_keys),
      sample_ids = sample_keys,
      snp_ids = snp_keys
    ),
    manifest_file,
    version = 3
  )

  log_msg(
    "Generated canonical sNMF input: ", geno_file,
    " (", length(sample_keys), " samples; ", length(snp_keys), " SNPs)",
    level = "SUCCESS"
  )
  list(
    geno_file = geno_file,
    sample_file = sample_file,
    source = "generated",
    n_samples = length(sample_keys),
    n_snps = length(snp_keys)
  )
}

# Late-loaded module definitions integrate ancestry backends with the exact
# retained sample and LD-pruned SNP set rather than requiring separately
# prepared, potentially mismatched external inputs.
run_module_admixture <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; ac <- cfg$analyses$admixture
  plink <- prepare_structure_plink_input(
    context$gds, context$sample_ids, context$final_snps,
    preferred_prefix = ac$plink_prefix,
    cache_dir = dirs$cache
  )
  context$structure_plink <- plink
  cv <- run_admixture_cv(
    ac$executable, plink$prefix, parse_int_range(ac$k),
    ac$threads, ac$cv_folds, dirs$admixture, cfg$compute$seed
  )
  analysis <- set_analysis_result(analysis, "admixture_cv", cv)
  analysis <- record_analysis_message(
    analysis, "INFO", "admixture",
    paste("PLINK input", plink$source, "with", plink$n_samples, "samples and", plink$n_snps, "SNPs")
  )
  write_tsv(cv, file.path(dirs$tables, "27_ADMIXTURE_CV.tsv"))
  plot_admixture_cv(cv, cfg, dirs)
  for (k in cv$K) {
    qpath <- file.path(dirs$admixture, sprintf("%s.%d.Q", basename(plink$prefix), k))
    if (file.exists(qpath)) {
      q <- read_admixture_q(qpath, plink$sample_file, context$metadata)
      write_tsv(q, file.path(dirs$tables, sprintf("28_ADMIXTURE_Q_K%d.tsv", k)))
      plot_q_matrix(q, k, cfg, dirs)
    }
  }
  module_result(analysis, context)
}

run_module_faststructure <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; fc <- cfg$analyses$faststructure
  plink <- context$structure_plink
  if (is.null(plink)) {
    plink <- prepare_structure_plink_input(
      context$gds, context$sample_ids, context$final_snps,
      preferred_prefix = fc$plink_prefix,
      cache_dir = dirs$cache
    )
  }
  context$structure_plink <- plink
  result <- run_faststructure(
    fc$structure_executable, fc$choosek_executable,
    plink$prefix, parse_int_range(fc$k), dirs$structure, cfg$compute$seed
  )
  ids <- data.table::fread(plink$sample_file, header = FALSE)[[1L]] |> as.character()
  for (k in names(result$q)) {
    q <- result$q[[k]]
    if (nrow(q) != length(ids)) stop("fastStructure Q rows do not match PLINK sample order", call. = FALSE)
    qdt <- data.table::as.data.table(q); qdt[, sample := ids]
    qdt[, population := context$metadata$population[match(sample, context$metadata$sample)]]
    if (anyNA(qdt$population)) stop("Some fastStructure samples are absent from retained metadata", call. = FALSE)
    data.table::setcolorder(qdt, c("sample", "population", grep("^cluster_", names(qdt), value = TRUE)))
    result$q[[k]] <- qdt
    write_tsv(qdt, file.path(dirs$tables, sprintf("29_fastStructure_Q_K%s.tsv", k)))
    plot_q_matrix(qdt, as.integer(k), cfg, dirs, prefix = "fastStructure_Q")
  }
  write_tsv(result$runs, file.path(dirs$tables, "29_fastStructure_runs.tsv"))
  analysis <- set_analysis_result(analysis, "faststructure", result)
  analysis <- record_analysis_message(
    analysis, "INFO", "faststructure",
    paste("PLINK input", plink$source, "with", plink$n_samples, "samples and", plink$n_snps, "SNPs")
  )
  module_result(analysis, context)
}
