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
