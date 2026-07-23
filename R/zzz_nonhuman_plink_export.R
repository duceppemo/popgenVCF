gds_autosome_bounds_defined <- function(gds,
                                          option_reader = SNPRelate::snpgdsOption) {
  option <- tryCatch(option_reader(gds), error = function(e) NULL)
  if (is.null(option)) return(FALSE)
  scalar_known <- function(x) length(x) == 1L && !is.na(x[[1L]])
  scalar_known(option$autosome.start) && scalar_known(option$autosome.end)
}

portable_gds_to_bed <- function(gdsobj, bed.fn, sample.id, snp.id,
                                verbose = FALSE,
                                option_reader = SNPRelate::snpgdsOption,
                                direct_converter = SNPRelate::snpgdsGDS2BED,
                                ped_converter = SNPRelate::snpgdsGDS2PED,
                                plink_executable = "plink",
                                plink_locator = Sys.which,
                                command_runner = system2) {
  dir.create(dirname(bed.fn), recursive = TRUE, showWarnings = FALSE)
  fallback_reason <- NULL

  if (gds_autosome_bounds_defined(gdsobj, option_reader)) {
    direct_error <- tryCatch(
      {
        direct_converter(
          gdsobj,
          bed.fn = bed.fn,
          sample.id = sample.id,
          snp.id = snp.id,
          verbose = verbose
        )
        NULL
      },
      error = function(e) e
    )
    if (is.null(direct_error)) return(invisible(NULL))

    direct_message <- conditionMessage(direct_error)
    is_chromosome_option_failure <- grepl(
      "missing value where TRUE/FALSE needed|argument is of length zero",
      direct_message
    )
    if (!is_chromosome_option_failure) stop(direct_error)
    fallback_reason <- paste("direct SNPRelate BED export failed:", direct_message)
  } else {
    fallback_reason <- "SNPRelate chromosome metadata has undefined autosome bounds"
  }

  log_msg(
    fallback_reason,
    "; using the portable PED-to-BED conversion path",
    level = "WARNING"
  )

  executable <- unname(as.character(plink_locator(plink_executable))[1L])
  if (is.na(executable) || !nzchar(executable)) {
    stop(
      "PLINK 1.9 executable not found; it is required to export non-human chromosome data",
      call. = FALSE
    )
  }

  destination_paths <- plink_bundle_paths(bed.fn)
  unlink(destination_paths, force = TRUE)
  ped_prefix <- tempfile("popgenVCF-ped-", tmpdir = dirname(bed.fn))
  ped_files <- paste0(ped_prefix, c(".ped", ".map"))
  auxiliary_files <- paste0(bed.fn, c(".log", ".nosex"))
  on.exit(unlink(c(ped_files, auxiliary_files), force = TRUE), add = TRUE)

  call_supported(
    ped_converter,
    list(
      gdsobj,
      ped.fn = ped_prefix,
      sample.id = sample.id,
      snp.id = snp.id,
      use.snp.rsid = FALSE,
      format = "A/G/C/T",
      verbose = verbose
    ),
    function_name = "SNPRelate::snpgdsGDS2PED"
  )
  if (!all(file.exists(ped_files))) {
    stop("SNPRelate PED fallback did not create both .ped and .map files", call. = FALSE)
  }

  arguments <- c(
    "--file", normalizePath(ped_prefix, mustWork = FALSE),
    "--make-bed",
    "--allow-extra-chr",
    "--keep-allele-order",
    "--out", normalizePath(bed.fn, mustWork = FALSE)
  )
  output <- command_runner(
    executable,
    arguments,
    stdout = TRUE,
    stderr = TRUE
  )
  writeLines(as.character(output), paste0(bed.fn, ".plink.log"), useBytes = TRUE)

  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  status <- suppressWarnings(as.integer(status)[1L])
  if (is.na(status) || status != 0L) {
    stop(
      sprintf(
        "PLINK PED-to-BED conversion failed with exit status %s; see %s",
        if (is.na(status)) "unknown" else status,
        paste0(bed.fn, ".plink.log")
      ),
      call. = FALSE
    )
  }

  generated <- inspect_plink_bundle(bed.fn, sample.id, snp.id)
  if (!isTRUE(generated$valid)) {
    stop("PLINK PED-to-BED conversion produced an invalid bundle: ",
         generated$reason, call. = FALSE)
  }
  invisible(NULL)
}

# These late-loaded definitions use a chromosome-agnostic converter while
# retaining the canonical sample/SNP selection and cache behavior.
run_module_admixture <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs; ac <- cfg$analyses$admixture
  plink <- prepare_structure_plink_input(
    context$gds, context$sample_ids, context$final_snps,
    preferred_prefix = ac$plink_prefix,
    cache_dir = dirs$cache,
    converter = portable_gds_to_bed
  )
  context$structure_plink <- plink
  cv <- run_admixture_cv(
    ac$executable, plink$prefix, parse_int_range(ac$k),
    ac$threads, ac$cv_folds, dirs$admixture, cfg$compute$seed
  )
  analysis <- set_analysis_result(analysis, "admixture_cv", cv)
  analysis <- record_analysis_message(
    analysis, "INFO", "admixture",
    paste("PLINK input", plink$source, "with", plink$n_samples,
          "samples and", plink$n_snps, "SNPs")
  )
  write_tsv(cv, file.path(dirs$tables, "27_ADMIXTURE_CV.tsv"))
  plot_admixture_cv(cv, cfg, dirs)
  for (k in cv$K) {
    qpath <- file.path(
      dirs$admixture,
      sprintf("%s.%d.Q", basename(plink$prefix), k)
    )
    if (isTRUE(file.exists(qpath))) {
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
      cache_dir = dirs$cache,
      converter = portable_gds_to_bed
    )
  }
  context$structure_plink <- plink
  result <- run_faststructure(
    fc$structure_executable, fc$choosek_executable,
    plink$prefix, parse_int_range(fc$k), dirs$structure, cfg$compute$seed
  )
  ids <- data.table::fread(plink$sample_file, header = FALSE)[[1L]] |>
    as.character()
  for (k in names(result$q)) {
    q <- result$q[[k]]
    if (nrow(q) != length(ids)) {
      stop("fastStructure Q rows do not match PLINK sample order", call. = FALSE)
    }
    qdt <- data.table::as.data.table(q)
    qdt[, sample := ids]
    qdt[, population := context$metadata$population[
      match(sample, context$metadata$sample)
    ]]
    if (anyNA(qdt$population)) {
      stop("Some fastStructure samples are absent from retained metadata", call. = FALSE)
    }
    data.table::setcolorder(
      qdt,
      c("sample", "population", grep("^cluster_", names(qdt), value = TRUE))
    )
    result$q[[k]] <- qdt
    write_tsv(qdt, file.path(dirs$tables, sprintf("29_fastStructure_Q_K%s.tsv", k)))
    plot_q_matrix(qdt, as.integer(k), cfg, dirs, prefix = "fastStructure_Q")
  }
  write_tsv(result$runs, file.path(dirs$tables, "29_fastStructure_runs.tsv"))
  analysis <- set_analysis_result(analysis, "faststructure", result)
  analysis <- record_analysis_message(
    analysis, "INFO", "faststructure",
    paste("PLINK input", plink$source, "with", plink$n_samples,
          "samples and", plink$n_snps, "SNPs")
  )
  module_result(analysis, context)
}
