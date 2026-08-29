#' Run the complete popgenVCF analysis pipeline
#'
#' @param config Configuration list or YAML configuration path.
#' @param registry Analysis module registry.
#' @param selected Optional module identifiers to execute.
#' @return The completed `PopgenVCFAnalysis` object.
#' @export
run_pipeline <- function(config, registry = default_analysis_registry(), selected = NULL) {
  cfg <- if (is.character(config)) read_config(config) else config
  cfg <- validate_config(cfg)
  dirs <- make_dirs(cfg$output$directory)
  .pg_env$log_file <- file.path(dirs$root, "pipeline.log")
  cat("", file = .pg_env$log_file)
  set.seed(cfg$compute$seed)
  analysis <- new_popgen_vcf_analysis(cfg, dirs)
  analysis$status <- "running"
  log_msg("popgenVCF v", popgenvcf_version())

  stage <- function(name, expr) {
    t0 <- proc.time()[["elapsed"]]
    ans <- run_stage(name, force(expr))
    analysis <<- record_analysis_timing(analysis, name, proc.time()[["elapsed"]] - t0)
    analysis <<- record_analysis_message(analysis, "SUCCESS", name, "completed")
    ans
  }

  metadata_supplied <- !is.null(cfg$input$metadata)
  metadata <- if (metadata_supplied) {
    stage("metadata import", read_metadata(
      cfg$input$metadata, cfg$input$metadata_header,
      sample_column = cfg$input$sample_column, population_column = cfg$input$population_column
    ))
  } else NULL

  prepared_vcf <- stage("VCF preparation", prepare_vcf_input(cfg$input$vcf, file.path(dirs$cache, "vcf")))
  analysis$inputs$vcf_source <- prepared_vcf$source
  analysis$inputs$vcf_path <- prepared_vcf$path
  analysis$inputs$vcf_index <- prepared_vcf$index
  analysis$inputs$vcf_normalized <- prepared_vcf$normalized
  gds_path <- file.path(dirs$cache, "genotypes.gds")
  gds <- stage("GDS preparation", prepare_gds(prepared_vcf$path, gds_path, cfg$compute$force_gds))
  analysis$inputs$gds_path <- gds_path
  on.exit(try(SNPRelate::snpgdsClose(gds), silent = TRUE), add = TRUE)
  ids <- get_gds_ids(gds)
  analysis$inputs$ids <- ids

  if (is.null(metadata)) metadata <- metadata_from_samples(ids$sample)
  # Computed before any SNP-level QC exists yet, so this is a plain
  # chromosome-name filter, not a QC-passing subset -- the same restriction
  # applied again, post-QC, to qc_snps/final_snps further below.
  early_autosomal_snps <- if (isTRUE(cfg$qc$autosome_only)) {
    ids$snp[!ids$chromosome %in% cfg$qc$non_autosomal_chromosome_names]
  } else NULL
  hs <- stage(
    "sample identity validation and QC",
    harmonize_samples(
      gds, ids, metadata, cfg$qc$max_sample_missing,
      metadata_supplied = metadata_supplied, snp_ids = early_autosomal_snps
    )
  )
  sample_ids <- hs$sample_ids
  metadata <- hs$metadata
  capabilities <- metadata_capabilities(metadata, metadata_supplied)
  analysis$inputs$metadata <- metadata
  analysis$inputs$metadata_supplied <- metadata_supplied
  analysis$inputs$capabilities <- capabilities
  analysis$samples$ids <- sample_ids
  analysis$samples$metadata <- metadata
  analysis$samples$qc <- hs$qc
  analysis$samples$metadata_match <- hs$metadata_match
  write_tsv(hs$qc, file.path(dirs$tables, "01_sample_QC.tsv"))
  write_tsv(hs$metadata_match, file.path(dirs$tables, "02_sample_metadata_match.tsv"))

  if (isTRUE(capabilities$population)) {
    participation <- metadata[, .(
      n_samples = .N, used_PCA = TRUE, used_diversity = TRUE,
      used_FST = .N >= 2, used_AMOVA = TRUE, used_DAPC = TRUE
    ), by = population]
    analysis$samples$participation <- participation
    write_tsv(participation, file.path(dirs$tables, "03_population_participation.tsv"))
    palette <- population_palette(
      metadata$population, figure_style_name(cfg)
    )
    write_tsv(
      data.table::data.table(population = names(palette), colour = unname(palette)),
      file.path(dirs$tables, "04_population_colors.tsv")
    )
  }

  vq <- stage(
    "variant QC audit",
    variant_qc(gds, sample_ids, ids, cfg$qc$maf, 0.2, cfg$analyses$sex_check_y_chromosome_names)
  )
  qc_snps_all <- vq[pass_combined == TRUE, snp_id]
  # Ploidy-sensitive modules (kinship, PCA, IBS, DAPC, ancestry backends, ROH,
  # FST, genome scans, diversity/HWE, AMOVA) assume uniform diploid genotypes
  # at every retained marker. Sex chromosomes violate that for the hemizygous
  # sex (e.g. human male chrX): pooling them into the same marker set as
  # autosomes measurably corrupts results -- confirmed empirically, a known
  # real duplicate pair's kinship collapsed from 0.45 to ~0 once chrX markers
  # were mixed in with autosomal ones for the estimate. Standard tools (PLINK,
  # KING, GCTA) exclude sex chromosomes from genome-wide kinship/PCA/GRM by
  # default for the same reason. sex_check is the deliberate exception: it
  # receives the unfiltered qc_snps_all and does its own chromosome-name-based
  # restriction to X internally.
  qc_snps <- if (isTRUE(cfg$qc$autosome_only)) {
    non_autosomal <- ids$chromosome[match(qc_snps_all, ids$snp)] %in% cfg$qc$non_autosomal_chromosome_names
    qc_snps_all[!non_autosomal]
  } else qc_snps_all
  analysis$variants$audit <- vq
  analysis$variants$qc_ids <- qc_snps
  analysis$variants$qc_ids_all <- qc_snps_all
  final_snps <- stage(
    "exact SNPRelate LD pruning",
    ld_prune_exact(
      gds, sample_ids, cfg$qc$maf, cfg$compute$threads, cfg$compute$seed, snp_ids = qc_snps,
      ld_r2 = cfg$qc$ld_r2, slide_max_bp = cfg$qc$ld_slide_max_bp,
      slide_max_n = cfg$qc$ld_slide_max_n, start_pos = cfg$qc$ld_start_pos
    )
  )
  analysis$variants$ld_ids <- final_snps
  validate_analysis(analysis, "ordination")
  qc <- qc_reports(vq, final_snps)
  analysis$variants$reports <- qc
  write_tsv(qc$variant, file.path(dirs$tables, "05_variant_QC.tsv"))
  write_tsv(qc$independent, file.path(dirs$tables, "06_QC_independent_counts.tsv"))
  write_tsv(qc$sequential, file.path(dirs$tables, "07_QC_sequential_counts.tsv"))
  write_tsv(data.table::data.table(snp_id = final_snps), file.path(dirs$tables, "08_LD_pruned_SNPs.tsv"))
  plot_qc_reports(qc, hs$qc, cfg, dirs)

  capability_table <- analysis_capability_table(registry, capabilities)
  write_tsv(capability_table, file.path(dirs$root, "analysis_capabilities.tsv"))
  selected_available <- resolve_pipeline_modules(registry, capabilities, cfg, selected)

  context <- list(
    cfg = cfg, dirs = dirs, gds = gds, gds_path = gds_path, ids = ids, sample_ids = sample_ids,
    metadata = metadata, hs = hs, vq = vq, qc_snps = qc_snps,
    qc_snps_all = qc_snps_all, final_snps = final_snps, capabilities = capabilities,
    vcf_path = prepared_vcf$path
  )

  if (length(selected_available)) {
    registry_start <- proc.time()[["elapsed"]]
    backend <- if (cfg$compute$threads > 1L && .Platform$OS.type != "windows") "multicore" else "sequential"
    engine <- new_execution_engine(workers = cfg$compute$threads, backend = backend)
    # Unconditional per-batch checkpointing: see execute_analysis_plan()'s
    # own documentation for why this, not cooperative cancellation, is the
    # resilience mechanism -- it survives any way the process might stop,
    # not only a cooperative request between modules. A real 44-hour
    # production run losing everything to a deliberate `docker stop` (see
    # NEWS.md) is exactly the scenario this exists for.
    checkpoint_path <- file.path(dirs$root, "execution_checkpoint.rds")
    executed <- execute_analysis_registry(
      analysis, context, registry, selected_available,
      engine = engine, checkpoint_path = checkpoint_path
    )
    analysis <- executed$analysis
    context <- executed$context
    analysis$artifacts <- executed$artifacts
    write_tsv(executed$plan$table, file.path(dirs$root, "analysis_execution_plan.tsv"))
    write_tsv(executed$execution, file.path(dirs$root, "analysis_execution_ledger.tsv"))
    artifact_table <- artifact_manifest_table(executed$artifacts)
    if (nrow(artifact_table)) {
      write_tsv(artifact_table, file.path(dirs$root, "analysis_artifacts.tsv"))
    }
    analysis <- record_analysis_timing(analysis, "analysis registry", proc.time()[["elapsed"]] - registry_start)
    analysis <- record_analysis_message(
      analysis, "SUCCESS", "analysis registry",
      paste("executed", length(executed$order), "module(s) in", executed$engine$waves, "wave(s)")
    )
    analysis <- set_analysis_result(analysis, "execution_order", executed$order)
  } else {
    analysis$artifacts <- new_artifact_manifest()
    analysis <- set_analysis_result(analysis, "execution_order", character())
    analysis <- set_analysis_result(
      analysis, "execution_engine",
      list(
        backend = "sequential", workers = 1L, waves = 0L, batches = list(),
        status_counts = list(pending = 0L, running = 0L, success = 0L, failed = 0L, blocked = 0L)
      )
    )
    empty_plan <- plan_analysis_execution(registry, cfg, character())
    empty_ledger <- new_execution_ledger(empty_plan, registry, list())
    analysis <- set_analysis_result(analysis, "execution_ledger", empty_ledger)
    write_tsv(empty_plan$table, file.path(dirs$root, "analysis_execution_plan.tsv"))
    write_tsv(empty_ledger, file.path(dirs$root, "analysis_execution_ledger.tsv"))
    analysis <- record_analysis_message(analysis, "INFO", "analysis registry", "no compatible analysis modules were enabled")
    log_msg("No compatible analysis modules enabled after QC", level = "INFO")
  }

  finalize_pipeline_analysis(analysis, registry, cfg, dirs)
}

# Shared by run_pipeline() and run_pipeline_resume(): everything that runs
# once the analysis registry has finished (successfully or not), regardless
# of whether execution started fresh or picked up from a checkpoint.
finalize_pipeline_analysis <- function(analysis, registry, cfg, dirs) {
  write_tsv(list_analyses(registry), file.path(dirs$root, "analysis_module_contracts.tsv"))
  validations <- analysis$results$validation %||% list()
  if (length(validations)) {
    validation_table <- data.table::rbindlist(lapply(names(validations), function(nm) {
      v <- validations[[nm]]
      data.table::data.table(
        module = nm, valid = isTRUE(v$valid),
        errors = paste(v$errors, collapse = "; "),
        warnings = paste(v$warnings, collapse = "; "),
        metrics = paste(sprintf("%s=%s", names(v$metrics), unlist(v$metrics)), collapse = "; ")
      )
    }), fill = TRUE)
    write_tsv(validation_table, file.path(dirs$root, "analysis_validation.tsv"))
  }

  analysis$status <- "complete"
  analysis$completed_at <- Sys.time()
  validate_analysis(analysis)
  results_rds <- file.path(dirs$root, "analysis_results.rds")
  saveRDS(analysis, results_rds, compress = "xz")
  write_manifest(cfg, dirs, analysis, analysis$timings)
  write_tsv(summary(analysis), file.path(dirs$root, "analysis_summary.tsv"))
  utils::capture.output(utils::sessionInfo(), file = file.path(dirs$root, "sessionInfo.txt"))
  if (isTRUE(cfg$report$enabled)) {
    t0 <- proc.time()[["elapsed"]]
    run_stage("manuscript report", render_report(results_rds, dirs$report, cfg$report$title, cfg$report$author))
    analysis <- record_analysis_timing(analysis, "manuscript report", proc.time()[["elapsed"]] - t0)
    analysis <- record_analysis_message(analysis, "SUCCESS", "manuscript report", "completed")
  }
  log_msg("Analysis complete: ", dirs$root, level = "SUCCESS")
  invisible(analysis)
}

#' Resume a popgenVCF analysis pipeline from its last execution checkpoint
#'
#' `run_pipeline()` writes an execution checkpoint after every completed
#' analysis-registry batch, unconditionally -- not only on a graceful,
#' cooperative stop. If a run is interrupted by any means (a crash, an
#' out-of-memory kill, `docker stop`, a host reboot), this resumes from the
#' last batch recorded in that checkpoint: already-completed modules are not
#' re-run, and every stage that precedes analysis-registry execution
#' (metadata import, VCF/GDS preparation, sample and variant QC, LD pruning)
#' is skipped entirely, since the checkpoint already carries their validated
#' results.
#'
#' @param output_directory The `output.directory` of the interrupted run
#'   (the same directory passed to, or configured for, the original
#'   [run_pipeline()] call).
#' @param registry Analysis module registry. Must match the registry the
#'   original run used -- [read_execution_checkpoint()] rejects a checkpoint
#'   whose recorded plan is incompatible with the current registry.
#' @return The completed `PopgenVCFAnalysis` object.
#' @export
run_pipeline_resume <- function(output_directory, registry = default_analysis_registry()) {
  checkpoint_path <- file.path(output_directory, "execution_checkpoint.rds")
  if (!file.exists(checkpoint_path)) {
    stop(
      "No execution checkpoint found at ", checkpoint_path,
      "; there is nothing to resume. Run without --resume to start a new analysis.",
      call. = FALSE
    )
  }
  checkpoint <- read_execution_checkpoint(checkpoint_path, registry = registry)
  cfg <- checkpoint$analysis$config
  dirs <- make_dirs(cfg$output$directory)
  .pg_env$log_file <- file.path(dirs$root, "pipeline.log")
  log_msg(
    "Resuming popgenVCF v", popgenvcf_version(), " from checkpoint: ",
    length(checkpoint$completed), "/", length(checkpoint$plan_order), " module(s) already complete"
  )

  # context$gds was stripped before the checkpoint was written (a gdsfmt
  # connection is a live external pointer that cannot survive
  # serialization -- see execution_checkpoint_safe_context()); every module
  # still to run needs a real, valid connection, so one is opened fresh from
  # the plain path string the checkpoint did carry. The checkpoint is then
  # rebuilt from scratch via new_execution_checkpoint() rather than having
  # its $context mutated in place: checkpoint_digest covers the whole
  # payload, so directly assigning into checkpoint$context here would leave
  # a digest computed for the old (gds-stripped) context, and
  # resume_analysis_execution()'s own re-validation would reject it as
  # tampered.
  context <- checkpoint$context
  context$gds <- SNPRelate::snpgdsOpen(
    context$gds_path, readonly = TRUE, allow.duplicate = TRUE, allow.fork = TRUE
  )
  on.exit(try(SNPRelate::snpgdsClose(context$gds), silent = TRUE), add = TRUE)
  full_plan <- plan_analysis_execution(registry, checkpoint$analysis$config, selected = checkpoint$plan_order)
  checkpoint <- new_execution_checkpoint(
    list(
      analysis = checkpoint$analysis, context = context, order = checkpoint$completed,
      plan = full_plan, artifacts = checkpoint$artifacts, execution = checkpoint$execution
    ),
    registry
  )

  backend <- if (cfg$compute$threads > 1L && .Platform$OS.type != "windows") "multicore" else "sequential"
  engine <- new_execution_engine(workers = cfg$compute$threads, backend = backend)
  executed <- resume_analysis_execution(checkpoint, registry, engine)

  analysis <- executed$analysis
  analysis$artifacts <- executed$artifacts
  write_tsv(executed$plan$table, file.path(dirs$root, "analysis_execution_plan.tsv"))
  write_tsv(executed$execution, file.path(dirs$root, "analysis_execution_ledger.tsv"))
  artifact_table <- artifact_manifest_table(executed$artifacts)
  if (nrow(artifact_table)) {
    write_tsv(artifact_table, file.path(dirs$root, "analysis_artifacts.tsv"))
  }
  analysis <- record_analysis_message(
    analysis, "SUCCESS", "analysis registry",
    paste0(
      "resumed from checkpoint: ", length(checkpoint$completed), " module(s) reused, ",
      length(setdiff(executed$order, checkpoint$completed)), " module(s) executed"
    )
  )
  analysis <- set_analysis_result(analysis, "execution_order", executed$order)

  finalize_pipeline_analysis(analysis, registry, cfg, dirs)
}
