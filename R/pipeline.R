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

  metadata <- stage("metadata import", read_metadata(cfg$input$metadata, cfg$input$metadata_header))
  analysis$inputs$metadata <- metadata
  prepared_vcf <- stage(
    "VCF preparation",
    prepare_vcf_input(cfg$input$vcf, file.path(dirs$cache, "vcf"))
  )
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

  hs <- stage("sample harmonization and QC", harmonize_samples(gds, ids, metadata, cfg$qc$max_sample_missing))
  sample_ids <- hs$sample_ids
  metadata <- hs$metadata
  analysis$samples$ids <- sample_ids
  analysis$samples$metadata <- metadata
  analysis$samples$qc <- hs$qc
  write_tsv(hs$qc, file.path(dirs$tables, "01_sample_QC.tsv"))
  participation <- metadata[, .(
    n_samples = .N, used_PCA = TRUE, used_diversity = TRUE,
    used_FST = .N >= 2, used_AMOVA = TRUE, used_DAPC = TRUE
  ), by = population]
  analysis$samples$participation <- participation
  write_tsv(participation, file.path(dirs$tables, "02_population_participation.tsv"))
  palette <- population_palette(metadata$population)
  write_tsv(data.table::data.table(population = names(palette), colour = unname(palette)),
            file.path(dirs$tables, "03_population_colors.tsv"))

  vq <- stage("variant QC audit", variant_qc(gds, sample_ids, ids, cfg$qc$maf, 0.2))
  qc_snps <- vq[pass_combined, snp_id]
  analysis$variants$audit <- vq
  analysis$variants$qc_ids <- qc_snps
  final_snps <- stage("exact SNPRelate LD pruning",
                      ld_prune_exact(gds, sample_ids, cfg$qc$maf,
                                     cfg$compute$threads, cfg$compute$seed))
  analysis$variants$ld_ids <- final_snps
  validate_analysis(analysis, "ordination")
  qc <- qc_reports(vq, final_snps)
  analysis$variants$reports <- qc
  write_tsv(qc$variant, file.path(dirs$tables, "04_variant_QC.tsv"))
  write_tsv(qc$independent, file.path(dirs$tables, "05_QC_independent_counts.tsv"))
  write_tsv(qc$sequential, file.path(dirs$tables, "06_QC_sequential_counts.tsv"))
  write_tsv(data.table::data.table(snp_id = final_snps), file.path(dirs$tables, "07_LD_pruned_SNPs.tsv"))
  plot_qc_reports(qc, hs$qc, cfg, dirs)

  context <- list(
    cfg = cfg, dirs = dirs, gds = gds, ids = ids, sample_ids = sample_ids,
    metadata = metadata, hs = hs, vq = vq, qc_snps = qc_snps,
    final_snps = final_snps
  )
  registry_start <- proc.time()[["elapsed"]]
  executed <- execute_analysis_registry(analysis, context, registry, selected)
  analysis <- executed$analysis
  context <- executed$context
  analysis <- record_analysis_timing(
    analysis, "analysis registry", proc.time()[["elapsed"]] - registry_start
  )
  analysis <- record_analysis_message(
    analysis, "SUCCESS", "analysis registry",
    paste("executed", length(executed$order), "module(s)")
  )
  analysis <- set_analysis_result(analysis, "execution_order", executed$order)
  module_table <- list_analyses(registry)
  write_tsv(module_table, file.path(dirs$root, "analysis_module_contracts.tsv"))
  validations <- analysis$results$validation %||% list()
  if (length(validations)) {
    validation_table <- data.table::rbindlist(lapply(names(validations), function(nm) {
      v <- validations[[nm]]
      data.table::data.table(
        module = nm,
        valid = isTRUE(v$valid),
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
    stage("manuscript report", render_report(
      results_rds, dirs$report, cfg$report$title, cfg$report$author
    ))
  }
  log_msg("Analysis complete: ", dirs$root, level = "SUCCESS")
  invisible(analysis)
}
