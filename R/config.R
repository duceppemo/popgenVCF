#' Create the default popgenVCF configuration
#'
#' @return A nested configuration list using the supported schema.
#' @export
default_config <- function() {
  resources <- detect_system_resources()
  list(
    schema_version = "1.0",
    input = list(vcf = NULL, metadata = NULL, metadata_header = "auto",
                 sample_column = NULL, population_column = NULL,
                 geographic_columns = c("latitude", "longitude")),
    output = list(directory = NULL, figure_formats = c("pdf", "png"), dpi = 600L,
                  figure_style = "accessibility-first", base_font_size = 11,
                  label_samples = "auto"),
    compute = list(threads = resources$threads, memory_mb = resources$memory_mb,
                   seed = 42L, force_gds = FALSE),
    qc = list(maf = 0.05, max_variant_missing = 0.20, max_sample_missing = 0.20,
              ld_r2 = 0.20, ld_slide_max_bp = Inf, ld_slide_max_n = 50L,
              ld_start_pos = "first",
              autosome_only = TRUE,
              non_autosomal_chromosome_names = c("X", "Y", "MT", "M", "chrX", "chrY", "chrM", "chrMT")),
    analyses = list(diversity = TRUE, hwe_alpha = 0.05, diversity_allelic_richness = TRUE,
                    bottleneck = TRUE, bottleneck_n_bins = 10L,
                    pca = TRUE, n_pcs = 10L, pca_loading_top_n = 20L,
                    pca_metadata_color = TRUE, pca_metadata_color_min_group = 3L,
                    pca_metadata_color_max_levels = 12L,
                    ibs = TRUE, kinship = TRUE, kinship_close_relative_threshold = 0.0442,
                    sex_check = TRUE, sex_check_x_chromosome_names = c("X", "chrX"),
                    sex_check_male_f_threshold = 0.8, sex_check_female_f_threshold = 0.2,
                    sex_check_y_chromosome_names = c("Y", "chrY"),
                    sex_check_y_male_call_rate_threshold = 0.5,
                    sex_check_y_female_call_rate_threshold = 0.1,
                    roh = TRUE, roh_gt_error_phred = 30,
                    roh_length_class_short_max_bp = 500000L,
                    roh_length_class_long_min_bp = 2000000L,
                    tree = TRUE, population_tree = TRUE,
                    tree_bootstrap = list(enabled = TRUE, replicates = 100L),
                    ml_tree = list(enabled = FALSE, bootstrap_replicates = 100L),
                    population_assignment = TRUE, fst = TRUE,
                    genome_scan = TRUE, genome_scan_window_bp = 50000L,
                    genome_scan_step_bp = 50000L, genome_scan_min_snps = 5L,
                    pcadapt = TRUE, pcadapt_k = NULL, pcadapt_min_maf = 0.05,
                    pcadapt_fdr_alpha = 0.05,
                    ld_decay = TRUE, ld_decay_max_distance_bp = 500000L,
                    ld_decay_bin_bp = 5000L, ld_decay_slide = 100L,
                    ne_ld = TRUE, ne_ld_max_snps = 2000L,
                    dapc = TRUE, dapc_k = "2:10", dapc_cross_validation = TRUE,
                    dapc_loading_top_n = 20L,
                    amova = TRUE, clonality = TRUE,
                    clonality_genotype_curve_replicates = 100L, clonality_ia_permutations = 0L,
                    sexbias = TRUE, sexbias_test = "mAIc", sexbias_permutations = 0L,
                    mantel = TRUE, isolation_by_distance = TRUE,
                    spatial_autocorrelation = TRUE, spatial_autocorrelation_bins = 10L,
                    spatial_autocorrelation_permutations = 999L,
                    chromosome_specific = TRUE, chromosome_min_snps = 100L,
                    bootstrap = list(enabled = TRUE, replicates = 500L, unit = "chromosome"),
                    structure = list(replicates = 3L, seeds = NULL, reproducibility_rmse = 0.05,
                                     minimum_cluster_correlation = 0.90),
                    admixture = list(enabled = FALSE, executable = "admixture", plink_prefix = NULL,
                                     k = "2:10", threads = "auto", cv_folds = 5L,
                                     q_sample_file = NULL, timeout_seconds = 14400),
                    faststructure = list(enabled = FALSE, structure_executable = "structure.py",
                                         choosek_executable = "chooseK.py", plink_prefix = NULL,
                                         k = "2:10", q_sample_file = NULL, timeout_seconds = 14400),
                    snmf = list(enabled = FALSE, geno_file = NULL, q_sample_file = NULL,
                                k = "2:10", repetitions = 5L, entropy = TRUE,
                                threads = "auto")),
    report = list(enabled = TRUE, title = "Population genomics analysis", author = "")
  )
}

template_config <- function() {
  # Population/geography-gated analyses (everything analysis_capability_table()
  # classifies as a population_module or coordinate_module) used to be forced
  # off here specifically for the generated --write-config template, even
  # though default_config() already enables them. That was redundant: the
  # capability gate already skips each one automatically, with a WARNING and
  # an analysis_capabilities.tsv record, whenever the required metadata is
  # actually absent -- so leaving them "enabled: true" in the template is
  # never unsafe, and lets a user who *does* have complete metadata get the
  # full analysis without having to find and flip each flag on individually.
  # ml_tree and the ancestry backends (admixture/faststructure/snmf) are
  # unrelated to this and stay off by default in default_config() itself:
  # they are not metadata-gated (ancestry backends are unsupervised and run
  # fine with no population metadata at all), they need an external
  # tool/executable that may not be installed, and/or are deliberately
  # opt-in for cost or hard-failure-on-purpose reasons.
  default_config()
}

merge_lists <- function(x, y) {
  for (nm in names(y)) {
    if (is.list(y[[nm]]) && is.list(x[[nm]])) x[[nm]] <- merge_lists(x[[nm]], y[[nm]]) else x[[nm]] <- y[[nm]]
  }
  x
}

#' Read and merge a popgenVCF configuration
#'
#' @param path YAML configuration file.
#' @return The user configuration merged with current defaults.
#' @export
read_config <- function(path) {
  if (!file.exists(path)) stopf("Configuration file not found: %s", path)
  merge_lists(default_config(), yaml::read_yaml(path))
}

validate_config <- function(cfg) {
  if (is.null(cfg$schema_version)) cfg$schema_version <- "1.0"
  if (!identical(as.character(cfg$schema_version), "1.0")) stopf("Unsupported configuration schema_version: %s", as.character(cfg$schema_version))
  if (is.null(cfg$input[["vcf"]]) || is.null(cfg$output[["directory"]])) stop("VCF and output directory are required", call. = FALSE)
  if (!file.exists(cfg$input[["vcf"]])) stopf("VCF not found: %s", cfg$input[["vcf"]])

  metadata_path <- cfg$input[["metadata"]]
  if (!is.null(metadata_path)) {
    metadata_path <- as.character(metadata_path)[1L]
    if (!nzchar(metadata_path)) {
      metadata_path <- NULL
    } else if (!file.exists(metadata_path)) {
      stopf("Metadata not found: %s", metadata_path)
    }
  }
  # Assign through single-bracket replacement so a NULL value remains an
  # explicitly named list element. Using [[<- NULL removes the element and
  # makes `$metadata` partially match `metadata_header`.
  cfg$input["metadata"] <- list(metadata_path)

  vals <- c(cfg$qc$maf, cfg$qc$max_variant_missing, cfg$qc$max_sample_missing, cfg$qc$ld_r2)
  if (any(!is.finite(vals)) || any(vals < 0) || any(vals > 1)) stop("QC proportions must be between zero and one", call. = FALSE)
  if (cfg$qc$maf > 0.5) stop("MAF cannot exceed 0.5", call. = FALSE)
  # max_variant_missing stays a fixed part of the scientific QC contract: unlike
  # the LD-pruning parameters below (which ld_prune_exact() alone consumes),
  # it is also read directly by variant_qc() for ROH's own missingness gate
  # (R/module_registry.R), so letting it vary would change more than just the
  # marker panel LD-pruning selects from.
  if (!isTRUE(all.equal(cfg$qc$max_variant_missing, 0.2))) {
    warning("The fixed QC contract requires max_variant_missing = 0.2; overriding the configured value.", call. = FALSE)
  }
  cfg$qc$max_variant_missing <- 0.2
  # ld_r2/ld_slide_max_bp/ld_slide_max_n/ld_start_pos are real, user-configurable
  # LD-pruning parameters (threaded into ld_prune_exact()'s snpgdsLDpruning()
  # call) -- validated, not silently overridden, so a bad value fails loudly
  # instead of a configured choice being discarded without notice.
  if (!is.finite(cfg$qc$ld_slide_max_n) || cfg$qc$ld_slide_max_n < 1L) {
    stop("qc.ld_slide_max_n must be a positive integer", call. = FALSE)
  }
  cfg$qc$ld_slide_max_n <- as.integer(cfg$qc$ld_slide_max_n)
  if (!identical(cfg$qc$ld_slide_max_bp, Inf) && (!is.finite(cfg$qc$ld_slide_max_bp) || cfg$qc$ld_slide_max_bp <= 0)) {
    stop("qc.ld_slide_max_bp must be a positive number or Inf", call. = FALSE)
  }
  cfg$qc$ld_start_pos <- as.character(cfg$qc$ld_start_pos)[1L]
  if (!cfg$qc$ld_start_pos %in% c("first", "last", "random", "random.f500")) {
    stopf("qc.ld_start_pos must be one of \"first\", \"last\", \"random\", \"random.f500\" (SNPRelate::snpgdsLDpruning()'s own accepted values); got %s", cfg$qc$ld_start_pos)
  }
  cfg$qc$non_autosomal_chromosome_names <- as.character(cfg$qc$non_autosomal_chromosome_names)

  if (is.null(cfg$compute$memory_mb)) {
    cfg$compute$memory_mb <- detect_available_memory_mb()
  }
  cfg$compute$threads <- as.integer(cfg$compute$threads)
  cfg$compute$memory_mb <- as.numeric(cfg$compute$memory_mb)
  cfg$compute$seed <- as.integer(cfg$compute$seed)
  cfg$output$dpi <- as.integer(cfg$output$dpi)
  cfg$output$base_font_size <- as.numeric(cfg$output$base_font_size)
  cfg$analyses$hwe_alpha <- as.numeric(cfg$analyses$hwe_alpha)
  cfg$analyses$bottleneck_n_bins <- as.integer(cfg$analyses$bottleneck_n_bins)
  cfg$analyses$n_pcs <- as.integer(cfg$analyses$n_pcs)
  cfg$analyses$pca_loading_top_n <- as.integer(cfg$analyses$pca_loading_top_n)
  cfg$analyses$pca_metadata_color_min_group <- as.integer(cfg$analyses$pca_metadata_color_min_group)
  cfg$analyses$pca_metadata_color_max_levels <- as.integer(cfg$analyses$pca_metadata_color_max_levels)
  cfg$analyses$kinship_close_relative_threshold <- as.numeric(cfg$analyses$kinship_close_relative_threshold)
  cfg$analyses$sex_check_x_chromosome_names <- as.character(cfg$analyses$sex_check_x_chromosome_names)
  cfg$analyses$sex_check_male_f_threshold <- as.numeric(cfg$analyses$sex_check_male_f_threshold)
  cfg$analyses$sex_check_female_f_threshold <- as.numeric(cfg$analyses$sex_check_female_f_threshold)
  cfg$analyses$sex_check_y_chromosome_names <- as.character(cfg$analyses$sex_check_y_chromosome_names)
  cfg$analyses$sex_check_y_male_call_rate_threshold <- as.numeric(cfg$analyses$sex_check_y_male_call_rate_threshold)
  cfg$analyses$sex_check_y_female_call_rate_threshold <- as.numeric(cfg$analyses$sex_check_y_female_call_rate_threshold)
  cfg$analyses$roh_gt_error_phred <- as.numeric(cfg$analyses$roh_gt_error_phred)
  cfg$analyses$roh_length_class_short_max_bp <- as.integer(cfg$analyses$roh_length_class_short_max_bp)
  cfg$analyses$roh_length_class_long_min_bp <- as.integer(cfg$analyses$roh_length_class_long_min_bp)
  cfg$analyses$genome_scan_window_bp <- as.integer(cfg$analyses$genome_scan_window_bp)
  cfg$analyses$genome_scan_step_bp <- as.integer(cfg$analyses$genome_scan_step_bp)
  cfg$analyses$genome_scan_min_snps <- as.integer(cfg$analyses$genome_scan_min_snps)
  if (!is.null(cfg$analyses$pcadapt_k)) cfg$analyses$pcadapt_k <- as.integer(cfg$analyses$pcadapt_k)
  cfg$analyses$pcadapt_min_maf <- as.numeric(cfg$analyses$pcadapt_min_maf)
  cfg$analyses$pcadapt_fdr_alpha <- as.numeric(cfg$analyses$pcadapt_fdr_alpha)
  cfg$analyses$ld_decay_max_distance_bp <- as.integer(cfg$analyses$ld_decay_max_distance_bp)
  cfg$analyses$ld_decay_bin_bp <- as.integer(cfg$analyses$ld_decay_bin_bp)
  cfg$analyses$ld_decay_slide <- as.integer(cfg$analyses$ld_decay_slide)
  cfg$analyses$ne_ld_max_snps <- as.integer(cfg$analyses$ne_ld_max_snps)
  cfg$analyses$spatial_autocorrelation_bins <- as.integer(cfg$analyses$spatial_autocorrelation_bins)
  cfg$analyses$spatial_autocorrelation_permutations <- as.integer(cfg$analyses$spatial_autocorrelation_permutations)
  cfg$analyses$chromosome_min_snps <- as.integer(cfg$analyses$chromosome_min_snps)
  cfg$analyses$dapc_loading_top_n <- as.integer(cfg$analyses$dapc_loading_top_n)
  cfg$analyses$clonality_genotype_curve_replicates <- as.integer(cfg$analyses$clonality_genotype_curve_replicates)
  cfg$analyses$clonality_ia_permutations <- as.integer(cfg$analyses$clonality_ia_permutations)
  cfg$analyses$sexbias_test <- as.character(cfg$analyses$sexbias_test)
  cfg$analyses$sexbias_permutations <- as.integer(cfg$analyses$sexbias_permutations)
  cfg$analyses$bootstrap$replicates <- as.integer(cfg$analyses$bootstrap$replicates)
  cfg$analyses$tree_bootstrap$replicates <- as.integer(cfg$analyses$tree_bootstrap$replicates)
  cfg$analyses$ml_tree$bootstrap_replicates <- as.integer(cfg$analyses$ml_tree$bootstrap_replicates)
  cfg$analyses$structure$replicates <- as.integer(cfg$analyses$structure$replicates)
  cfg$analyses$snmf$repetitions <- as.integer(cfg$analyses$snmf$repetitions)
  if (!is.finite(cfg$compute$threads) || cfg$compute$threads < 1L) stop("compute.threads must be >= 1", call. = FALSE)
  if (is.na(cfg$compute$memory_mb) || cfg$compute$memory_mb <= 0) {
    stop("compute.memory_mb must be positive or .inf", call. = FALSE)
  }
  if (!is.finite(cfg$compute$seed)) stop("compute.seed must be an integer", call. = FALSE)
  if (!is.finite(cfg$output$dpi) || cfg$output$dpi < 72L) stop("output.dpi must be >= 72", call. = FALSE)
  if (!is.finite(cfg$output$base_font_size) || cfg$output$base_font_size < 8) {
    stop("output.base_font_size must be >= 8", call. = FALSE)
  }
  if (!is.finite(cfg$analyses$hwe_alpha) || cfg$analyses$hwe_alpha <= 0 || cfg$analyses$hwe_alpha >= 1) stop("analyses.hwe_alpha must be between zero and one", call. = FALSE)
  if (!is.finite(cfg$analyses$bottleneck_n_bins) || cfg$analyses$bottleneck_n_bins < 2L) stop("analyses.bottleneck_n_bins must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$n_pcs) || cfg$analyses$n_pcs < 2L) stop("analyses.n_pcs must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$pca_loading_top_n) || cfg$analyses$pca_loading_top_n < 1L) stop("analyses.pca_loading_top_n must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$pca_metadata_color_min_group) || cfg$analyses$pca_metadata_color_min_group < 1L) stop("analyses.pca_metadata_color_min_group must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$pca_metadata_color_max_levels) || cfg$analyses$pca_metadata_color_max_levels < 2L) stop("analyses.pca_metadata_color_max_levels must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$kinship_close_relative_threshold) || cfg$analyses$kinship_close_relative_threshold <= 0 || cfg$analyses$kinship_close_relative_threshold > 0.5) stop("analyses.kinship_close_relative_threshold must be between zero (exclusive) and 0.5 (inclusive)", call. = FALSE)
  if (!length(cfg$analyses$sex_check_x_chromosome_names) || anyNA(cfg$analyses$sex_check_x_chromosome_names) || any(!nzchar(cfg$analyses$sex_check_x_chromosome_names))) stop("analyses.sex_check_x_chromosome_names must be one or more non-empty chromosome name(s)", call. = FALSE)
  if (!is.finite(cfg$analyses$sex_check_male_f_threshold) || !is.finite(cfg$analyses$sex_check_female_f_threshold)) stop("analyses.sex_check_male_f_threshold and analyses.sex_check_female_f_threshold must be finite", call. = FALSE)
  if (cfg$analyses$sex_check_female_f_threshold >= cfg$analyses$sex_check_male_f_threshold) stop("analyses.sex_check_female_f_threshold must be less than analyses.sex_check_male_f_threshold", call. = FALSE)
  if (!length(cfg$analyses$sex_check_y_chromosome_names) || anyNA(cfg$analyses$sex_check_y_chromosome_names) || any(!nzchar(cfg$analyses$sex_check_y_chromosome_names))) stop("analyses.sex_check_y_chromosome_names must be one or more non-empty chromosome name(s)", call. = FALSE)
  if (!is.finite(cfg$analyses$sex_check_y_male_call_rate_threshold) || !is.finite(cfg$analyses$sex_check_y_female_call_rate_threshold)) stop("analyses.sex_check_y_male_call_rate_threshold and analyses.sex_check_y_female_call_rate_threshold must be finite", call. = FALSE)
  if (cfg$analyses$sex_check_y_female_call_rate_threshold >= cfg$analyses$sex_check_y_male_call_rate_threshold) stop("analyses.sex_check_y_female_call_rate_threshold must be less than analyses.sex_check_y_male_call_rate_threshold", call. = FALSE)
  if (cfg$analyses$sex_check_y_male_call_rate_threshold > 1 || cfg$analyses$sex_check_y_female_call_rate_threshold < 0) stop("analyses.sex_check_y_*_call_rate_threshold must be between zero and one", call. = FALSE)
  if (!is.finite(cfg$analyses$roh_gt_error_phred) || cfg$analyses$roh_gt_error_phred <= 0) stop("analyses.roh_gt_error_phred must be positive", call. = FALSE)
  if (!is.finite(cfg$analyses$roh_length_class_short_max_bp) || cfg$analyses$roh_length_class_short_max_bp < 1L) stop("analyses.roh_length_class_short_max_bp must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$roh_length_class_long_min_bp) || cfg$analyses$roh_length_class_long_min_bp <= cfg$analyses$roh_length_class_short_max_bp) stop("analyses.roh_length_class_long_min_bp must be greater than analyses.roh_length_class_short_max_bp", call. = FALSE)
  if (!is.finite(cfg$analyses$genome_scan_window_bp) || cfg$analyses$genome_scan_window_bp < 1L) stop("analyses.genome_scan_window_bp must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$genome_scan_step_bp) || cfg$analyses$genome_scan_step_bp < 1L) stop("analyses.genome_scan_step_bp must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$genome_scan_min_snps) || cfg$analyses$genome_scan_min_snps < 2L) stop("analyses.genome_scan_min_snps must be >= 2", call. = FALSE)
  if (!is.null(cfg$analyses$pcadapt_k) && (!is.finite(cfg$analyses$pcadapt_k) || cfg$analyses$pcadapt_k < 1L)) stop("analyses.pcadapt_k must be NULL or >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$pcadapt_min_maf) || cfg$analyses$pcadapt_min_maf < 0 || cfg$analyses$pcadapt_min_maf >= 0.5) stop("analyses.pcadapt_min_maf must be between zero (inclusive) and 0.5 (exclusive)", call. = FALSE)
  if (!is.finite(cfg$analyses$pcadapt_fdr_alpha) || cfg$analyses$pcadapt_fdr_alpha <= 0 || cfg$analyses$pcadapt_fdr_alpha >= 1) stop("analyses.pcadapt_fdr_alpha must be between zero and one", call. = FALSE)
  if (!is.finite(cfg$analyses$ld_decay_max_distance_bp) || cfg$analyses$ld_decay_max_distance_bp < 1L) stop("analyses.ld_decay_max_distance_bp must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$ld_decay_bin_bp) || cfg$analyses$ld_decay_bin_bp < 1L) stop("analyses.ld_decay_bin_bp must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$ld_decay_slide) || cfg$analyses$ld_decay_slide < 1L) stop("analyses.ld_decay_slide must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$ne_ld_max_snps) || cfg$analyses$ne_ld_max_snps < 2L) stop("analyses.ne_ld_max_snps must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$spatial_autocorrelation_bins) || cfg$analyses$spatial_autocorrelation_bins < 2L) stop("analyses.spatial_autocorrelation_bins must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$spatial_autocorrelation_permutations) || cfg$analyses$spatial_autocorrelation_permutations < 0L) stop("analyses.spatial_autocorrelation_permutations must be >= 0", call. = FALSE)
  if (!is.finite(cfg$analyses$chromosome_min_snps) || cfg$analyses$chromosome_min_snps < 2L) stop("analyses.chromosome_min_snps must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$dapc_loading_top_n) || cfg$analyses$dapc_loading_top_n < 1L) stop("analyses.dapc_loading_top_n must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$clonality_genotype_curve_replicates) || cfg$analyses$clonality_genotype_curve_replicates < 0L) stop("analyses.clonality_genotype_curve_replicates must be >= 0", call. = FALSE)
  if (!is.finite(cfg$analyses$clonality_ia_permutations) || cfg$analyses$clonality_ia_permutations < 0L) stop("analyses.clonality_ia_permutations must be >= 0", call. = FALSE)
  if (!cfg$analyses$sexbias_test %in% c("mAIc", "vAIc", "FIS", "FST")) stop("analyses.sexbias_test must be one of \"mAIc\", \"vAIc\", \"FIS\", or \"FST\"", call. = FALSE)
  if (!is.finite(cfg$analyses$sexbias_permutations) || cfg$analyses$sexbias_permutations < 0L) stop("analyses.sexbias_permutations must be >= 0", call. = FALSE)
  if (cfg$analyses$sexbias_test %in% c("FIS", "FST") && cfg$analyses$sexbias_permutations <= 0L) stop("analyses.sexbias_permutations must be > 0 when analyses.sexbias_test is \"FIS\" or \"FST\"", call. = FALSE)
  if (!is.finite(cfg$analyses$bootstrap$replicates) || cfg$analyses$bootstrap$replicates < 0L) stop("bootstrap.replicates must be >= 0", call. = FALSE)
  if (!is.finite(cfg$analyses$tree_bootstrap$replicates) || cfg$analyses$tree_bootstrap$replicates < 0L) stop("tree_bootstrap.replicates must be >= 0", call. = FALSE)
  if (!is.finite(cfg$analyses$ml_tree$bootstrap_replicates) || cfg$analyses$ml_tree$bootstrap_replicates < 0L) stop("ml_tree.bootstrap_replicates must be >= 0", call. = FALSE)

  allowed_formats <- c("pdf", "png", "svg")
  cfg$output$figure_formats <- unique(tolower(as.character(cfg$output$figure_formats)))
  invalid_formats <- setdiff(cfg$output$figure_formats, allowed_formats)
  if (length(invalid_formats)) stopf("Unsupported figure format(s): %s", paste(invalid_formats, collapse = ", "))
  cfg$output$figure_style <- figure_style_name(cfg)
  cfg$input$metadata_header <- tolower(as.character(cfg$input$metadata_header))
  if (!cfg$input$metadata_header %in% c("auto", "yes", "no", "true", "false")) stop("input.metadata_header must be auto, yes, or no", call. = FALSE)
  for (field in c("sample_column", "population_column")) {
    value <- cfg$input[[field]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || !nzchar(trimws(value)))) {
      stopf("input.%s must be NULL or a single non-empty column name", field)
    }
  }
  if (!is.finite(cfg$analyses$structure$replicates) || cfg$analyses$structure$replicates < 1L) stop("analyses.structure.replicates must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$structure$reproducibility_rmse) || cfg$analyses$structure$reproducibility_rmse < 0) stop("analyses.structure.reproducibility_rmse must be non-negative", call. = FALSE)
  if (!is.finite(cfg$analyses$structure$minimum_cluster_correlation) || cfg$analyses$structure$minimum_cluster_correlation < -1 || cfg$analyses$structure$minimum_cluster_correlation > 1) stop("analyses.structure.minimum_cluster_correlation must be between -1 and 1", call. = FALSE)

  resolve_threads <- function(value, field) {
    if (is.null(value) || (is.character(value) && length(value) == 1L &&
        identical(tolower(value), "auto"))) {
      return(cfg$compute$threads)
    }
    value <- suppressWarnings(as.integer(value)[1L])
    if (is.na(value) || value < 1L) stop(field, " must be 'auto' or >= 1", call. = FALSE)
    value
  }
  cfg$analyses$admixture$threads <- resolve_threads(
    cfg$analyses$admixture$threads, "analyses.admixture.threads"
  )
  cfg$analyses$snmf$threads <- resolve_threads(
    cfg$analyses$snmf$threads, "analyses.snmf.threads"
  )

  resolve_timeout_seconds <- function(value, field) {
    value <- suppressWarnings(as.numeric(value)[1L])
    if (is.na(value) || value <= 0) stop(field, " must be a positive number of seconds", call. = FALSE)
    value
  }
  cfg$analyses$admixture$timeout_seconds <- resolve_timeout_seconds(
    cfg$analyses$admixture$timeout_seconds, "analyses.admixture.timeout_seconds"
  )
  cfg$analyses$faststructure$timeout_seconds <- resolve_timeout_seconds(
    cfg$analyses$faststructure$timeout_seconds, "analyses.faststructure.timeout_seconds"
  )

  # ADMIXTURE and fastStructure PLINK inputs are prepared from the retained
  # samples and LD-pruned SNPs at runtime. A configured prefix is only a
  # preferred, compatibility-checked override, and sample order is derived
  # from the selected PLINK bundle. sNMF uses the same retained data to prepare
  # its .geno and sample-order files; configured files are optional overrides.
  cfg
}

make_dirs <- function(outdir) {
  root <- ensure_dir(outdir)
  d <- list(root = root, tables = file.path(root, "tables"), figures = file.path(root, "figures"),
            trees = file.path(root, "trees"), cache = file.path(root, "cache"), report = file.path(root, "report"),
            chromosomes = file.path(root, "chromosomes"), admixture = file.path(root, "admixture"),
            structure = file.path(root, "structure"))
  lapply(d[-1], ensure_dir)
  d
}
