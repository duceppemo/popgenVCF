#' Create the default popgenVCF configuration
#'
#' @return A nested configuration list using the supported schema.
#' @export
default_config <- function() {
  resources <- detect_system_resources()
  list(
    schema_version = "1.0",
    input = list(vcf = NULL, metadata = NULL, metadata_header = "auto",
                 geographic_columns = c("latitude", "longitude")),
    output = list(directory = NULL, figure_formats = c("pdf", "png"), dpi = 600L,
                  figure_style = "accessibility-first", base_font_size = 11,
                  label_samples = "auto"),
    compute = list(threads = resources$threads, memory_mb = resources$memory_mb,
                   seed = 42L, force_gds = FALSE),
    qc = list(maf = 0.05, max_variant_missing = 0.20, max_sample_missing = 0.20,
              ld_r2 = 0.20, ld_slide_max_bp = Inf, ld_slide_max_n = 50L,
              ld_start_pos = "first"),
    analyses = list(diversity = TRUE, hwe_alpha = 0.05, pca = TRUE, n_pcs = 10L, pca_loading_top_n = 20L,
                    ibs = TRUE, tree = TRUE, fst = TRUE,
                    dapc = TRUE, dapc_k = "2:10", dapc_cross_validation = TRUE,
                    dapc_loading_top_n = 20L,
                    amova = TRUE, mantel = TRUE, isolation_by_distance = TRUE,
                    chromosome_specific = TRUE, chromosome_min_snps = 100L,
                    bootstrap = list(enabled = TRUE, replicates = 500L, unit = "chromosome"),
                    structure = list(replicates = 3L, seeds = NULL, reproducibility_rmse = 0.05,
                                     minimum_cluster_correlation = 0.90),
                    admixture = list(enabled = FALSE, executable = "admixture", plink_prefix = NULL,
                                     k = "2:10", threads = "auto", cv_folds = 5L,
                                     q_sample_file = NULL),
                    faststructure = list(enabled = FALSE, structure_executable = "structure.py",
                                         choosek_executable = "chooseK.py", plink_prefix = NULL,
                                         k = "2:10", q_sample_file = NULL),
                    snmf = list(enabled = FALSE, geno_file = NULL, q_sample_file = NULL,
                                k = "2:10", repetitions = 5L, entropy = TRUE,
                                threads = "auto")),
    report = list(enabled = TRUE, title = "Population genomics analysis", author = "")
  )
}

template_config <- function() {
  cfg <- default_config()
  # Analyses requiring population or geographic metadata are visible but off in
  # the generated template. Existing partial configurations retain the historic
  # defaults supplied by default_config().
  cfg$analyses$diversity <- FALSE
  cfg$analyses$fst <- FALSE
  cfg$analyses$dapc <- FALSE
  cfg$analyses$amova <- FALSE
  cfg$analyses$mantel <- FALSE
  cfg$analyses$isolation_by_distance <- FALSE
  cfg$analyses$chromosome_specific <- FALSE
  cfg$analyses$bootstrap$enabled <- FALSE
  cfg
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
  fixed_changed <- !isTRUE(all.equal(cfg$qc$ld_r2, 0.2)) || !isTRUE(all.equal(cfg$qc$max_variant_missing, 0.2))
  if (fixed_changed) warning("The fixed QC contract requires max_variant_missing = 0.2 and LD r^2 = 0.2; overriding configured values.", call. = FALSE)
  cfg$qc$ld_r2 <- 0.2; cfg$qc$max_variant_missing <- 0.2
  cfg$qc$ld_slide_max_bp <- Inf; cfg$qc$ld_slide_max_n <- 50L; cfg$qc$ld_start_pos <- "first"

  if (is.null(cfg$compute$memory_mb)) {
    cfg$compute$memory_mb <- detect_available_memory_mb()
  }
  cfg$compute$threads <- as.integer(cfg$compute$threads)
  cfg$compute$memory_mb <- as.numeric(cfg$compute$memory_mb)
  cfg$compute$seed <- as.integer(cfg$compute$seed)
  cfg$output$dpi <- as.integer(cfg$output$dpi)
  cfg$output$base_font_size <- as.numeric(cfg$output$base_font_size)
  cfg$analyses$hwe_alpha <- as.numeric(cfg$analyses$hwe_alpha)
  cfg$analyses$n_pcs <- as.integer(cfg$analyses$n_pcs)
  cfg$analyses$pca_loading_top_n <- as.integer(cfg$analyses$pca_loading_top_n)
  cfg$analyses$chromosome_min_snps <- as.integer(cfg$analyses$chromosome_min_snps)
  cfg$analyses$dapc_loading_top_n <- as.integer(cfg$analyses$dapc_loading_top_n)
  cfg$analyses$bootstrap$replicates <- as.integer(cfg$analyses$bootstrap$replicates)
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
  if (!is.finite(cfg$analyses$n_pcs) || cfg$analyses$n_pcs < 2L) stop("analyses.n_pcs must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$pca_loading_top_n) || cfg$analyses$pca_loading_top_n < 1L) stop("analyses.pca_loading_top_n must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$chromosome_min_snps) || cfg$analyses$chromosome_min_snps < 2L) stop("analyses.chromosome_min_snps must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$dapc_loading_top_n) || cfg$analyses$dapc_loading_top_n < 1L) stop("analyses.dapc_loading_top_n must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$bootstrap$replicates) || cfg$analyses$bootstrap$replicates < 0L) stop("bootstrap.replicates must be >= 0", call. = FALSE)

  allowed_formats <- c("pdf", "png", "svg")
  cfg$output$figure_formats <- unique(tolower(as.character(cfg$output$figure_formats)))
  invalid_formats <- setdiff(cfg$output$figure_formats, allowed_formats)
  if (length(invalid_formats)) stopf("Unsupported figure format(s): %s", paste(invalid_formats, collapse = ", "))
  cfg$output$figure_style <- figure_style_name(cfg)
  cfg$input$metadata_header <- tolower(as.character(cfg$input$metadata_header))
  if (!cfg$input$metadata_header %in% c("auto", "yes", "no", "true", "false")) stop("input.metadata_header must be auto, yes, or no", call. = FALSE)
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
