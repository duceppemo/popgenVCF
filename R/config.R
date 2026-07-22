#' Create the default popgenVCF configuration
#'
#' @return A nested configuration list using the supported schema.
#' @export

default_config <- function() {
  list(
    schema_version = "1.0",
    input = list(vcf = NULL, metadata = NULL, metadata_header = "auto",
                 geographic_columns = c("latitude", "longitude")),
    output = list(directory = NULL, figure_formats = c("pdf", "png"), dpi = 600L,
                  label_samples = "auto"),
    compute = list(threads = max(1L, parallel::detectCores() - 1L), seed = 42L, force_gds = FALSE),
    qc = list(maf = 0.05, max_variant_missing = 0.20, max_sample_missing = 0.20,
              ld_r2 = 0.20, ld_slide_max_bp = Inf, ld_slide_max_n = 50L,
              ld_start_pos = "first"),
    analyses = list(n_pcs = 10L, dapc = TRUE, dapc_k = "2:10", dapc_cross_validation = TRUE,
                    amova = TRUE, mantel = TRUE, isolation_by_distance = TRUE,
                    chromosome_specific = TRUE, chromosome_min_snps = 100L,
                    bootstrap = list(enabled = TRUE, replicates = 500L, unit = "chromosome"),
                    structure = list(replicates = 3L, seeds = NULL, reproducibility_rmse = 0.05,
                                     minimum_cluster_correlation = 0.90),
                    admixture = list(enabled = FALSE, executable = "admixture", plink_prefix = NULL,
                                     k = "2:10", threads = 1L, cv_folds = 5L, q_sample_file = NULL),
                    faststructure = list(enabled = FALSE, structure_executable = "structure.py",
                                         choosek_executable = "chooseK.py", plink_prefix = NULL,
                                         k = "2:10", q_sample_file = NULL),
                    snmf = list(enabled = FALSE, geno_file = NULL, q_sample_file = NULL,
                                k = "2:10", repetitions = 5L, entropy = TRUE)),
    report = list(enabled = TRUE, title = "Population genomics analysis", author = "")
  )
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

  cfg$compute$threads <- as.integer(cfg$compute$threads)
  cfg$compute$seed <- as.integer(cfg$compute$seed)
  cfg$output$dpi <- as.integer(cfg$output$dpi)
  cfg$analyses$n_pcs <- as.integer(cfg$analyses$n_pcs)
  cfg$analyses$chromosome_min_snps <- as.integer(cfg$analyses$chromosome_min_snps)
  cfg$analyses$bootstrap$replicates <- as.integer(cfg$analyses$bootstrap$replicates)
  cfg$analyses$structure$replicates <- as.integer(cfg$analyses$structure$replicates)
  cfg$analyses$snmf$repetitions <- as.integer(cfg$analyses$snmf$repetitions)
  if (!is.finite(cfg$compute$threads) || cfg$compute$threads < 1L) stop("compute.threads must be >= 1", call. = FALSE)
  if (!is.finite(cfg$compute$seed)) stop("compute.seed must be an integer", call. = FALSE)
  if (!is.finite(cfg$output$dpi) || cfg$output$dpi < 72L) stop("output.dpi must be >= 72", call. = FALSE)
  if (!is.finite(cfg$analyses$n_pcs) || cfg$analyses$n_pcs < 2L) stop("analyses.n_pcs must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$chromosome_min_snps) || cfg$analyses$chromosome_min_snps < 2L) stop("analyses.chromosome_min_snps must be >= 2", call. = FALSE)
  if (!is.finite(cfg$analyses$bootstrap$replicates) || cfg$analyses$bootstrap$replicates < 0L) stop("bootstrap.replicates must be >= 0", call. = FALSE)

  allowed_formats <- c("pdf", "png", "svg")
  cfg$output$figure_formats <- unique(tolower(as.character(cfg$output$figure_formats)))
  invalid_formats <- setdiff(cfg$output$figure_formats, allowed_formats)
  if (length(invalid_formats)) stopf("Unsupported figure format(s): %s", paste(invalid_formats, collapse = ", "))
  cfg$input$metadata_header <- tolower(as.character(cfg$input$metadata_header))
  if (!cfg$input$metadata_header %in% c("auto", "yes", "no", "true", "false")) stop("input.metadata_header must be auto, yes, or no", call. = FALSE)
  if (!is.finite(cfg$analyses$structure$replicates) || cfg$analyses$structure$replicates < 1L) stop("analyses.structure.replicates must be >= 1", call. = FALSE)
  if (!is.finite(cfg$analyses$structure$reproducibility_rmse) || cfg$analyses$structure$reproducibility_rmse < 0) stop("analyses.structure.reproducibility_rmse must be non-negative", call. = FALSE)
  if (!is.finite(cfg$analyses$structure$minimum_cluster_correlation) || cfg$analyses$structure$minimum_cluster_correlation < -1 || cfg$analyses$structure$minimum_cluster_correlation > 1) stop("analyses.structure.minimum_cluster_correlation must be between -1 and 1", call. = FALSE)

  if (isTRUE(cfg$analyses$admixture$enabled)) {
    ac <- cfg$analyses$admixture
    if (is.null(ac$plink_prefix) || !nzchar(ac$plink_prefix)) stop("analyses.admixture.plink_prefix is required when ADMIXTURE is enabled", call. = FALSE)
    if (is.null(ac$q_sample_file) || !file.exists(ac$q_sample_file)) stop("A valid analyses.admixture.q_sample_file is required", call. = FALSE)
  }
  if (isTRUE(cfg$analyses$faststructure$enabled)) {
    fc <- cfg$analyses$faststructure
    if (is.null(fc$plink_prefix) || !nzchar(fc$plink_prefix)) stop("analyses.faststructure.plink_prefix is required", call. = FALSE)
    if (is.null(fc$q_sample_file) || !file.exists(fc$q_sample_file)) stop("A valid analyses.faststructure.q_sample_file is required", call. = FALSE)
  }
  if (isTRUE(cfg$analyses$snmf$enabled)) {
    sc <- cfg$analyses$snmf
    if (is.null(sc$geno_file) || !file.exists(sc$geno_file)) stop("A valid analyses.snmf.geno_file is required", call. = FALSE)
    if (is.null(sc$q_sample_file) || !file.exists(sc$q_sample_file)) stop("A valid analyses.snmf.q_sample_file is required", call. = FALSE)
  }
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
