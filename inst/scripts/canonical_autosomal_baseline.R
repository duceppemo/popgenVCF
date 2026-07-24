canonical_autosomal_baseline_contract <- function() {
  list(
    schema_version = "1.0",
    region = "22:20000000-21000000",
    variant_filter = "biallelic SNPs",
    maf_threshold = 0.05,
    maximum_variant_missing = 0.20,
    maximum_sample_missing = 0.20,
    ld_r2 = 0.20,
    seed = 42L,
    pca_components = 10L
  )
}

canonical_autosomal_sample_metadata <- function(panel, sample_ids) {
  panel <- as.data.frame(panel, stringsAsFactors = FALSE)
  sample_ids <- as.character(sample_ids)
  if (!length(sample_ids) || anyDuplicated(sample_ids)) {
    stop("retained sample identifiers are empty or duplicated", call. = FALSE)
  }
  sample_col <- canonical_production_panel_column(
    panel, c("sample", "sample_id", "sampleid"), "sample identifier"
  )
  population_col <- canonical_production_panel_column(
    panel, c("pop", "population"), "population"
  )
  superpopulation_col <- canonical_production_panel_column(
    panel, c("super_pop", "superpopulation", "super_population"), "superpopulation"
  )
  sex_col <- canonical_production_panel_column(panel, c("gender", "sex"), "sex")
  metadata <- data.frame(
    sample_id = trimws(as.character(panel[[sample_col]])),
    population = trimws(as.character(panel[[population_col]])),
    superpopulation = trimws(as.character(panel[[superpopulation_col]])),
    sex = trimws(as.character(panel[[sex_col]])),
    stringsAsFactors = FALSE
  )
  if (!nrow(metadata) || anyNA(metadata) || any(!nzchar(as.matrix(metadata))) ||
      anyDuplicated(metadata$sample_id)) {
    stop("autosomal baseline sample metadata is incomplete or duplicated", call. = FALSE)
  }
  if (!setequal(sample_ids, metadata$sample_id)) {
    stop("autosomal baseline analysis and panel sample inventories do not match", call. = FALSE)
  }
  metadata <- metadata[match(sample_ids, metadata$sample_id), , drop = FALSE]
  rownames(metadata) <- NULL
  metadata
}

canonical_autosomal_observations <- function(analysis, subset_variant_count) {
  if (!popgenVCF::is_popgen_vcf_analysis(analysis) ||
      !identical(analysis$status, "complete")) {
    stop("analysis must be a complete PopgenVCFAnalysis", call. = FALSE)
  }
  subset_variant_count <- as.integer(subset_variant_count)
  if (length(subset_variant_count) != 1L || is.na(subset_variant_count) ||
      subset_variant_count < 1L) {
    stop("subset_variant_count must be one positive integer", call. = FALSE)
  }
  pca <- popgenVCF::get_analysis_result(analysis, "pca")
  if (!is.list(pca) || is.null(pca$variance) || nrow(pca$variance) < 2L) {
    stop("analysis must contain at least two PCA variance components", call. = FALSE)
  }
  pc <- as.numeric(pca$variance$proportion[seq_len(2L)])
  if (any(!is.finite(pc)) || any(pc <= 0) || sum(pc) > 1 + 1e-12) {
    stop("PCA variance observations are invalid", call. = FALSE)
  }
  observed <- list(
    subset_variant_count = subset_variant_count,
    retained_sample_count = as.integer(length(analysis$samples$ids)),
    qc_variant_count = as.integer(length(analysis$variants$qc_ids)),
    ld_pruned_variant_count = as.integer(length(analysis$variants$ld_ids)),
    pca_pc1_variance_proportion = pc[[1L]],
    pca_pc2_variance_proportion = pc[[2L]]
  )
  if (any(vapply(observed[seq_len(4L)], function(x) is.na(x) || x < 1L, logical(1)))) {
    stop("autosomal count observations must all be positive", call. = FALSE)
  }
  observed
}

canonical_autosomal_baseline_registry <- function(observed, dataset_id, provenance) {
  if (!is.list(observed) || !identical(
    sort(names(observed)),
    sort(c(
      "subset_variant_count", "retained_sample_count", "qc_variant_count",
      "ld_pruned_variant_count", "pca_pc1_variance_proportion",
      "pca_pc2_variance_proportion"
    ))
  )) stop("autosomal observations have an invalid inventory", call. = FALSE)
  if (!is.list(provenance) || !length(provenance) || is.null(names(provenance))) {
    stop("autosomal baseline provenance must be a non-empty named list", call. = FALSE)
  }
  definitions <- list(
    subset_variant_count = list("qc", "exact", 0,
      "The checksum-bound region and biallelic-SNP derivation must retain an exact variant count."),
    retained_sample_count = list("qc", "exact", 0,
      "Sample QC must retain the exact complete canonical sample inventory."),
    qc_variant_count = list("qc", "exact", 0,
      "Fixed MAF and missingness rules are deterministic for the checksum-bound derived input."),
    ld_pruned_variant_count = list("qc", "exact", 0,
      "The fixed seed, SNP order, LD threshold, and pruning window define an exact retained count."),
    pca_pc1_variance_proportion = list("pca", "relative", 1e-6,
      "PC1 variance should be numerically stable across supported SNPRelate and BLAS environments."),
    pca_pc2_variance_proportion = list("pca", "relative", 1e-6,
      "PC2 variance should be numerically stable across supported SNPRelate and BLAS environments.")
  )
  metrics <- lapply(names(definitions), function(id) {
    definition <- definitions[[id]]
    popgenVCF::new_canonical_baseline_metric(
      id = id, dataset_id = dataset_id, analysis = definition[[1L]],
      expected = observed[[id]], comparator = definition[[2L]],
      tolerance = definition[[3L]], version = "proposal-1",
      rationale = definition[[4L]], provenance = provenance
    )
  })
  popgenVCF::new_canonical_baseline_registry(metrics)
}

canonical_autosomal_observation_table <- function(observed) {
  data.frame(
    metric_id = names(observed),
    value = vapply(observed, function(x) format(
      x, digits = 17L, scientific = FALSE, trim = TRUE
    ), character(1)),
    stringsAsFactors = FALSE
  )
}

canonical_autosomal_copy_evidence <- function(analysis, output_dir) {
  root <- canonical_production_dir(analysis$dirs$root, "analysis output directory")
  relative <- c(
    "analysis_execution_ledger.tsv", "analysis_execution_plan.tsv",
    "analysis_summary.tsv", "analysis_validation.tsv", "pipeline.log",
    file.path("tables", "01_sample_QC.tsv"),
    file.path("tables", "02_sample_metadata_match.tsv"),
    file.path("tables", "06_QC_independent_counts.tsv"),
    file.path("tables", "07_QC_sequential_counts.tsv"),
    file.path("tables", "12_PCA_scores.tsv"),
    file.path("tables", "13_PCA_variance.tsv")
  )
  source <- file.path(root, relative)
  if (any(!file.exists(source))) {
    stop("autosomal analysis is missing required evidence files", call. = FALSE)
  }
  destination <- file.path(output_dir, "analysis", relative)
  for (directory in unique(dirname(destination))) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  copied <- file.copy(source, destination, overwrite = FALSE)
  if (!all(copied)) stop("failed to retain autosomal analysis evidence", call. = FALSE)
  normalizePath(destination, winslash = "/", mustWork = TRUE)
}

verify_canonical_autosomal_baseline_proposal <- function(output_dir) {
  output_dir <- canonical_production_dir(output_dir, "baseline proposal output")
  checksum_path <- file.path(output_dir, "autosomal-baseline-SHA256SUMS.txt")
  if (!file.exists(checksum_path)) stop("autosomal baseline checksum inventory is missing", call. = FALSE)
  lines <- readLines(checksum_path, warn = FALSE)
  if (!length(lines) || any(!grepl("^[a-f0-9]{64}  [^/].+", lines))) {
    stop("autosomal baseline checksum inventory is malformed", call. = FALSE)
  }
  expected <- substr(lines, 1L, 64L)
  relative <- substring(lines, 67L)
  if (anyDuplicated(relative) || any(startsWith(relative, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", relative))) {
    stop("autosomal baseline checksum paths are unsafe or duplicated", call. = FALSE)
  }
  paths <- file.path(output_dir, relative)
  if (any(!file.exists(paths)) || any(file.info(paths)$isdir) ||
      any(nzchar(Sys.readlink(paths)))) {
    stop("autosomal baseline evidence is missing or is not regular", call. = FALSE)
  }
  actual <- sort(list.files(output_dir, recursive = TRUE, full.names = TRUE))
  actual <- actual[basename(actual) != basename(checksum_path)]
  actual_relative <- vapply(
    actual, canonical_production_relative, character(1), root = output_dir
  )
  if (!identical(unname(sort(relative)), unname(sort(actual_relative)))) {
    stop("autosomal baseline checksum inventory is incomplete", call. = FALSE)
  }
  observed <- vapply(paths, canonical_production_sha256, character(1))
  if (!identical(unname(expected), unname(observed))) {
    stop("autosomal baseline checksum verification failed", call. = FALSE)
  }
  forbidden <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
  forbidden <- forbidden[grepl("\\.(vcf|vcf\\.gz|tbi|gds)$", forbidden, ignore.case = TRUE)]
  if (length(forbidden)) stop("raw genotype data were placed in baseline evidence", call. = FALSE)
  invisible(TRUE)
}

write_canonical_autosomal_baseline_proposal <- function(
    analysis, source, source_dir, panel, subset, output_dir,
    candidate_id, git_commit, generated_at, environment = NULL) {
  candidate_id <- canonical_production_scalar(candidate_id, "candidate_id")
  git_commit <- canonical_production_commit(git_commit)
  generated_at <- canonical_production_timestamp(generated_at)
  popgenVCF::validate_canonical_source(source)
  source_dir <- canonical_production_dir(source_dir, "source_dir")
  descriptor <- popgenVCF::canonical_dataset_from_source(source, source_dir)
  if (!is.list(subset) || !all(c(
    "region", "variant_filter", "variant_count", "source_vcf_sha256",
    "derived_vcf_sha256", "derived_index_sha256", "command", "bcftools_version"
  ) %in% names(subset))) stop("derived subset evidence is incomplete", call. = FALSE)
  contract <- canonical_autosomal_baseline_contract()
  if (!identical(subset$region, contract$region) ||
      !identical(subset$variant_filter, contract$variant_filter)) {
    stop("derived subset does not match the autosomal baseline contract", call. = FALSE)
  }
  digest_fields <- c(
    "source_vcf_sha256", "derived_vcf_sha256", "derived_index_sha256"
  )
  if (any(!vapply(subset[digest_fields], function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("^[a-f0-9]{64}$", x)
  }, logical(1)))) {
    stop("derived subset SHA-256 evidence is invalid", call. = FALSE)
  }
  vcf_file <- descriptor$files[grepl("\\.vcf\\.gz$", descriptor$files$filename), , drop = FALSE]
  if (nrow(vcf_file) != 1L ||
      !identical(subset$source_vcf_sha256, vcf_file$sha256[[1L]])) {
    stop("derived subset is not bound to the approved source VCF", call. = FALSE)
  }
  canonical_production_scalar(subset$command, "derived subset command")
  canonical_production_scalar(subset$bcftools_version, "bcftools_version")
  output_dir <- canonical_production_dir(
    output_dir, "baseline proposal output", create = TRUE, empty = TRUE
  )
  metadata <- canonical_autosomal_sample_metadata(panel, analysis$samples$ids)
  observed <- canonical_autosomal_observations(analysis, subset$variant_count)
  provenance <- list(
    candidate_id = candidate_id, git_commit = git_commit,
    source_dataset_version = source$version, region = subset$region,
    variant_filter = subset$variant_filter, source_vcf_sha256 = subset$source_vcf_sha256,
    derived_vcf_sha256 = subset$derived_vcf_sha256,
    derived_index_sha256 = subset$derived_index_sha256,
    maf_threshold = contract$maf_threshold,
    maximum_variant_missing = contract$maximum_variant_missing,
    maximum_sample_missing = contract$maximum_sample_missing,
    ld_r2 = contract$ld_r2, seed = contract$seed,
    pca_components = contract$pca_components,
    threads = as.integer(analysis$config$compute$threads)
  )
  registry <- canonical_autosomal_baseline_registry(observed, descriptor$id, provenance)
  snapshot <- popgenVCF::new_canonical_real_data_baseline_snapshot(
    dataset = descriptor, registry = registry, sample_metadata = metadata,
    dataset_version = descriptor$version,
    generated_by = "canonical-real-data.yml autosomal QC/PCA baseline proposal",
    generated_at = generated_at, source_commit = git_commit,
    approval = "proposed",
    notes = paste(
      "Unapproved proposal from the checksum-bound chromosome 22 interval",
      subset$region, "using fixed QC, LD-pruning, and PCA parameters."
    )
  )
  snapshot_path <- popgenVCF::write_canonical_real_data_baseline_snapshot(
    snapshot, file.path(output_dir, "autosomal-baseline-proposal.json")
  )
  observations_path <- file.path(output_dir, "autosomal-baseline-observations.tsv")
  data.table::fwrite(
    canonical_autosomal_observation_table(observed), observations_path,
    sep = "\t", quote = FALSE
  )
  subset_path <- file.path(output_dir, "autosomal-baseline-derived-input.json")
  jsonlite::write_json(
    c(list(
      schema_version = "1.0", dataset_id = descriptor$id,
      analysis_contract = contract
    ), subset),
    subset_path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 17
  )
  analysis_paths <- canonical_autosomal_copy_evidence(analysis, output_dir)
  if (is.null(environment)) {
    environment <- canonical_production_environment(list(
      bcftools_version = subset$bcftools_version,
      analysis_modules = "pca",
      analysis_seed = contract$seed
    ))
  }
  environment_path <- canonical_production_write_environment(
    environment, file.path(output_dir, "autosomal-baseline-environment.tsv")
  )
  primary <- c(snapshot_path, observations_path, subset_path, environment_path, analysis_paths)
  proposal <- list(
    schema_version = "1.0", record_type = "canonical_autosomal_baseline_proposal",
    candidate_id = candidate_id, package_version = as.character(utils::packageVersion("popgenVCF")),
    git_commit = git_commit, generated_at = generated_at,
    dataset_id = descriptor$id, dataset_version = descriptor$version,
    approval = "proposed", production_baseline_gate = "not_passed",
    metric_count = length(observed), raw_genotype_data_in_evidence = FALSE,
    statement = paste(
      "This bundle is a quantitative proposal for scientific review.",
      "It is not an approved baseline or release-candidate gate record."
    )
  )
  proposal_path <- file.path(output_dir, "autosomal-baseline-proposal-record.json")
  jsonlite::write_json(
    proposal, proposal_path, auto_unbox = TRUE, pretty = TRUE,
    null = "null", digits = 17
  )
  artifacts <- canonical_production_artifacts(c(primary, proposal_path), output_dir)
  manifest_path <- file.path(output_dir, "autosomal-baseline-artifacts.tsv")
  data.table::fwrite(artifacts, manifest_path, sep = "\t", quote = FALSE)
  checksum_path <- canonical_production_write_checksums(
    output_dir, file.path(output_dir, "autosomal-baseline-SHA256SUMS.txt")
  )
  verify_canonical_autosomal_baseline_proposal(output_dir)
  list(
    output_dir = output_dir, snapshot = snapshot_path, proposal = proposal_path,
    checksums = checksum_path, observations = observed,
    dataset_id = descriptor$id, approval = "proposed"
  )
}
