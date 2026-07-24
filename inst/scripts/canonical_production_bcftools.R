canonical_production_parse_index_count <- function(output) {
  lines <- trimws(as.character(output))
  lines <- lines[nzchar(lines)]
  values <- suppressWarnings(as.numeric(lines))
  valid <- !is.na(values) & is.finite(values) & values > 0 & values == floor(values)
  if (sum(valid) != 1L) return(NA_real_)
  unname(values[valid][[1L]])
}

canonical_production_validate_sexes <- function(sex, policy) {
  normalized <- tolower(trimws(as.character(sex)))
  male <- normalized %in% c("male", "m", "1")
  female <- normalized %in% c("female", "f", "2")
  satisfied <- switch(policy,
    male_only = length(normalized) > 0L && all(male),
    mixed = any(male) && any(female) && all(male | female),
    FALSE
  )
  if (!satisfied && identical(policy, "male_only")) {
    stop("canonical chromosome Y panel contains a non-male sex assignment", call. = FALSE)
  }
  if (!satisfied) stop(
    "canonical autosomal panel does not contain complete mixed-sex assignments", call. = FALSE)
  invisible(TRUE)
}

canonical_production_variant_inventory <- function(
    executable,
    vcf_path,
    run = canonical_production_system2) {
  if (!is.function(run)) stop("run must be a function", call. = FALSE)

  nrecords_output <- tryCatch(
    run(
      executable,
      c("index", "--nrecords", shQuote(vcf_path)),
      "bcftools indexed variant count"
    ),
    error = function(error) character()
  )
  nrecords <- canonical_production_parse_index_count(nrecords_output)
  if (!is.na(nrecords)) {
    return(list(
      variant_count = nrecords,
      method = "index_metadata",
      contigs = character()
    ))
  }

  format <- shQuote("%CHROM\\n")
  streamed <- run(
    executable,
    c("query", "-f", format, shQuote(vcf_path)),
    "bcftools streamed variant inventory"
  )
  streamed <- trimws(streamed[nzchar(trimws(streamed))])
  if (!length(streamed)) {
    stop("canonical VCF contains no readable variant records", call. = FALSE)
  }

  contigs <- unique(streamed)
  regions <- paste(contigs, collapse = ",")
  indexed <- run(
    executable,
    c("query", "-r", shQuote(regions), "-f", format, shQuote(vcf_path)),
    "bcftools indexed variant inventory"
  )
  indexed <- trimws(indexed[nzchar(trimws(indexed))])

  streamed_counts <- table(factor(streamed, levels = contigs))
  indexed_counts <- table(factor(indexed, levels = contigs))
  if (!length(indexed) || !identical(unname(streamed_counts), unname(indexed_counts))) {
    stop(
      "canonical VCF index does not expose the complete streamed variant inventory",
      call. = FALSE
    )
  }

  list(
    variant_count = as.numeric(length(streamed)),
    method = "indexed_query_fallback",
    contigs = contigs
  )
}

canonical_production_inspect_bcftools_compatible <- function(
    source,
    directory,
    bcftools = "bcftools") {
  popgenVCF::validate_canonical_source(source)
  directory <- canonical_production_dir(directory, "data_dir")
  executable <- Sys.which(canonical_production_scalar(bcftools, "bcftools"))
  if (!nzchar(executable)) {
    stop("bcftools is required for production inspection", call. = FALSE)
  }

  files <- source$files$filename
  vcf_name <- files[grepl("\\.vcf\\.gz$", files)]
  index_name <- files[grepl("\\.vcf\\.gz\\.tbi$", files)]
  panel_name <- files[grepl("\\.panel$", files)]
  if (length(vcf_name) != 1L || length(index_name) != 1L || length(panel_name) != 1L) {
    stop(
      "canonical source must contain exactly one VCF, tabix index, and panel",
      call. = FALSE
    )
  }

  vcf_path <- file.path(directory, vcf_name)
  panel_path <- file.path(directory, panel_name)
  version <- canonical_production_system2(
    executable,
    "--version",
    "bcftools version query"
  )
  version <- sub("^bcftools[[:space:]]+", "", version[[1L]])

  sample_ids <- canonical_production_system2(
    executable,
    c("query", "-l", shQuote(vcf_path)),
    "bcftools sample query"
  )
  sample_ids <- trimws(sample_ids[nzchar(trimws(sample_ids))])
  if (!length(sample_ids) || anyDuplicated(sample_ids)) {
    stop("canonical VCF sample identifiers are empty or duplicated", call. = FALSE)
  }

  inventory <- canonical_production_variant_inventory(executable, vcf_path)
  variant_count <- inventory$variant_count

  panel <- data.table::fread(panel_path, data.table = FALSE, check.names = FALSE)
  sample_col <- canonical_production_panel_column(
    panel,
    c("sample", "sample_id", "sampleid"),
    "sample identifier"
  )
  population_col <- canonical_production_panel_column(
    panel,
    c("pop", "population"),
    "population"
  )
  superpopulation_col <- canonical_production_panel_column(
    panel,
    c("super_pop", "superpopulation", "super_population"),
    "superpopulation"
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
    stop("canonical panel metadata is incomplete or duplicated", call. = FALSE)
  }
  if (!setequal(sample_ids, metadata$sample_id)) {
    stop("canonical VCF and panel sample inventories do not match", call. = FALSE)
  }
  metadata <- metadata[match(sample_ids, metadata$sample_id), , drop = FALSE]
  rownames(metadata) <- NULL
  canonical_production_validate_sexes(metadata$sex, source$sample_sex_policy)

  list(
    summary = data.frame(
      dataset_id = source$id,
      dataset_version = source$version,
      vcf_file = vcf_name,
      index_file = index_name,
      panel_file = panel_name,
      variant_count = variant_count,
      variant_count_method = inventory$method,
      vcf_sample_count = length(sample_ids),
      panel_sample_count = nrow(metadata),
      exact_sample_set = TRUE,
      complete_metadata = TRUE,
      chromosome_scope = source$chromosome_scope,
      sample_sex_policy = source$sample_sex_policy,
      sex_policy_satisfied = TRUE,
      bcftools_version = version,
      stringsAsFactors = FALSE
    ),
    sample_metadata = metadata,
    commands = list(
      sample_inventory = paste("bcftools query -l", shQuote(vcf_name)),
      variant_count = paste(
        "bcftools index --nrecords",
        shQuote(vcf_name),
        "with streamed/indexed bcftools query fallback when index statistics are unavailable"
      )
    )
  )
}
