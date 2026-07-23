canonical_production_scalar <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  trimws(x)
}

canonical_production_commit <- function(x) {
  value <- tolower(canonical_production_scalar(x, "git_commit"))
  if (!grepl("^[0-9a-f]{40}$", value)) {
    stop("git_commit must be a full lowercase 40-character SHA", call. = FALSE)
  }
  value
}

canonical_production_timestamp <- function(x) {
  value <- canonical_production_scalar(x, "generated_at")
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", value)) {
    stop("generated_at must be an ISO-8601 UTC timestamp", call. = FALSE)
  }
  value
}

canonical_production_require_namespaces <- function() {
  required <- c("popgenVCF", "data.table", "digest", "jsonlite")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(required)
}

canonical_production_dir <- function(path, label, create = FALSE, empty = FALSE) {
  path <- canonical_production_scalar(path, label)
  if (isTRUE(create) && !dir.exists(path) &&
      !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    stop("failed to create ", label, call. = FALSE)
  }
  if (!dir.exists(path)) stop(label, " does not exist", call. = FALSE)
  if (isTRUE(empty) && length(list.files(path, all.files = TRUE, no.. = TRUE))) {
    stop(label, " must not contain pre-existing evidence", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

canonical_production_is_within <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

canonical_production_relative <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop("evidence artifact is outside output_dir: ", path, call. = FALSE)
  }
  substring(path, nchar(prefix) + 1L)
}

canonical_production_sha256 <- function(path) {
  tolower(digest::digest(path, algo = "sha256", file = TRUE))
}

canonical_production_system2 <- function(command, args, label) {
  output <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (as.integer(status) != 0L) {
    detail <- paste(output, collapse = "\n")
    stop(label, " failed", if (nzchar(detail)) paste0(":\n", detail) else "", call. = FALSE)
  }
  output
}

canonical_production_stage_source <- function(source, destination, source_dir = NULL,
                                              allow_download = FALSE, quiet = TRUE) {
  popgenVCF::validate_canonical_source(source)
  destination <- canonical_production_dir(destination, "data_dir", create = TRUE)
  if (!is.null(source_dir)) source_dir <- canonical_production_dir(source_dir, "source_dir")
  if (!is.logical(allow_download) || length(allow_download) != 1L || is.na(allow_download)) {
    stop("allow_download must be TRUE or FALSE", call. = FALSE)
  }

  rows <- lapply(seq_len(nrow(source$files)), function(i) {
    spec <- source$files[i, , drop = FALSE]
    target <- file.path(destination, spec$filename)
    method <- "preexisting_verified"

    if (file.exists(target)) {
      if (!identical(tolower(unname(tools::md5sum(target))), spec$upstream_md5)) {
        stop("pre-existing canonical source file has an unexpected MD5: ",
             spec$filename, call. = FALSE)
      }
    } else {
      temporary <- tempfile(pattern = paste0(spec$filename, "."), tmpdir = destination)
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      candidate <- if (is.null(source_dir)) "" else file.path(source_dir, spec$filename)

      if (nzchar(candidate) && file.exists(candidate)) {
        if (!file.copy(candidate, temporary, overwrite = TRUE)) {
          stop("failed to copy canonical source file: ", spec$filename, call. = FALSE)
        }
        method <- "local_mirror"
      } else if (isTRUE(allow_download) && !is.na(spec$source) && nzchar(spec$source)) {
        status <- tryCatch(
          utils::download.file(spec$source, temporary, mode = "wb", quiet = quiet),
          error = identity
        )
        if (inherits(status, "error") || as.integer(status) != 0L) {
          stop("failed to download canonical source file: ", spec$filename, call. = FALSE)
        }
        method <- "approved_remote_source"
      } else {
        stop("canonical source file is unavailable locally and downloads are disabled: ",
             spec$filename, call. = FALSE)
      }

      observed_md5 <- tolower(unname(tools::md5sum(temporary)))
      if (!identical(observed_md5, spec$upstream_md5)) {
        stop("upstream MD5 verification failed: ", spec$filename, call. = FALSE)
      }
      if (!file.rename(temporary, target) &&
          !file.copy(temporary, target, overwrite = TRUE)) {
        stop("failed to install canonical source file: ", spec$filename, call. = FALSE)
      }
    }

    data.frame(
      filename = spec$filename,
      acquisition_method = method,
      source = spec$source,
      observed_size = as.numeric(file.info(target)$size),
      expected_md5 = spec$upstream_md5,
      observed_md5 = tolower(unname(tools::md5sum(target))),
      sha256 = canonical_production_sha256(target),
      stringsAsFactors = FALSE
    )
  })

  acquisition <- do.call(rbind, rows)
  acquisition$md5_ok <- acquisition$observed_md5 == acquisition$expected_md5
  acquisition$passed <- acquisition$md5_ok & grepl("^[a-f0-9]{64}$", acquisition$sha256)
  acquisition <- acquisition[order(acquisition$filename), , drop = FALSE]
  rownames(acquisition) <- NULL
  verification <- popgenVCF::verify_canonical_source(source, destination)
  if (!all(acquisition$passed) || !all(verification$passed)) {
    stop("canonical source acquisition verification failed", call. = FALSE)
  }
  list(directory = destination, acquisition = acquisition, verification = verification)
}

canonical_production_panel_column <- function(panel, aliases, label) {
  normalized <- tolower(gsub("[^a-z0-9]+", "_", names(panel)))
  index <- match(aliases, normalized, nomatch = 0L)
  index <- index[index > 0L]
  if (!length(index)) stop("canonical panel is missing the ", label, " column", call. = FALSE)
  names(panel)[index[[1L]]]
}

canonical_production_inspect_bcftools <- function(source, directory, bcftools = "bcftools") {
  popgenVCF::validate_canonical_source(source)
  directory <- canonical_production_dir(directory, "data_dir")
  executable <- Sys.which(canonical_production_scalar(bcftools, "bcftools"))
  if (!nzchar(executable)) stop("bcftools is required for production inspection", call. = FALSE)

  files <- source$files$filename
  vcf_name <- files[grepl("\\.vcf\\.gz$", files)]
  index_name <- files[grepl("\\.vcf\\.gz\\.tbi$", files)]
  panel_name <- files[grepl("\\.panel$", files)]
  if (length(vcf_name) != 1L || length(index_name) != 1L || length(panel_name) != 1L) {
    stop("canonical source must contain exactly one VCF, tabix index, and panel", call. = FALSE)
  }

  vcf_path <- file.path(directory, vcf_name)
  panel_path <- file.path(directory, panel_name)
  version <- canonical_production_system2(executable, "--version", "bcftools version query")
  version <- sub("^bcftools[[:space:]]+", "", version[[1L]])
  sample_ids <- canonical_production_system2(
    executable, c("query", "-l", shQuote(vcf_path)), "bcftools sample query"
  )
  sample_ids <- trimws(sample_ids[nzchar(trimws(sample_ids))])
  if (!length(sample_ids) || anyDuplicated(sample_ids)) {
    stop("canonical VCF sample identifiers are empty or duplicated", call. = FALSE)
  }
  variants <- canonical_production_system2(
    executable, c("index", "--nrecords", shQuote(vcf_path)),
    "bcftools indexed variant count"
  )
  variant_count <- suppressWarnings(as.numeric(trimws(variants[[1L]])))
  if (length(variant_count) != 1L || is.na(variant_count) || variant_count <= 0) {
    stop("canonical VCF has an invalid indexed variant count", call. = FALSE)
  }

  panel <- data.table::fread(panel_path, data.table = FALSE, check.names = FALSE)
  sample_col <- canonical_production_panel_column(panel, c("sample", "sample_id", "sampleid"), "sample identifier")
  population_col <- canonical_production_panel_column(panel, c("pop", "population"), "population")
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
    stop("canonical panel metadata is incomplete or duplicated", call. = FALSE)
  }
  if (!setequal(sample_ids, metadata$sample_id)) {
    stop("canonical VCF and panel sample inventories do not match", call. = FALSE)
  }
  metadata <- metadata[match(sample_ids, metadata$sample_id), , drop = FALSE]
  rownames(metadata) <- NULL
  if (!all(tolower(metadata$sex) %in% c("male", "m", "1"))) {
    stop("canonical chromosome Y panel contains a non-male sex assignment", call. = FALSE)
  }

  list(
    summary = data.frame(
      dataset_id = source$id,
      dataset_version = source$version,
      vcf_file = vcf_name,
      index_file = index_name,
      panel_file = panel_name,
      variant_count = variant_count,
      vcf_sample_count = length(sample_ids),
      panel_sample_count = nrow(metadata),
      exact_sample_set = TRUE,
      complete_metadata = TRUE,
      male_only = TRUE,
      bcftools_version = version,
      stringsAsFactors = FALSE
    ),
    sample_metadata = metadata,
    commands = list(
      sample_inventory = paste("bcftools query -l", shQuote(vcf_name)),
      variant_count = paste("bcftools index --nrecords", shQuote(vcf_name))
    )
  )
}

canonical_production_validate_inspection <- function(inspection, source) {
  required <- c("summary", "sample_metadata", "commands")
  if (!is.list(inspection) || !all(required %in% names(inspection))) {
    stop("inspection must contain summary, sample_metadata, and commands", call. = FALSE)
  }
  summary <- as.data.frame(inspection$summary, stringsAsFactors = FALSE)
  metadata <- as.data.frame(inspection$sample_metadata, stringsAsFactors = FALSE)
  summary_fields <- c(
    "dataset_id", "dataset_version", "variant_count", "vcf_sample_count",
    "panel_sample_count", "exact_sample_set", "complete_metadata", "male_only",
    "bcftools_version"
  )
  metadata_fields <- c("sample_id", "population", "superpopulation", "sex")
  commands <- inspection$commands
  valid_commands <- is.list(commands) && length(commands) && !is.null(names(commands)) &&
    all(nzchar(names(commands))) && all(vapply(commands, function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
    }, logical(1)))
  if (!valid_commands) stop("inspection commands are invalid", call. = FALSE)
  if (nrow(summary) != 1L || !all(summary_fields %in% names(summary))) {
    stop("inspection summary has an invalid schema", call. = FALSE)
  }
  if (!identical(as.character(summary$dataset_id[[1L]]), source$id) ||
      !identical(as.character(summary$dataset_version[[1L]]), source$version) ||
      !isTRUE(summary$variant_count[[1L]] > 0) ||
      !isTRUE(summary$vcf_sample_count[[1L]] > 0) ||
      as.numeric(summary$vcf_sample_count[[1L]]) != as.numeric(summary$panel_sample_count[[1L]]) ||
      !all(vapply(c("exact_sample_set", "complete_metadata", "male_only"),
                  function(field) isTRUE(summary[[field]][[1L]]), logical(1)))) {
    stop("inspection summary does not satisfy canonical production requirements", call. = FALSE)
  }
  if (!nrow(metadata) || !all(metadata_fields %in% names(metadata)) ||
      anyNA(metadata[metadata_fields]) || any(!nzchar(as.matrix(metadata[metadata_fields]))) ||
      anyDuplicated(metadata$sample_id) || nrow(metadata) != summary$vcf_sample_count[[1L]]) {
    stop("inspection sample metadata is incomplete or inconsistent", call. = FALSE)
  }
  metadata <- metadata[order(metadata$sample_id), metadata_fields, drop = FALSE]
  rownames(metadata) <- NULL
  list(summary = summary, sample_metadata = metadata, commands = commands[sort(names(commands))])
}

canonical_production_environment <- function(extra = list()) {
  packages <- c("popgenVCF", "data.table", "digest", "jsonlite")
  versions <- vapply(packages, function(x) as.character(utils::packageVersion(x)), character(1))
  base <- list(
    r_version = R.version.string,
    platform = R.version$platform,
    os = paste(Sys.info()[c("sysname", "release", "version", "machine")], collapse = " | "),
    locale = Sys.getlocale(),
    packages = as.list(versions)
  )
  c(base, extra[sort(names(extra))])
}

canonical_production_write_environment <- function(environment, path) {
  flatten <- function(x, prefix = "") {
    rows <- list()
    for (name in sort(names(x))) {
      key <- if (nzchar(prefix)) paste(prefix, name, sep = ".") else name
      value <- x[[name]]
      if (is.list(value)) {
        rows <- c(rows, flatten(value, key))
      } else {
        rows[[length(rows) + 1L]] <- data.frame(
          key = key, value = paste(as.character(value), collapse = "|"),
          stringsAsFactors = FALSE
        )
      }
    }
    rows
  }
  rows <- flatten(environment)
  table <- if (length(rows)) do.call(rbind, rows) else data.frame(
    key = character(), value = character(), stringsAsFactors = FALSE
  )
  data.table::fwrite(table, path, sep = "\t", quote = FALSE, na = "NA")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

canonical_production_artifacts <- function(paths, root) {
  paths <- unique(normalizePath(paths, winslash = "/", mustWork = TRUE))
  table <- data.frame(
    path = vapply(paths, canonical_production_relative, character(1), root = root),
    size_bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(paths, canonical_production_sha256, character(1)),
    stringsAsFactors = FALSE
  )
  table <- table[order(table$path), , drop = FALSE]
  rownames(table) <- NULL
  table
}

canonical_production_write_checksums <- function(root, path) {
  files <- sort(list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE))
  checksum_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  files <- files[normalizePath(files, winslash = "/", mustWork = TRUE) != checksum_path]
  relative <- vapply(files, canonical_production_relative, character(1), root = root)
  hashes <- vapply(files, canonical_production_sha256, character(1))
  writeLines(paste0(hashes, "  ", relative), path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

verify_canonical_production_evidence <- function(output_dir) {
  output_dir <- canonical_production_dir(output_dir, "output_dir")
  checksum_path <- file.path(output_dir, "canonical-production-SHA256SUMS.txt")
  if (!file.exists(checksum_path)) stop("canonical production checksum inventory is missing", call. = FALSE)
  lines <- readLines(checksum_path, warn = FALSE)
  if (!length(lines) || any(!grepl("^[a-f0-9]{64}  [^/].+", lines))) {
    stop("canonical production checksum inventory is malformed", call. = FALSE)
  }
  expected_hashes <- substr(lines, 1L, 64L)
  relative <- substring(lines, 67L)
  if (anyDuplicated(relative) || any(startsWith(relative, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", relative))) {
    stop("canonical production checksum paths are unsafe or duplicated", call. = FALSE)
  }
  paths <- file.path(output_dir, relative)
  if (any(!file.exists(paths)) || any(file.info(paths)$isdir) || any(nzchar(Sys.readlink(paths)))) {
    stop("canonical production evidence file is missing or is not regular", call. = FALSE)
  }
  actual_files <- sort(list.files(output_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE))
  actual_files <- actual_files[basename(actual_files) != "canonical-production-SHA256SUMS.txt"]
  actual_relative <- vapply(actual_files, canonical_production_relative, character(1), root = output_dir)
  if (!identical(sort(relative), sort(actual_relative))) {
    stop("canonical production checksum inventory is incomplete", call. = FALSE)
  }
  observed_hashes <- vapply(paths, canonical_production_sha256, character(1))
  if (!identical(unname(observed_hashes), unname(expected_hashes))) {
    stop("canonical production evidence checksum verification failed", call. = FALSE)
  }
  invisible(TRUE)
}

run_canonical_production_execution <- function(
    output_dir, data_dir, candidate_id, git_commit, generated_at,
    source = popgenVCF::canonical_1000g_chrY_source(), source_dir = NULL,
    allow_download = FALSE, quiet = TRUE,
    inspect = canonical_production_inspect_bcftools, environment = NULL) {
  canonical_production_require_namespaces()
  candidate_id <- canonical_production_scalar(candidate_id, "candidate_id")
  git_commit <- canonical_production_commit(git_commit)
  generated_at <- canonical_production_timestamp(generated_at)
  popgenVCF::validate_canonical_source(source)
  if (!is.function(inspect)) stop("inspect must be a function", call. = FALSE)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- canonical_production_dir(output_dir, "output_dir", empty = TRUE)
  data_dir <- canonical_production_dir(data_dir, "data_dir", create = TRUE)
  if (canonical_production_is_within(data_dir, output_dir)) {
    stop("data_dir must be outside output_dir so raw canonical data cannot be uploaded", call. = FALSE)
  }

  staged <- canonical_production_stage_source(
    source, data_dir, source_dir = source_dir,
    allow_download = allow_download, quiet = quiet
  )
  inspection <- canonical_production_validate_inspection(
    inspect(source, staged$directory), source
  )
  descriptor <- popgenVCF::canonical_dataset_from_source(source, staged$directory)
  registry <- popgenVCF::register_canonical_dataset(
    popgenVCF::new_canonical_dataset_registry(), descriptor,
    approval = "approved", reviewed_by = source$reviewed_by,
    reviewed_at = source$reviewed_at,
    notes = paste("Approved upstream archive", source$doi,
                  "verified by MD5 and promoted to SHA-256.")
  )

  source_dir_out <- file.path(output_dir, "source")
  dataset_dir_out <- file.path(source_dir_out, "dataset")
  dir.create(dataset_dir_out, recursive = TRUE, showWarnings = FALSE)
  acquisition_path <- file.path(source_dir_out, "canonical_source_acquisition.tsv")
  verification_path <- file.path(source_dir_out, "canonical_source_verification.tsv")
  registry_path <- file.path(source_dir_out, "canonical_dataset_registry.tsv")
  structure_path <- file.path(output_dir, "canonical_dataset_structure.tsv")
  metadata_path <- file.path(output_dir, "canonical_sample_metadata.tsv")
  environment_path <- file.path(output_dir, "canonical-production-environment.tsv")
  execution_path <- file.path(output_dir, "canonical-production-execution.json")

  data.table::fwrite(staged$acquisition, acquisition_path, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(staged$verification, verification_path, sep = "\t", quote = FALSE, na = "NA")
  popgenVCF::write_canonical_dataset_registry(registry, registry_path)
  dataset_paths <- popgenVCF::write_canonical_validation_evidence(
    descriptor, staged$directory, dataset_dir_out
  )
  data.table::fwrite(inspection$summary, structure_path, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(inspection$sample_metadata, metadata_path, sep = "\t", quote = FALSE, na = "NA")
  if (is.null(environment)) {
    environment <- canonical_production_environment(list(
      bcftools_version = as.character(inspection$summary$bcftools_version[[1L]])
    ))
  }
  canonical_production_write_environment(environment, environment_path)

  primary_paths <- c(
    acquisition_path, verification_path, registry_path,
    unname(unlist(dataset_paths[!is.na(dataset_paths)], use.names = FALSE)),
    structure_path, metadata_path, environment_path
  )
  primary_artifacts <- canonical_production_artifacts(primary_paths, output_dir)
  execution <- list(
    schema_version = "1.0",
    record_type = "canonical_production_execution",
    candidate_id = candidate_id,
    package_version = as.character(utils::packageVersion("popgenVCF")),
    git_commit = git_commit,
    generated_at = generated_at,
    status = "passed",
    dataset = list(
      id = descriptor$id, version = descriptor$version, doi = source$doi,
      license = descriptor$license,
      files = lapply(seq_len(nrow(descriptor$files)), function(i) {
        as.list(descriptor$files[i, c("filename", "sha256", "size_bytes", "source"), drop = FALSE])
      })
    ),
    commands = inspection$commands,
    validation = list(
      upstream_md5_verified = TRUE, sha256_promoted = TRUE,
      indexed_vcf_readable = TRUE, sample_inventory_matched = TRUE,
      metadata_complete = TRUE, male_only_chrY_panel = TRUE,
      variant_count = as.numeric(inspection$summary$variant_count[[1L]]),
      sample_count = as.integer(inspection$summary$vcf_sample_count[[1L]])
    ),
    gate_states = list(
      canonical_validation = "passed",
      production_baseline = "not_run",
      external_concordance = "not_run"
    ),
    data_retention = list(
      raw_dataset_in_evidence_bundle = FALSE,
      statement = paste(
        "The workflow retains checksum, provenance, structure, and sample-metadata",
        "evidence only; raw canonical source files remain outside the uploaded artifact."
      )
    ),
    artifacts = lapply(seq_len(nrow(primary_artifacts)), function(i) {
      as.list(primary_artifacts[i, , drop = FALSE])
    })
  )
  jsonlite::write_json(
    execution, execution_path, auto_unbox = TRUE, pretty = TRUE,
    null = "null", na = "null", digits = 17
  )

  artifact_table <- canonical_production_artifacts(c(primary_paths, execution_path), output_dir)
  manifest_path <- file.path(output_dir, "canonical-production-artifacts.tsv")
  data.table::fwrite(artifact_table, manifest_path, sep = "\t", quote = FALSE, na = "NA")
  gate_record <- list(
    gate_id = "canonical_validation",
    status = "passed",
    summary = paste0(
      "Approved canonical dataset ", descriptor$id, " ", descriptor$version,
      " was acquired in an external data directory, verified against the approved ",
      "upstream MD5 inventory, promoted to SHA-256, and structurally inspected ",
      "with matching complete sample metadata."
    ),
    artifacts = lapply(seq_len(nrow(artifact_table)), function(i) {
      as.list(artifact_table[i, , drop = FALSE])
    }),
    approval = NULL
  )
  gate_path <- file.path(output_dir, "canonical-validation-gate-record.json")
  jsonlite::write_json(
    gate_record, gate_path, auto_unbox = TRUE, pretty = TRUE,
    null = "null", na = "null", digits = 17
  )
  checksum_path <- canonical_production_write_checksums(
    output_dir, file.path(output_dir, "canonical-production-SHA256SUMS.txt")
  )
  verify_canonical_production_evidence(output_dir)

  list(
    output_dir = output_dir, data_dir = data_dir,
    execution = normalizePath(execution_path, winslash = "/", mustWork = TRUE),
    gate_record = normalizePath(gate_path, winslash = "/", mustWork = TRUE),
    artifacts = normalizePath(manifest_path, winslash = "/", mustWork = TRUE),
    checksums = checksum_path,
    dataset_id = descriptor$id, dataset_version = descriptor$version,
    sample_count = as.integer(inspection$summary$vcf_sample_count[[1L]]),
    variant_count = as.numeric(inspection$summary$variant_count[[1L]])
  )
}
