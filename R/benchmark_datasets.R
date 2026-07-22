benchmark_dataset_id <- function(x, label = "id") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  tolower(x)
}

#' Create a benchmark dataset catalogue entry
#'
#' @param id Stable dataset identifier.
#' @param version Dataset release version.
#' @param scale Dataset scale.
#' @param source_type One of `embedded`, `local`, or `remote`.
#' @param filename Cached file name.
#' @param checksum Optional SHA256 checksum.
#' @param source Source path, URL, or embedded materializer function.
#' @param organism Organism label.
#' @param analyses Character vector of supported analysis identifiers.
#' @param required_software Character vector of external software requirements.
#' @param estimated_runtime_seconds,estimated_memory_mb Resource estimates.
#' @param published Whether a remote entry is currently published.
#' @param metadata Additional named metadata.
#' @return A validated `PopgenVCFBenchmarkDatasetEntry`.
#' @export
new_benchmark_dataset_entry <- function(id, version = "1", scale = "tiny",
                                        source_type = c("embedded", "local", "remote"),
                                        filename, checksum = NA_character_, source,
                                        organism = "unspecified", analyses = character(),
                                        required_software = character(),
                                        estimated_runtime_seconds = NA_real_,
                                        estimated_memory_mb = NA_real_,
                                        published = TRUE, metadata = list()) {
  id <- benchmark_dataset_id(id)
  version <- benchmark_dataset_id(as.character(version), "version")
  scale <- match.arg(scale, c("tiny", "small", "medium", "large", "real"))
  source_type <- match.arg(source_type)
  filename <- basename(benchmark_dataset_id(filename, "filename"))
  if (source_type == "embedded" && !is.function(source)) {
    stop("embedded dataset source must be a materializer function", call. = FALSE)
  }
  if (source_type != "embedded" &&
      (!is.character(source) || length(source) != 1L || is.na(source) || !nzchar(source))) {
    stop("local and remote dataset sources must be one path or URL", call. = FALSE)
  }
  checksum <- as.character(checksum)[1L]
  if (!is.na(checksum) && !grepl("^[a-fA-F0-9]{64}$", checksum)) {
    stop("checksum must be a SHA256 hexadecimal string", call. = FALSE)
  }
  estimates <- c(estimated_runtime_seconds, estimated_memory_mb)
  if (any(!is.na(estimates) & (!is.finite(estimates) | estimates < 0))) {
    stop("resource estimates must be nonnegative finite values or NA", call. = FALSE)
  }
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata)))) {
    stop("metadata must be a named list", call. = FALSE)
  }
  x <- structure(list(
    schema_version = "1.0", id = id, version = version, scale = scale,
    source_type = source_type, filename = filename, checksum = tolower(checksum),
    source = source, organism = as.character(organism)[1L],
    analyses = sort(unique(tolower(as.character(analyses)))),
    required_software = sort(unique(as.character(required_software))),
    estimated_runtime_seconds = as.numeric(estimated_runtime_seconds),
    estimated_memory_mb = as.numeric(estimated_memory_mb),
    published = isTRUE(published), metadata = metadata
  ), class = "PopgenVCFBenchmarkDatasetEntry")
  validate_benchmark_dataset_entry(x)
}

#' Validate a benchmark dataset catalogue entry
#' @param x A `PopgenVCFBenchmarkDatasetEntry`.
#' @return `x`, invisibly.
#' @export
validate_benchmark_dataset_entry <- function(x) {
  if (!inherits(x, "PopgenVCFBenchmarkDatasetEntry")) {
    stop("x must be a PopgenVCFBenchmarkDatasetEntry", call. = FALSE)
  }
  if (!identical(x$schema_version, "1.0")) stop("unsupported dataset entry schema", call. = FALSE)
  benchmark_dataset_id(x$id)
  benchmark_dataset_id(x$version, "version")
  if (!x$source_type %in% c("embedded", "local", "remote")) stop("invalid source type", call. = FALSE)
  if (!nzchar(x$filename) || basename(x$filename) != x$filename) stop("filename must be a basename", call. = FALSE)
  invisible(x)
}

#' Create and modify a benchmark dataset catalogue
#' @param entries Optional list of entries.
#' @return A `PopgenVCFBenchmarkDatasetCatalogue`.
#' @export
new_benchmark_dataset_catalogue <- function(entries = list()) {
  x <- structure(list(entries = list()), class = "PopgenVCFBenchmarkDatasetCatalogue")
  for (entry in entries) x <- register_benchmark_dataset(x, entry)
  x
}

#' @param catalogue A benchmark dataset catalogue.
#' @param entry A dataset entry.
#' @rdname new_benchmark_dataset_catalogue
#' @export
register_benchmark_dataset <- function(catalogue, entry) {
  if (!inherits(catalogue, "PopgenVCFBenchmarkDatasetCatalogue")) stop("catalogue is invalid", call. = FALSE)
  validate_benchmark_dataset_entry(entry)
  key <- paste(entry$id, entry$version, sep = "@")
  if (key %in% names(catalogue$entries)) stop("duplicate dataset entry: ", key, call. = FALSE)
  catalogue$entries[[key]] <- entry
  catalogue
}

#' List benchmark datasets
#' @param catalogue A benchmark dataset catalogue.
#' @param scale,organism,analysis,source_type Optional filters.
#' @return A data table.
#' @noRd
list_benchmark_datasets <- function(catalogue, scale = NULL, organism = NULL,
                                    analysis = NULL, source_type = NULL) {
  if (!inherits(catalogue, "PopgenVCFBenchmarkDatasetCatalogue")) stop("catalogue is invalid", call. = FALSE)
  rows <- lapply(catalogue$entries, function(x) data.table::data.table(
    id = x$id, version = x$version, scale = x$scale,
    source_type = x$source_type, filename = x$filename,
    organism = x$organism, analyses = paste(x$analyses, collapse = ","),
    required_software = paste(x$required_software, collapse = ","),
    estimated_runtime_seconds = x$estimated_runtime_seconds,
    estimated_memory_mb = x$estimated_memory_mb,
    published = x$published
  ))
  tab <- data.table::rbindlist(rows, fill = TRUE)
  if (!is.null(scale)) tab <- tab[scale %in% as.character(scale)]
  if (!is.null(organism)) tab <- tab[tolower(organism) %in% tolower(as.character(organism))]
  if (!is.null(analysis)) {
    requested <- tolower(as.character(analysis))
    tab <- tab[vapply(strsplit(analyses, ",", fixed = TRUE), function(z) any(z %in% requested), logical(1L))]
  }
  if (!is.null(source_type)) tab <- tab[source_type %in% as.character(source_type)]
  tab[]
}

#' Return the benchmark dataset cache root
#' @param cache_dir Optional explicit cache directory.
#' @return Normalized cache directory path.
#' @export
benchmark_dataset_cache_dir <- function(cache_dir = NULL) {
  root <- cache_dir %||% Sys.getenv("POPGENVCF_BENCHMARK_CACHE", unset = "")
  if (!nzchar(root)) root <- tools::R_user_dir("popgenVCF", which = "cache")
  normalizePath(root, mustWork = FALSE)
}

benchmark_dataset_cache_path <- function(entry, cache_dir = NULL) {
  validate_benchmark_dataset_entry(entry)
  file.path(benchmark_dataset_cache_dir(cache_dir), entry$id, entry$version, entry$filename)
}

#' Verify a cached benchmark dataset
#' @param path Dataset file path.
#' @param checksum Expected SHA256 checksum, or `NA` to skip verification.
#' @return `TRUE` when valid.
#' @export
verify_benchmark_dataset <- function(path, checksum = NA_character_) {
  if (!file.exists(path)) return(FALSE)
  if (is.na(checksum) || !nzchar(checksum)) return(TRUE)
  identical(tolower(digest::digest(path, algo = "sha256", file = TRUE)), tolower(checksum))
}

benchmark_materialize_embedded <- function(entry, destination) {
  value <- entry$source(destination)
  path <- if (is.character(value) && length(value) == 1L) value else destination
  if (!file.exists(path)) stop("embedded materializer did not create a dataset file", call. = FALSE)
  if (!identical(normalizePath(path), normalizePath(destination, mustWork = FALSE))) {
    if (!file.copy(path, destination, overwrite = TRUE)) stop("failed to copy embedded dataset", call. = FALSE)
  }
  destination
}

#' Resolve a benchmark dataset into the local cache
#'
#' @param entry A dataset entry.
#' @param cache_dir Optional cache root.
#' @param offline Do not access remote sources.
#' @param force Refresh even when a valid cached file exists.
#' @param quiet Suppress download progress.
#' @return Path to the verified cached file.
#' @export
resolve_benchmark_dataset <- function(entry, cache_dir = NULL, offline = FALSE,
                                      force = FALSE, quiet = TRUE) {
  validate_benchmark_dataset_entry(entry)
  destination <- benchmark_dataset_cache_path(entry, cache_dir)
  if (!force && verify_benchmark_dataset(destination, entry$checksum)) return(destination)
  if (file.exists(destination)) unlink(destination)
  if (isTRUE(offline) && entry$source_type == "remote") {
    stop("dataset is not available in the cache and offline mode is enabled: ", entry$id, call. = FALSE)
  }
  if (entry$source_type == "remote" && !entry$published) {
    stop("remote benchmark dataset is catalogued but not yet published: ", entry$id, call. = FALSE)
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(entry$filename, "."), tmpdir = dirname(destination))
  on.exit(unlink(temporary), add = TRUE)
  if (entry$source_type == "embedded") {
    benchmark_materialize_embedded(entry, temporary)
  } else if (entry$source_type == "local") {
    if (!file.exists(entry$source)) stop("local benchmark dataset does not exist: ", entry$source, call. = FALSE)
    if (!file.copy(entry$source, temporary, overwrite = TRUE)) stop("failed to copy local benchmark dataset", call. = FALSE)
  } else {
    status <- tryCatch(utils::download.file(entry$source, temporary, mode = "wb", quiet = quiet), error = identity)
    if (inherits(status, "error") || !identical(as.integer(status), 0L)) {
      message <- if (inherits(status, "error")) conditionMessage(status) else paste("status", status)
      stop("failed to download benchmark dataset '", entry$id, "': ", message, call. = FALSE)
    }
  }
  if (!verify_benchmark_dataset(temporary, entry$checksum)) {
    stop("SHA256 verification failed for benchmark dataset: ", entry$id, call. = FALSE)
  }
  if (!file.rename(temporary, destination)) {
    if (!file.copy(temporary, destination, overwrite = TRUE)) stop("failed to install benchmark dataset in cache", call. = FALSE)
  }
  destination
}

#' Convert a catalogue entry to a benchmark dataset
#' @param entry A dataset entry.
#' @param cache_dir,offline Cache resolution options.
#' @param reader Function reading the resolved file.
#' @return A `PopgenVCFBenchmarkDataset`.
#' @export
benchmark_dataset_from_entry <- function(entry, cache_dir = NULL, offline = FALSE,
                                         reader = function(path) readRDS(path)) {
  validate_benchmark_dataset_entry(entry)
  if (!is.function(reader)) stop("reader must be a function", call. = FALSE)
  new_benchmark_dataset(
    id = entry$id, scale = entry$scale,
    loader = function() reader(resolve_benchmark_dataset(entry, cache_dir, offline)),
    source = paste(entry$source_type, entry$id, entry$version, sep = ":"),
    checksum = entry$checksum,
    metadata = c(entry$metadata, list(version = entry$version, organism = entry$organism,
                                      analyses = entry$analyses))
  )
}

embedded_tiny_benchmark_entry <- function() {
  payload <- list(
    genotype = matrix(c(0L, 1L, 2L, 0L, 1L, 2L, 2L, 1L, 0L), nrow = 3L,
                      dimnames = list(paste0("s", 1:3), paste0("v", 1:3))),
    population = c("A", "A", "B")
  )
  temporary <- tempfile(fileext = ".rds")
  saveRDS(payload, temporary, version = 3)
  checksum <- digest::digest(temporary, algo = "sha256", file = TRUE)
  unlink(temporary)
  new_benchmark_dataset_entry(
    id = "synthetic_tiny", version = "1", scale = "tiny",
    source_type = "embedded", filename = "synthetic_tiny.rds", checksum = checksum,
    source = function(path) { saveRDS(payload, path, version = 3); path },
    organism = "synthetic", analyses = c("qc", "pca", "ibs", "diversity", "fst"),
    estimated_runtime_seconds = 1, estimated_memory_mb = 32,
    metadata = list(samples = 3L, variants = 3L)
  )
}

#' Default benchmark dataset catalogue
#' @return A catalogue containing CI fixtures and planned curated references.
#' @export
default_benchmark_dataset_catalogue <- function() {
  planned <- function(id, label) new_benchmark_dataset_entry(
    id = id, version = "1", scale = "real", source_type = "remote",
    filename = paste0(id, ".vcf.gz"), checksum = NA_character_,
    source = paste0("https://github.com/duceppemo/popgenVCF/releases/download/benchmark-data-v1/", id, ".vcf.gz"),
    organism = "Homo sapiens", analyses = c("qc", "pca", "ibs", "fst", "ancestry"),
    required_software = c("bcftools", "plink2"), published = FALSE,
    metadata = list(description = label)
  )
  new_benchmark_dataset_catalogue(list(
    embedded_tiny_benchmark_entry(),
    planned("1000g_subset", "Curated 1000 Genomes subset"),
    planned("hgdp_subset", "Curated HGDP subset"),
    planned("hapmap_subset", "Curated HapMap subset")
  ))
}
