# Keep catalogue filter arguments out of data.table's column lookup scope.
list_benchmark_datasets <- function(catalogue, scale = NULL, organism = NULL,
                                    analysis = NULL, source_type = NULL) {
  if (!inherits(catalogue, "PopgenVCFBenchmarkDatasetCatalogue")) {
    stop("catalogue is invalid", call. = FALSE)
  }

  rows <- lapply(catalogue$entries, function(x) data.table::data.table(
    id = x$id,
    version = x$version,
    scale = x$scale,
    source_type = x$source_type,
    filename = x$filename,
    organism = x$organism,
    analyses = paste(x$analyses, collapse = ","),
    required_software = paste(x$required_software, collapse = ","),
    estimated_runtime_seconds = x$estimated_runtime_seconds,
    estimated_memory_mb = x$estimated_memory_mb,
    published = x$published
  ))
  tab <- data.table::rbindlist(rows, fill = TRUE)

  requested_scale <- if (is.null(scale)) NULL else as.character(scale)
  requested_organism <- if (is.null(organism)) NULL else tolower(as.character(organism))
  requested_analysis <- if (is.null(analysis)) NULL else tolower(as.character(analysis))
  requested_source_type <- if (is.null(source_type)) NULL else as.character(source_type)

  if (!is.null(requested_scale)) {
    tab <- tab[tab$scale %in% requested_scale]
  }
  if (!is.null(requested_organism)) {
    tab <- tab[tolower(tab$organism) %in% requested_organism]
  }
  if (!is.null(requested_analysis)) {
    keep <- vapply(
      strsplit(tab$analyses, ",", fixed = TRUE),
      function(values) any(values %in% requested_analysis),
      logical(1L)
    )
    tab <- tab[keep]
  }
  if (!is.null(requested_source_type)) {
    tab <- tab[tab$source_type %in% requested_source_type]
  }

  tab[]
}
