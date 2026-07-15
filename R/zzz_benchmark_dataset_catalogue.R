# Dataset catalogue filtering is kept separate from cache resolution so filter
# arguments never collide with data.table column names.
#' @export
list_benchmark_datasets <- function(catalogue, scale = NULL, organism = NULL,
                                    analysis = NULL, source_type = NULL) {
  if (!inherits(catalogue, "PopgenVCFBenchmarkDatasetCatalogue")) {
    stop("catalogue is invalid", call. = FALSE)
  }
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
  if (!nrow(tab)) return(tab)
  if (!is.null(scale)) tab <- tab[tab$scale %in% as.character(scale), ]
  if (!is.null(organism)) tab <- tab[tolower(tab$organism) %in% tolower(as.character(organism)), ]
  if (!is.null(analysis)) {
    requested <- tolower(as.character(analysis))
    keep <- vapply(strsplit(tab$analyses, ",", fixed = TRUE), function(z) any(z %in% requested), logical(1L))
    tab <- tab[keep, ]
  }
  if (!is.null(source_type)) tab <- tab[tab$source_type %in% as.character(source_type), ]
  tab[]
}
