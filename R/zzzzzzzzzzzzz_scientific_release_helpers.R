# Loaded after the scientific release API so dependency normalization supports
# both named atomic vectors and named lists while preserving canonical ordering.
scientific_release_dependencies <- function(x) {
  if (is.null(x)) {
    return(data.frame(package = character(), version = character(), stringsAsFactors = FALSE))
  }
  if (!is.data.frame(x) && !is.null(names(x))) {
    x <- data.frame(
      package = names(x),
      version = unlist(x, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!all(c("package", "version") %in% names(x))) {
    stop("dependencies must contain package and version", call. = FALSE)
  }
  x <- x[, c("package", "version"), drop = FALSE]
  x$package <- trimws(as.character(x$package))
  x$version <- trimws(as.character(x$version))
  if (any(!nzchar(x$package)) || any(!nzchar(x$version)) || anyDuplicated(x$package)) {
    stop("dependencies require unique non-empty package names and versions", call. = FALSE)
  }
  x <- x[order(x$package), , drop = FALSE]
  rownames(x) <- NULL
  x
}
