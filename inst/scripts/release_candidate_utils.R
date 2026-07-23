rc_need <- function() {
  pkgs <- c("data.table", "digest", "jsonlite")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(miss)) stop("Missing required packages: ", paste(miss, collapse = ", "), call. = FALSE)
}
rc_scalar <- function(x, label) {
  x <- as.character(unlist(x, use.names = FALSE))
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    stop(label, " must be one non-empty string", call. = FALSE)
  trimws(x)
}
rc_bool <- function(x, label) {
  x <- as.logical(unlist(x, use.names = FALSE))
  if (length(x) != 1L || is.na(x)) stop(label, " must be logical", call. = FALSE)
  x
}
rc_sha <- function(x, label = "sha256") {
  x <- tolower(sub("^sha256:", "", rc_scalar(x, label)))
  if (!grepl("^[0-9a-f]{64}$", x)) stop(label, " must be a SHA-256 digest", call. = FALSE)
  x
}
rc_datetime <- function(x, label) {
  x <- rc_scalar(x, label)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", x))
    stop(label, " must use YYYY-MM-DDTHH:MM:SSZ", call. = FALSE)
  x
}
rc_date <- function(x, label) {
  x <- rc_scalar(x, label)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))
    stop(label, " must use YYYY-MM-DD", call. = FALSE)
  x
}

