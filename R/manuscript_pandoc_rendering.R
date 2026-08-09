pandoc_status <- function(executable = Sys.which("pandoc")) {
  executable <- unname(as.character(executable)[1L])
  available <- nzchar(executable) && file.exists(executable)
  version <- NA_character_
  if (available) {
    out <- suppressWarnings(system2(executable, "--version", stdout = TRUE, stderr = TRUE))
    if (length(out)) version <- trimws(out[[1L]])
  }
  list(available = available, executable = if (available) normalizePath(executable, winslash = "/") else NA_character_, version = version)
}

pandoc_render_arguments <- function(manuscript_directory, format = c("html", "docx"), output = NULL) {
  format <- match.arg(format)
  validate_manuscript(manuscript_directory)
  source <- file.path(manuscript_directory, "manuscript.md")
  extension <- if (identical(format, "html")) "html" else "docx"
  if (is.null(output)) output <- file.path(manuscript_directory, "rendered", paste0("manuscript.", extension))
  args <- c("--from=markdown", paste0("--to=", format), "--citeproc", source, paste0("--output=", output))
  args
}

validate_manuscript_render <- function(x) {
  if (!inherits(x, "PopgenVCFManuscriptRender")) stop("x must be a PopgenVCFManuscriptRender", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported manuscript render schema version", call. = FALSE)
  if (!x$dry_run && !identical(x$status, 0L)) stop("Pandoc rendering failed", call. = FALSE)
  if (!x$dry_run) {
    if (!file.exists(x$output)) stop("rendered manuscript output is missing", call. = FALSE)
    actual <- digest::digest(x$output, algo = "sha256", file = TRUE)
    if (!identical(actual, x$output_sha256)) stop("rendered manuscript checksum mismatch", call. = FALSE)
  }
  invisible(TRUE)
}

render_manuscript <- function(manuscript_directory, format = c("html", "docx"), pandoc = Sys.which("pandoc"), dry_run = FALSE, overwrite = FALSE) {
  format <- match.arg(format)
  validate_manuscript(manuscript_directory)
  status <- pandoc_status(pandoc)
  output <- file.path(manuscript_directory, "rendered", paste0("manuscript.", if (format == "html") "html" else "docx"))
  args <- pandoc_render_arguments(manuscript_directory, format, output)
  if (file.exists(output) && !isTRUE(overwrite) && !isTRUE(dry_run)) stop("rendered manuscript output already exists", call. = FALSE)
  if (!isTRUE(dry_run) && !status$available) stop("Pandoc is not available", call. = FALSE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  stdout_file <- file.path(dirname(output), paste0("pandoc-", format, ".stdout.log"))
  stderr_file <- file.path(dirname(output), paste0("pandoc-", format, ".stderr.log"))
  exit_status <- NA_integer_
  if (!isTRUE(dry_run)) {
    exit_status <- system2(status$executable, args, stdout = stdout_file, stderr = stderr_file)
  }
  record <- structure(list(
    schema_version = "1.0", format = format, manuscript_directory = normalizePath(manuscript_directory, winslash = "/"),
    pandoc = status, arguments = args, output = normalizePath(output, winslash = "/", mustWork = FALSE),
    stdout = normalizePath(stdout_file, winslash = "/", mustWork = FALSE), stderr = normalizePath(stderr_file, winslash = "/", mustWork = FALSE),
    status = if (isTRUE(dry_run)) NA_integer_ else as.integer(exit_status), dry_run = isTRUE(dry_run),
    output_sha256 = if (!isTRUE(dry_run) && file.exists(output)) digest::digest(output, algo = "sha256", file = TRUE) else NA_character_
  ), class = "PopgenVCFManuscriptRender")
  if (!isTRUE(dry_run)) {
    jsonlite::write_json(unclass(record), file.path(dirname(output), paste0("render-", format, ".json")), pretty = TRUE, auto_unbox = TRUE, null = "null")
    validate_manuscript_render(record)
  }
  record
}
