#' Create a deterministic graphical abstract specification
#'
#' @param manuscript A canonical `PopgenVCFManuscript` object.
#' @param panels A list of panel records. Each record must contain `artifact_id`
#'   and `path`; optional fields are `label`, `role`, and `caption`.
#' @param title Author-supplied graphical abstract title.
#' @param message Author-supplied central message.
#' @param alt_text Author-supplied accessibility description.
#' @param width_px,height_px Target pixel dimensions.
#' @param resolution_dpi Target resolution in dots per inch.
#' @param orientation One of `landscape`, `portrait`, or `square`.
#' @param background Background description, such as `white` or `transparent`.
#' @return A `PopgenVCFGraphicalAbstract` object.
#' @export
new_graphical_abstract <- function(manuscript, panels = list(), title = NULL,
                                   message = NULL, alt_text = NULL,
                                   width_px = 1800L, height_px = 1000L,
                                   resolution_dpi = 300L,
                                   orientation = c("landscape", "portrait", "square"),
                                   background = "white") {
  validate_manuscript(manuscript)
  orientation <- match.arg(orientation)
  stopifnot(length(width_px) == 1L, width_px > 0L,
            length(height_px) == 1L, height_px > 0L,
            length(resolution_dpi) == 1L, resolution_dpi > 0L)

  normalize_text <- function(x) if (is.null(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
  normalize_panel <- function(x, i) {
    if (!is.list(x) || is.null(x$artifact_id) || is.null(x$path)) {
      stop("Each panel must provide artifact_id and path", call. = FALSE)
    }
    path <- normalizePath(x$path, winslash = "/", mustWork = TRUE)
    list(
      order = i,
      artifact_id = as.character(x$artifact_id),
      path = path,
      filename = basename(path),
      label = normalize_text(x$label),
      role = normalize_text(x$role),
      caption = normalize_text(x$caption),
      sha256 = unname(digest::digest(file = path, algo = "sha256"))
    )
  }
  normalized_panels <- Map(normalize_panel, panels, seq_along(panels))

  payload <- list(
    manuscript_id = manuscript$id,
    title = normalize_text(title),
    message = normalize_text(message),
    alt_text = normalize_text(alt_text),
    dimensions = list(width_px = as.integer(width_px), height_px = as.integer(height_px),
                      resolution_dpi = as.integer(resolution_dpi), orientation = orientation,
                      background = as.character(background)),
    panels = normalized_panels
  )
  payload$id <- paste0("graphical-abstract:", digest::digest(payload, algo = "sha256", serialize = TRUE))
  class(payload) <- c("PopgenVCFGraphicalAbstract", "list")
  validate_graphical_abstract(payload, strict = FALSE)
  payload
}

#' Validate a graphical abstract specification
#'
#' @param x A `PopgenVCFGraphicalAbstract` object.
#' @param strict Whether missing author-supplied content is an error.
#' @return `TRUE` invisibly.
#' @export
validate_graphical_abstract <- function(x, strict = TRUE) {
  if (!inherits(x, "PopgenVCFGraphicalAbstract")) stop("x is not a graphical abstract specification", call. = FALSE)
  required <- c("manuscript_id", "title", "message", "alt_text", "dimensions", "panels", "id")
  if (!all(required %in% names(x))) stop("Malformed graphical abstract specification", call. = FALSE)
  ids <- vapply(x$panels, `[[`, character(1), "artifact_id")
  if (anyDuplicated(ids)) stop("Panel artifact identities must be unique", call. = FALSE)
  for (panel in x$panels) {
    if (!file.exists(panel$path)) stop("Graphical abstract panel file is missing: ", panel$path, call. = FALSE)
    actual <- unname(digest::digest(file = panel$path, algo = "sha256"))
    if (!identical(actual, panel$sha256)) stop("Graphical abstract panel checksum mismatch: ", panel$path, call. = FALSE)
  }
  if (isTRUE(strict)) {
    missing <- c(title = is.na(x$title), message = is.na(x$message), alt_text = is.na(x$alt_text), panels = length(x$panels) == 0L)
    if (any(missing)) stop("Incomplete graphical abstract specification: ", paste(names(missing)[missing], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Write a graphical abstract specification bundle
#'
#' @param graphical_abstract A `PopgenVCFGraphicalAbstract` object.
#' @param directory Parent output directory.
#' @param overwrite Whether an existing output directory may be replaced.
#' @param strict Whether incomplete author-supplied content is an error.
#' @return The normalized output directory invisibly.
#' @export
write_graphical_abstract <- function(graphical_abstract, directory, overwrite = FALSE, strict = FALSE) {
  validate_graphical_abstract(graphical_abstract, strict = strict)
  out <- file.path(directory, "graphical-abstract")
  if (dir.exists(out)) {
    if (!isTRUE(overwrite)) stop("Graphical abstract directory already exists", call. = FALSE)
    unlink(out, recursive = TRUE, force = TRUE)
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  panel_rows <- lapply(graphical_abstract$panels, function(x) {
    data.frame(order = x$order, artifact_id = x$artifact_id, filename = x$filename,
               role = ifelse(is.na(x$role), "", x$role),
               label = ifelse(is.na(x$label), "", x$label),
               caption = ifelse(is.na(x$caption), "", x$caption),
               sha256 = x$sha256, stringsAsFactors = FALSE)
  })
  manifest <- if (length(panel_rows)) do.call(rbind, panel_rows) else data.frame(
    order = integer(), artifact_id = character(), filename = character(), role = character(),
    label = character(), caption = character(), sha256 = character())
  utils::write.table(manifest, file.path(out, "graphical-abstract-manifest.tsv"),
                     sep = "\t", row.names = FALSE, quote = TRUE, na = "")

  record <- unclass(graphical_abstract)
  jsonlite::write_json(record, file.path(out, "graphical-abstract-record.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
  brief <- c(
    "# Graphical abstract assembly brief", "",
    paste0("- Specification ID: `", graphical_abstract$id, "`"),
    paste0("- Manuscript ID: `", graphical_abstract$manuscript_id, "`"),
    paste0("- Title: ", ifelse(is.na(graphical_abstract$title), "[AUTHOR INPUT REQUIRED]", graphical_abstract$title)),
    paste0("- Central message: ", ifelse(is.na(graphical_abstract$message), "[AUTHOR INPUT REQUIRED]", graphical_abstract$message)),
    paste0("- Accessibility text: ", ifelse(is.na(graphical_abstract$alt_text), "[AUTHOR INPUT REQUIRED]", graphical_abstract$alt_text)),
    paste0("- Canvas: ", graphical_abstract$dimensions$width_px, " x ", graphical_abstract$dimensions$height_px,
           " px at ", graphical_abstract$dimensions$resolution_dpi, " dpi"),
    paste0("- Orientation: ", graphical_abstract$dimensions$orientation),
    paste0("- Background: ", graphical_abstract$dimensions$background), "",
    "## Ordered panels", ""
  )
  if (!length(graphical_abstract$panels)) brief <- c(brief, "[AUTHOR ASSET SELECTION REQUIRED]")
  for (panel in graphical_abstract$panels) {
    brief <- c(brief, paste0(panel$order, ". `", panel$artifact_id, "` - ", panel$filename))
  }
  writeLines(brief, file.path(out, "graphical-abstract-brief.md"), useBytes = TRUE)
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}
