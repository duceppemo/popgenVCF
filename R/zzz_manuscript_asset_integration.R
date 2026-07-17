manuscript_anchor_id <- function(category, id) {
  value <- paste(category, id, sep = "-")
  value <- gsub("[^A-Za-z0-9_-]+", "-", value)
  value <- gsub("-+", "-", value)
  value <- gsub("^-|-$", "", value)
  tolower(value)
}

manuscript_artifact_value <- function(tab, column, i, default = NA_character_) {
  if (!column %in% names(tab)) return(default)
  value <- tab[[column]][[i]]
  if (is.null(value) || !length(value) || is.na(value) || !nzchar(as.character(value))) return(default)
  as.character(value)
}

manuscript_bibliography_text <- function(bibliography) {
  if (is.null(bibliography) || !length(bibliography)) return(character())
  if (is.character(bibliography)) return(as.character(bibliography))
  if (is.data.frame(bibliography) && "bibtex" %in% names(bibliography)) {
    return(as.character(bibliography$bibtex))
  }
  if (is.list(bibliography) && !is.null(bibliography$bibtex)) {
    return(as.character(bibliography$bibtex))
  }
  character()
}

#' Build the manuscript artifact cross-reference table
#'
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @return A deterministic data table linking manuscript labels, anchors, and
#'   immutable artifact identifiers.
#' @export
manuscript_cross_reference_table <- function(manuscript) {
  validate_manuscript(manuscript)
  artifacts <- data.table::copy(manuscript$artifacts)
  if (!nrow(artifacts)) {
    return(data.table::data.table(
      id = character(), category = character(), label = character(),
      anchor = character(), path = character(), caption = character(),
      embeddable = logical()
    ))
  }
  captions <- data.table::copy(manuscript$captions)
  caption_by_id <- setNames(captions$caption, captions$id)
  label_by_id <- setNames(captions$label, captions$id)
  rows <- lapply(seq_len(nrow(artifacts)), function(i) {
    id <- manuscript_artifact_value(artifacts, "id", i, paste0("artifact_", i))
    category <- manuscript_artifact_value(artifacts, "category", i, "supplementary")
    path <- manuscript_artifact_value(artifacts, "destination", i,
      manuscript_artifact_value(artifacts, "path", i, ""))
    extension <- tolower(tools::file_ext(path))
    data.table::data.table(
      id = id,
      category = category,
      label = label_by_id[[id]] %||% id,
      anchor = manuscript_anchor_id(category, id),
      path = path,
      caption = caption_by_id[[id]] %||% manuscript_artifact_value(artifacts, "name", i, id),
      embeddable = identical(category, "figure") && extension %in% c("png", "jpg", "jpeg", "gif", "svg", "webp")
    )
  })
  result <- data.table::rbindlist(rows, fill = TRUE)
  data.table::setorderv(result, c("category", "label", "id"))
  result[]
}

manuscript_render_reference <- function(row) {
  anchor <- paste0("<a id=\"", row$anchor, "\"></a>")
  target <- if (nzchar(row$path)) row$path else "missing-artifact"
  if (isTRUE(row$embeddable)) {
    return(c(anchor, paste0("![", row$caption, "](", target, ")"),
             paste0("**", row$label, ".** ", row$caption, " [artifact `", row$id, "`].")))
  }
  c(anchor, paste0("- **", row$label, ".** [", row$caption, "](", target,
                   ") [artifact `", row$id, "`]."))
}

manuscript_reference_section <- function(manuscript, category) {
  refs <- manuscript_cross_reference_table(manuscript)
  refs <- refs[category == ..category]
  if (!nrow(refs)) return("None recorded.")
  unlist(lapply(seq_len(nrow(refs)), function(i) manuscript_render_reference(refs[i])), use.names = FALSE)
}

manuscript_copy_assets <- function(manuscript, directory) {
  artifacts <- data.table::copy(manuscript$artifacts)
  if (!nrow(artifacts)) return(artifacts)
  if (!"destination" %in% names(artifacts)) artifacts[, destination := NA_character_]
  for (i in seq_len(nrow(artifacts))) {
    source <- manuscript_artifact_value(artifacts, "path", i, "")
    if (!nzchar(source) || !file.exists(source)) next
    category <- manuscript_artifact_value(artifacts, "category", i, "supplementary")
    folder <- switch(category, figure = "figures", table = "tables", "supplementary")
    safe <- gsub("[^A-Za-z0-9._-]+", "_", basename(source))
    id <- manuscript_artifact_value(artifacts, "id", i, paste0("artifact_", i))
    prefix <- substr(digest::digest(id, algo = "sha256", serialize = FALSE), 1L, 12L)
    relative <- file.path("assets", folder, paste0(prefix, "_", safe))
    target <- file.path(directory, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, target, overwrite = TRUE, copy.mode = TRUE)) {
      stop("failed to copy manuscript artifact: ", source, call. = FALSE)
    }
    artifacts$destination[[i]] <- gsub("\\\\", "/", relative)
  }
  artifacts
}

render_manuscript_markdown <- function(manuscript) {
  validate_manuscript(manuscript)
  keywords <- if (length(manuscript$keywords)) paste(manuscript$keywords, collapse = "; ") else "[Keywords required.]"
  declarations <- manuscript$declarations
  bibliography <- manuscript_bibliography_text(manuscript$bibliography)
  reference_note <- if (length(bibliography)) {
    "BibTeX entries are preserved in `references.bib`; citation-style rendering is deferred to the CSL phase."
  } else {
    "No BibTeX bibliography was embedded. Citation-style rendering is deferred to the CSL phase."
  }
  c(
    paste0("# ", manuscript$title), "",
    "## Authors", "", manuscript_author_lines(manuscript$authors), "",
    "## Abstract", "", manuscript$abstract, "",
    paste0("**Keywords:** ", keywords), "",
    "## Introduction", "", manuscript$introduction, "",
    "## Methods", "", manuscript$methods, "",
    "## Results", "", manuscript$results, "",
    "### Figures", "", manuscript_reference_section(manuscript, "figure"), "",
    "### Tables", "", manuscript_reference_section(manuscript, "table"), "",
    "## Discussion", "", manuscript$discussion, "",
    "## Data availability", "", declarations$data_availability, "",
    "## Software availability", "", declarations$software_availability, "",
    "## Reproducibility statement", "", declarations$reproducibility, "",
    "## Funding", "", declarations$funding, "",
    "## Author contributions", "", declarations$author_contributions, "",
    "## Competing interests", "", declarations$competing_interests, "",
    "## Supplementary materials", "", manuscript_reference_section(manuscript, "supplementary"), "",
    "## References", "", reference_note, "",
    "---", "",
    paste0("Generated from popgenVCF project `", manuscript$project_id, "`."),
    paste0("Project digest: `", manuscript$project_digest, "`."),
    paste0("Publication digest: `", manuscript$publication_digest, "`.")
  )
}

write_manuscript <- function(manuscript, directory, overwrite = FALSE) {
  validate_manuscript(manuscript)
  if (dir.exists(directory) && length(list.files(directory, all.files = TRUE, no.. = TRUE)) && !isTRUE(overwrite)) {
    stop("manuscript directory is not empty", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  written <- manuscript
  written$artifacts <- manuscript_copy_assets(manuscript, directory)
  cross_references <- manuscript_cross_reference_table(written)
  writeLines(render_manuscript_markdown(written), file.path(directory, "manuscript.md"), useBytes = TRUE)
  saveRDS(written, file.path(directory, "manuscript.rds"), version = 3)
  data.table::fwrite(written$authors, file.path(directory, "authors.tsv"), sep = "\t")
  data.table::fwrite(written$captions, file.path(directory, "captions.tsv"), sep = "\t")
  data.table::fwrite(cross_references, file.path(directory, "cross-references.tsv"), sep = "\t")
  bibliography <- manuscript_bibliography_text(written$bibliography)
  if (length(bibliography)) writeLines(bibliography, file.path(directory, "references.bib"), useBytes = TRUE)
  files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  files <- files[basename(files) != "manuscript-manifest.tsv"]
  manifest <- data.table::data.table(
    path = substring(normalizePath(files, winslash = "/"), nchar(normalizePath(directory, winslash = "/")) + 2L),
    size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::setorderv(manifest, "path")
  data.table::fwrite(manifest, file.path(directory, "manuscript-manifest.tsv"), sep = "\t")
  validate_manuscript(directory)
  invisible(normalizePath(directory, winslash = "/"))
}
