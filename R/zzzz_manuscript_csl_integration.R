manuscript_csl_key_pattern <- "^[A-Za-z0-9_:.+/-]+$"

#' Create a manuscript citation profile
#'
#' @param style_id Stable citation-style identity.
#' @param csl Optional path to a Citation Style Language file.
#' @return A validated `PopgenVCFCitationProfile`.
#' @export
new_citation_profile <- function(style_id = "generic", csl = NULL) {
  style_id <- trimws(as.character(style_id)[1L])
  if (!nzchar(style_id)) stop("citation style identity must be non-empty", call. = FALSE)
  csl_path <- NULL
  csl_sha256 <- NA_character_
  csl_name <- NA_character_
  if (!is.null(csl)) {
    csl_path <- normalizePath(as.character(csl)[1L], winslash = "/", mustWork = TRUE)
    if (!identical(tolower(tools::file_ext(csl_path)), "csl")) {
      stop("citation style file must use the .csl extension", call. = FALSE)
    }
    csl_sha256 <- digest::digest(csl_path, algo = "sha256", file = TRUE)
    csl_name <- basename(csl_path)
  }
  profile <- structure(list(
    schema_version = "1.0",
    style_id = style_id,
    csl_path = csl_path,
    csl_name = csl_name,
    csl_sha256 = csl_sha256,
    bundle_path = if (is.null(csl_path)) NA_character_ else "citation-style.csl"
  ), class = "PopgenVCFCitationProfile")
  validate_citation_profile(profile)
  profile
}

#' Validate a manuscript citation profile
#'
#' @param x A `PopgenVCFCitationProfile`.
#' @return `TRUE` invisibly, or an error.
#' @export
validate_citation_profile <- function(x) {
  if (!inherits(x, "PopgenVCFCitationProfile")) {
    stop("x must be a PopgenVCFCitationProfile", call. = FALSE)
  }
  if (!identical(x$schema_version, "1.0")) stop("unsupported citation profile schema version", call. = FALSE)
  if (!is.character(x$style_id) || length(x$style_id) != 1L || !nzchar(x$style_id)) {
    stop("citation style identity is invalid", call. = FALSE)
  }
  if (!is.null(x$csl_path)) {
    if (!file.exists(x$csl_path)) stop("citation style file does not exist", call. = FALSE)
    actual <- digest::digest(x$csl_path, algo = "sha256", file = TRUE)
    if (!identical(actual, x$csl_sha256)) stop("citation style checksum mismatch", call. = FALSE)
  }
  invisible(TRUE)
}

#' Attach a citation profile to a manuscript
#'
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @param profile A validated `PopgenVCFCitationProfile`.
#' @return An updated manuscript object.
#' @export
set_manuscript_citation_profile <- function(manuscript, profile) {
  validate_manuscript(manuscript)
  validate_citation_profile(profile)
  updated <- manuscript
  updated$citation_profile <- profile
  updated$citation_profile_digest <- digest::digest(profile, algo = "sha256", serialize = TRUE)
  validate_manuscript(updated)
  updated
}

#' Extract canonical BibTeX citation keys
#'
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @return A sorted character vector of unique citation keys.
#' @export
manuscript_citation_keys <- function(manuscript) {
  validate_manuscript(manuscript)
  bib <- manuscript_bibliography_text(manuscript$bibliography)
  if (!length(bib)) return(character())
  text <- paste(bib, collapse = "\n")
  matches <- gregexpr("@[A-Za-z]+\\s*\\{\\s*([^,[:space:]]+)", text, perl = TRUE)
  tokens <- regmatches(text, matches)[[1L]]
  if (!length(tokens)) return(character())
  keys <- sub("^@[A-Za-z]+\\s*\\{\\s*", "", tokens, perl = TRUE)
  if (any(!grepl(manuscript_csl_key_pattern, keys))) {
    stop("bibliography contains an invalid citation key", call. = FALSE)
  }
  sort(unique(keys))
}

manuscript_citation_manifest <- function(manuscript) {
  keys <- manuscript_citation_keys(manuscript)
  profile <- manuscript$citation_profile %||% new_citation_profile()
  data.table::data.table(
    citation_key = keys,
    bibliography_file = rep("references.bib", length(keys)),
    style_id = rep(profile$style_id, length(keys)),
    csl_file = rep(if (is.null(profile$csl_path)) NA_character_ else profile$bundle_path, length(keys)),
    csl_sha256 = rep(profile$csl_sha256, length(keys))
  )
}

manuscript_yaml_quote <- function(x) {
  paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
}

manuscript_yaml_front_matter <- function(manuscript) {
  bibliography <- manuscript_bibliography_text(manuscript$bibliography)
  profile <- manuscript$citation_profile %||% new_citation_profile()
  lines <- c("---", paste0("title: ", manuscript_yaml_quote(manuscript$title)))
  if (length(bibliography)) lines <- c(lines, "bibliography: references.bib")
  if (!is.null(profile$csl_path)) lines <- c(lines, paste0("csl: ", profile$bundle_path))
  c(lines, "link-citations: true", "---", "")
}

manuscript_copy_csl <- function(profile, directory) {
  if (is.null(profile$csl_path)) return(profile)
  target <- file.path(directory, profile$bundle_path)
  if (!file.copy(profile$csl_path, target, overwrite = TRUE, copy.mode = TRUE)) {
    stop("failed to copy citation style file", call. = FALSE)
  }
  actual <- digest::digest(target, algo = "sha256", file = TRUE)
  if (!identical(actual, profile$csl_sha256)) stop("copied citation style checksum mismatch", call. = FALSE)
  profile
}

render_manuscript_markdown <- function(manuscript) {
  validate_manuscript(manuscript)
  keywords <- if (length(manuscript$keywords)) paste(manuscript$keywords, collapse = "; ") else "[Keywords required.]"
  declarations <- manuscript$declarations
  bibliography <- manuscript_bibliography_text(manuscript$bibliography)
  profile <- manuscript$citation_profile %||% new_citation_profile()
  reference_note <- if (length(bibliography)) {
    paste0("Canonical BibTeX entries are preserved in `references.bib` using citation profile `", profile$style_id, "`.")
  } else {
    paste0("No BibTeX bibliography was embedded. Citation profile `", profile$style_id, "` is recorded for later rendering.")
  }
  c(
    manuscript_yaml_front_matter(manuscript),
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
  profile <- written$citation_profile %||% new_citation_profile()
  written$citation_profile <- manuscript_copy_csl(profile, directory)
  cross_references <- manuscript_cross_reference_table(written)
  citation_manifest <- manuscript_citation_manifest(written)
  writeLines(render_manuscript_markdown(written), file.path(directory, "manuscript.md"), useBytes = TRUE)
  saveRDS(written, file.path(directory, "manuscript.rds"), version = 3)
  data.table::fwrite(written$authors, file.path(directory, "authors.tsv"), sep = "\t")
  data.table::fwrite(written$captions, file.path(directory, "captions.tsv"), sep = "\t")
  data.table::fwrite(cross_references, file.path(directory, "cross-references.tsv"), sep = "\t")
  data.table::fwrite(citation_manifest, file.path(directory, "citation-manifest.tsv"), sep = "\t")
  jsonlite::write_json(unclass(written$citation_profile), file.path(directory, "citation-profile.json"),
                       pretty = TRUE, auto_unbox = TRUE, null = "null")
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
