publication_analysis_methods_text <- function(project, modules, parameters, inventory) {
  base <- publication_methods_text(project, modules, parameters)
  present <- inventory[state %in% c("present", "diagnostic-only")]
  if (!nrow(present)) return(base)
  sections <- paste0("## ", toupper(substring(present$kind, 1L, 1L)), substring(present$kind, 2L), "\n\n", present$method)
  paste(c(base, sections), collapse = "\n\n")
}

publication_analysis_caption_table <- function(artifacts, style, inventory) {
  publication_validate_caption_ownership(artifacts, inventory)
  captions <- publication_caption_table(artifacts, style)
  if (!nrow(captions)) return(captions)
  active <- inventory[state %in% c("present", "diagnostic-only")]
  for (i in seq_len(nrow(captions))) {
    id <- tolower(captions$id[[i]])
    matches <- which(vapply(seq_len(nrow(active)), function(j) {
      grepl(active$kind[[j]], id, fixed = TRUE) || grepl(tolower(active$analysis[[j]]), id, fixed = TRUE)
    }, logical(1L)))
    if (length(matches) == 1L) captions$caption[[i]] <- paste0(captions$label[[i]], ". ", active$legend[[matches]])
  }
  captions
}

#' Create a publication companion plan
#'
#' @param project A `PopgenVCFProject`.
#' @param style Publication style identifier or custom profile.
#' @param title Optional manuscript title.
#' @return A validated `PopgenVCFPublicationBundle` plan.
#' @export
new_publication_bundle <- function(project, style = "generic", title = project$name) {
  validate_popgenvcf_project(project)
  style <- publication_style(style)
  artifacts <- publication_artifact_table(project)
  parameters <- publication_parameter_table(project)
  modules <- publication_module_table(project)
  software <- publication_software_table(project)
  inventory <- publication_narrative_inventory(project)
  completeness <- publication_narrative_completeness(inventory)
  bundle <- structure(list(
    schema_version = "1.2", project_id = project$project_id,
    title = as.character(title)[1L], style = style,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    methods = publication_analysis_methods_text(project, modules, parameters, inventory),
    parameters = parameters, modules = modules, software = software,
    analyses = inventory[state %in% c("present", "diagnostic-only")],
    narrative_inventory = inventory, narrative_completeness = completeness,
    supplementary_summaries = inventory[, .(analysis, kind, state, supplementary_summary)],
    artifacts = artifacts,
    captions = publication_analysis_caption_table(artifacts, style, inventory),
    project_digest = digest::digest(project$component_digests, algo = "sha256", serialize = TRUE)
  ), class = "PopgenVCFPublicationBundle")
  validate_publication_bundle(bundle)
  bundle
}

#' Generate a publication companion directory
#'
#' @param project A `PopgenVCFProject`.
#' @param directory Output directory.
#' @param style Publication style identifier or profile.
#' @param title Optional manuscript title.
#' @param include_project Include a portable `.popgenvcf` project bundle.
#' @param include_fair Include a FAIR subdirectory when FAIR metadata is embedded.
#' @param overwrite Permit replacement of a non-empty directory.
#' @return Normalized output directory, invisibly.
#' @export
generate_publication_bundle <- function(project, directory, style = "generic", title = project$name,
                                        include_project = TRUE, include_fair = TRUE, overwrite = FALSE) {
  validate_popgenvcf_project(project)
  if (dir.exists(directory) && length(list.files(directory, all.files = TRUE, no.. = TRUE)) && !isTRUE(overwrite)) {
    stop("publication directory is not empty", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  invisible(lapply(c("manuscript", "figures", "tables", "supplementary", "provenance", "FAIR"),
                   function(z) dir.create(file.path(directory, z), recursive = TRUE, showWarnings = FALSE)))
  bundle <- new_publication_bundle(project, style = style, title = title)
  copied <- publication_copy_artifacts(bundle, directory)
  bundle$artifacts <- copied
  writeLines(c(paste0("# ", bundle$title), "", bundle$methods), file.path(directory, "manuscript", "methods.md"))
  data.table::fwrite(bundle$software, file.path(directory, "manuscript", "software.tsv"), sep = "\t")
  data.table::fwrite(bundle$parameters, file.path(directory, "manuscript", "parameters.tsv"), sep = "\t")
  data.table::fwrite(bundle$modules, file.path(directory, "manuscript", "modules.tsv"), sep = "\t")
  data.table::fwrite(bundle$analyses, file.path(directory, "manuscript", "analysis-narratives.tsv"), sep = "\t")
  data.table::fwrite(bundle$narrative_inventory, file.path(directory, "manuscript", "narrative-inventory.tsv"), sep = "\t")
  data.table::fwrite(bundle$narrative_completeness, file.path(directory, "manuscript", "narrative-completeness.tsv"), sep = "\t")
  data.table::fwrite(bundle$supplementary_summaries, file.path(directory, "supplementary", "analysis-summaries.tsv"), sep = "\t")
  data.table::fwrite(bundle$captions, file.path(directory, "manuscript", "captions.tsv"), sep = "\t")
  write_publication_bibliography(bundle$analyses, file.path(directory, "manuscript", "references.bib"))
  data.table::fwrite(copied, file.path(directory, "provenance", "artifacts.tsv"), sep = "\t")
  jsonlite::write_json(list(project_id = bundle$project_id, project_digest = bundle$project_digest,
                            style = bundle$style, created_at = bundle$created_at,
                            completeness = bundle$narrative_completeness,
                            inventory = bundle$narrative_inventory[, .(analysis, kind, state, reason)]),
                       file.path(directory, "provenance", "publication.json"), pretty = TRUE, auto_unbox = TRUE)
  if (isTRUE(include_project)) write_popgenvcf_project(project, file.path(directory, "supplementary", "analysis.popgenvcf"), overwrite = TRUE)
  fair <- project$artifacts$fair_metadata
  if (isTRUE(include_fair) && inherits(fair, "PopgenVCFFAIRMetadata")) write_fair_bundle(fair, file.path(directory, "FAIR"), overwrite = TRUE)
  saveRDS(bundle, file.path(directory, "publication-bundle.rds"), version = 3)
  files <- list.files(directory, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[basename(files) != "publication-manifest.tsv"]
  root <- normalizePath(directory, winslash = "/")
  manifest <- data.table::data.table(
    path = substring(normalizePath(files, winslash = "/"), nchar(root) + 2L),
    size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::setorder(manifest, path)
  data.table::fwrite(manifest, file.path(directory, "publication-manifest.tsv"), sep = "\t")
  validate_publication_bundle(directory)
  invisible(normalizePath(directory, winslash = "/", mustWork = TRUE))
}
