publication_style_profiles <- function() {
  list(
    generic = list(name = "Generic", figure_prefix = "Figure", table_prefix = "Table",
                   supplementary_prefix = "Supplementary", citation_style = "author-year"),
    nature = list(name = "Nature", figure_prefix = "Fig.", table_prefix = "Table",
                  supplementary_prefix = "Supplementary", citation_style = "numbered"),
    molecular_ecology = list(name = "Molecular Ecology", figure_prefix = "Figure",
                             table_prefix = "Table", supplementary_prefix = "Appendix",
                             citation_style = "author-year"),
    g3 = list(name = "G3", figure_prefix = "Figure", table_prefix = "Table",
              supplementary_prefix = "File S", citation_style = "author-year"),
    bmc = list(name = "BMC", figure_prefix = "Fig.", table_prefix = "Table",
               supplementary_prefix = "Additional file", citation_style = "numbered"),
    plos = list(name = "PLOS", figure_prefix = "Fig", table_prefix = "Table",
                supplementary_prefix = "S", citation_style = "numbered")
  )
}

#' Return a publication style profile
#'
#' @param style Style identifier or a custom profile list.
#' @return A validated plain publication style profile.
#' @export
publication_style <- function(style = "generic") {
  if (is.list(style)) {
    required <- c("name", "figure_prefix", "table_prefix", "supplementary_prefix", "citation_style")
    missing <- setdiff(required, names(style))
    if (length(missing)) stop("publication style is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    return(style)
  }
  key <- gsub("[^a-z0-9]+", "_", tolower(as.character(style)[1L]))
  profiles <- publication_style_profiles()
  if (!key %in% names(profiles)) stop("unknown publication style: ", style, call. = FALSE)
  profiles[[key]]
}

publication_plain <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0L) return(NA_character_)
  if (is.atomic(x) && length(x) == 1L) return(as.character(x))
  paste(capture.output(str(x, give.attr = FALSE, vec.len = 8L)), collapse = " ")
}

publication_parameter_table <- function(project) {
  params <- project$parameters %||% list()
  if (!length(params)) return(data.table::data.table(parameter = character(), value = character()))
  data.table::data.table(
    parameter = names(params) %||% paste0("parameter_", seq_along(params)),
    value = vapply(params, publication_plain, character(1L))
  )
}

publication_module_table <- function(project) {
  modules <- project$modules %||% list()
  if (!length(modules)) return(data.table::data.table(module = character(), status = character(), detail = character()))
  rows <- lapply(seq_along(modules), function(i) {
    z <- modules[[i]]
    data.table::data.table(
      module = names(modules)[[i]] %||% z$id %||% paste0("module_", i),
      status = z$status %||% "recorded",
      detail = publication_plain(z$reason %||% z$method %||% z$backend %||% NA_character_)
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

publication_software_table <- function(project) {
  base <- data.table::data.table(
    software = c("popgenVCF", "R"),
    version = c(project$package_version %||% as.character(utils::packageVersion("popgenVCF")),
                project$runtime$r_version %||% R.version.string),
    role = c("analysis platform", "runtime")
  )
  provenance <- project$provenance$software %||% project$software %||% list()
  if (!length(provenance)) return(base)
  extra <- data.table::rbindlist(lapply(names(provenance), function(nm) {
    data.table::data.table(software = nm, version = publication_plain(provenance[[nm]]), role = "dependency")
  }), fill = TRUE)
  unique(data.table::rbindlist(list(base, extra), fill = TRUE), by = c("software", "version", "role"))
}

publication_artifact_table <- function(project) {
  lineage <- project$artifacts$artifact_lineage %||% project$provenance$artifact_lineage
  if (!is.null(lineage)) {
    validate_artifact_lineage(lineage)
    tab <- lineage_artifact_table(lineage)
    tab[, category := data.table::fcase(
      grepl("figure|plot|image", type, ignore.case = TRUE) | grepl("pdf|svg|png|jpeg|jpg", format, ignore.case = TRUE), "figure",
      grepl("table|data", type, ignore.case = TRUE) | grepl("tsv|csv|xlsx", format, ignore.case = TRUE), "table",
      default = "supplementary")]
    return(tab[])
  }
  manifest <- project$artifacts$manifest %||% project$artifacts$artifact_manifest
  if (inherits(manifest, "PopgenVCFArtifactManifest")) {
    tab <- artifact_manifest_table(manifest)
    tab[, category := data.table::fcase(
      grepl("figure|plot", type, ignore.case = TRUE), "figure",
      grepl("table|data", type, ignore.case = TRUE), "table",
      default = "supplementary")]
    return(tab[])
  }
  data.table::data.table(id = character(), name = character(), type = character(), format = character(),
                         path = character(), sha256 = character(), size_bytes = numeric(), category = character())
}

publication_methods_text <- function(project, modules, parameters) {
  intro <- paste0("Analyses were conducted with popgenVCF ", project$package_version,
                  " using the reproducible project identifier ", project$project_id, ".")
  module_text <- if (!nrow(modules)) {
    "No module execution records were embedded in the project."
  } else {
    paste0("Recorded analysis modules were: ", paste(modules$module, collapse = ", "), ".")
  }
  parameter_text <- if (!nrow(parameters)) {
    "No project-level parameter records were available."
  } else {
    paste0("Project parameters were preserved in parameters.tsv (", nrow(parameters), " entries).")
  }
  paste(intro, module_text, parameter_text, sep = "\n\n")
}

publication_caption_table <- function(artifacts, style) {
  if (!nrow(artifacts)) return(data.table::data.table(id = character(), category = character(), label = character(), caption = character()))
  artifacts <- data.table::copy(artifacts)
  artifacts[, sequence := seq_len(.N), by = category]
  artifacts[, label := data.table::fcase(
    category == "figure", paste(style$figure_prefix, sequence),
    category == "table", paste(style$table_prefix, sequence),
    default = paste(style$supplementary_prefix, sequence))]
  artifacts[, caption := paste0(label, ". ", ifelse(is.na(name) | !nzchar(name), id, name),
                              ". Generated from the immutable popgenVCF artifact record `", id, "`.")]
  artifacts[, .(id, category, label, caption)]
}

#' Validate a publication companion plan or written directory
#'
#' @param x A publication bundle plan or directory path.
#' @return `TRUE` invisibly, or an error.
#' @export
validate_publication_bundle <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("manuscript/methods.md", "manuscript/software.tsv", "manuscript/parameters.tsv",
                  "manuscript/captions.tsv", "publication-manifest.tsv", "publication-bundle.rds")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("publication bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "publication-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      if (!file.exists(path)) stop("publication file is missing: ", manifest$path[[i]], call. = FALSE)
      actual <- digest::digest(path, algo = "sha256", file = TRUE)
      if (!identical(actual, manifest$sha256[[i]])) stop("publication checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFPublicationBundle")) stop("x must be a PopgenVCFPublicationBundle or directory", call. = FALSE)
  if (!is.character(x$title) || length(x$title) != 1L || !nzchar(x$title)) stop("publication title is invalid", call. = FALSE)
  if (!is.list(x$style) || is.null(x$style$name)) stop("publication style is invalid", call. = FALSE)
  if (anyDuplicated(x$artifacts$id)) stop("publication artifact identifiers must be unique", call. = FALSE)
  invisible(TRUE)
}

publication_copy_artifacts <- function(bundle, directory) {
  tab <- data.table::copy(bundle$artifacts)
  if (!nrow(tab)) return(tab)
  tab[, destination := NA_character_]
  for (i in seq_len(nrow(tab))) {
    source <- tab$path[[i]]
    if (is.na(source) || !nzchar(source) || !file.exists(source)) next
    folder <- switch(tab$category[[i]], figure = "figures", table = "tables", "supplementary")
    safe <- gsub("[^A-Za-z0-9._-]+", "_", basename(source))
    destination <- file.path(directory, folder, paste0(sprintf("%03d", i), "_", safe))
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE)) stop("failed to copy publication artifact: ", source, call. = FALSE)
    tab$destination[[i]] <- substring(normalizePath(destination, winslash = "/"), nchar(normalizePath(directory, winslash = "/")) + 2L)
  }
  tab
}

#' Attach a publication plan to a project
#'
#' @param project A reproducible project.
#' @param bundle A `PopgenVCFPublicationBundle`.
#' @return Updated project.
#' @export
set_project_publication_bundle <- function(project, bundle) {
  validate_popgenvcf_project(project)
  validate_publication_bundle(bundle)
  if (!identical(project$project_id, bundle$project_id)) stop("publication bundle belongs to another project", call. = FALSE)
  project$artifacts$publication_bundle <- bundle
  project$provenance$publication <- list(title = bundle$title, style = bundle$style$name,
                                         project_digest = bundle$project_digest)
  project$component_digests$publication_bundle <- digest::digest(bundle, algo = "sha256", serialize = TRUE)
  project
}
