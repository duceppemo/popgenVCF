fair_scalar <- function(x, label, allow_na = FALSE) {
  if (!is.character(x) || length(x) != 1L || (!allow_na && (is.na(x) || !nzchar(x)))) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  as.character(x)[1L]
}

fair_orcid <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) return(NA_character_)
  value <- sub("^https?://orcid\\.org/", "", trimws(as.character(x)[1L]))
  if (!grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", value)) {
    stop("ORCID must use the 0000-0000-0000-0000 form", call. = FALSE)
  }
  value
}

#' Create a FAIR creator record
#'
#' @param name Creator name.
#' @param orcid Optional ORCID identifier.
#' @param affiliation Optional affiliation.
#' @param email Optional email address.
#' @return A plain validated creator record.
#' @export
new_fair_creator <- function(name, orcid = NA_character_, affiliation = NA_character_,
                             email = NA_character_) {
  list(
    name = fair_scalar(name, "name"),
    orcid = fair_orcid(orcid),
    affiliation = as.character(affiliation)[1L],
    email = as.character(email)[1L]
  )
}

fair_project_urn <- function(project) {
  validate_popgenvcf_project(project)
  paste0("urn:popgenvcf:project:", tolower(project$project_id))
}

#' Return stable FAIR identifiers
#'
#' @param project A `PopgenVCFProject`.
#' @param artifact_id Optional artifact identifier.
#' @return A stable URN.
#' @export
fair_identifier <- function(project, artifact_id = NULL) {
  root <- fair_project_urn(project)
  if (is.null(artifact_id)) return(root)
  artifact_id <- fair_scalar(artifact_id, "artifact_id")
  paste0(root, ":artifact:", substr(digest::digest(artifact_id, algo = "sha256"), 1L, 24L))
}

fair_lineage <- function(project) {
  lineage <- project$artifacts$artifact_lineage %||% project$provenance$artifact_lineage
  if (is.null(lineage)) return(NULL)
  validate_artifact_lineage(lineage)
  lineage
}

fair_artifact_records <- function(project) {
  lineage <- fair_lineage(project)
  if (is.null(lineage)) return(data.table::data.table(
    id = character(), urn = character(), name = character(), type = character(),
    format = character(), sha256 = character(), size_bytes = numeric(),
    path = character(), producer = character(), consumers = character()))
  tab <- lineage_artifact_table(lineage)
  tab[, urn := vapply(id, function(z) fair_identifier(project, z), character(1L))]
  data.table::setcolorder(tab, c("id", "urn", setdiff(names(tab), c("id", "urn"))))
  tab[]
}

#' Create FAIR metadata for a reproducible project
#'
#' @param project A `PopgenVCFProject`.
#' @param title,description Human-readable metadata.
#' @param creators List of records from `new_fair_creator()`.
#' @param license SPDX identifier or license URL for the analysis project.
#' @param keywords Character keywords.
#' @param publisher Optional publisher or repository.
#' @param publication_year Publication year.
#' @param rights_uri Optional explicit license URI.
#' @return A validated `PopgenVCFFAIRMetadata`.
#' @export
new_fair_metadata <- function(
    project, title = project$name, description = "Reproducible population-genomics analysis",
    creators = list(), license = "MIT", keywords = c("population genetics", "VCF", "reproducibility"),
    publisher = "popgenVCF", publication_year = as.integer(format(Sys.Date(), "%Y")),
    rights_uri = NA_character_) {
  validate_popgenvcf_project(project)
  if (!is.list(creators)) stop("creators must be a list", call. = FALSE)
  if (length(creators) && !all(vapply(creators, function(x) is.list(x) && !is.null(x$name), logical(1L)))) {
    stop("creators contains an invalid creator", call. = FALSE)
  }
  artifacts <- fair_artifact_records(project)
  metadata <- structure(list(
    schema_version = "1.0",
    identifier = fair_project_urn(project),
    project_id = project$project_id,
    title = fair_scalar(title, "title"),
    description = fair_scalar(description, "description"),
    creators = creators,
    license = fair_scalar(license, "license"),
    rights_uri = as.character(rights_uri)[1L],
    keywords = unique(as.character(keywords)),
    publisher = fair_scalar(publisher, "publisher"),
    publication_year = as.integer(publication_year)[1L],
    created_at = project$created_at,
    package_version = project$package_version,
    git_sha = project$git_sha,
    project_urn = fair_project_urn(project),
    artifacts = artifacts,
    inputs = data.table::copy(project$inputs),
    lineage_digest = if (is.null(fair_lineage(project))) NA_character_ else fair_lineage(project)$digest
  ), class = "PopgenVCFFAIRMetadata")
  validate_fair_metadata(metadata)
  metadata
}

#' Validate FAIR metadata
#'
#' @param x A `PopgenVCFFAIRMetadata`.
#' @return `x`, invisibly.
#' @export
validate_fair_metadata <- function(x) {
  if (!inherits(x, "PopgenVCFFAIRMetadata")) stop("x must be PopgenVCFFAIRMetadata", call. = FALSE)
  fair_scalar(x$identifier, "identifier")
  fair_scalar(x$title, "title")
  fair_scalar(x$description, "description")
  fair_scalar(x$license, "license")
  if (!grepl("^urn:popgenvcf:project:", x$identifier)) stop("FAIR project identifier is invalid", call. = FALSE)
  if (is.na(x$publication_year) || x$publication_year < 1900L) stop("publication_year is invalid", call. = FALSE)
  if (length(x$creators)) invisible(lapply(x$creators, function(z) {
    fair_scalar(z$name, "creator name")
    fair_orcid(z$orcid)
  }))
  artifacts <- data.table::as.data.table(x$artifacts)
  if (nrow(artifacts)) {
    if (anyDuplicated(artifacts$id) || anyDuplicated(artifacts$urn)) stop("FAIR artifact identifiers must be unique", call. = FALSE)
    if (any(!grepl("^[0-9a-f]{64}$", artifacts$sha256))) stop("FAIR artifact SHA256 is invalid", call. = FALSE)
  }
  invisible(x)
}

fair_creator_jsonld <- function(x) {
  id <- if (is.na(x$orcid)) paste0("#person-", substr(digest::digest(x$name, algo = "sha256"), 1L, 12L)) else paste0("https://orcid.org/", x$orcid)
  out <- list(`@id` = id, `@type` = "Person", name = x$name)
  if (!is.na(x$affiliation) && nzchar(x$affiliation)) out$affiliation <- x$affiliation
  if (!is.na(x$email) && nzchar(x$email)) out$email <- x$email
  out
}

#' Build standards-facing FAIR documents
#'
#' @param metadata FAIR metadata.
#' @return A plain named list containing RO-Crate, CodeMeta, DataCite, and CFF records.
#' @export
fair_documents <- function(metadata) {
  validate_fair_metadata(metadata)
  creators <- lapply(metadata$creators, fair_creator_jsonld)
  creator_refs <- lapply(creators, function(x) list(`@id` = x$`@id`))
  artifact_entities <- lapply(seq_len(nrow(metadata$artifacts)), function(i) {
    z <- metadata$artifacts[i]
    list(`@id` = z$urn[[1L]], `@type` = "File", name = z$name[[1L]],
         encodingFormat = z$format[[1L]], sha256 = z$sha256[[1L]],
         contentSize = z$size_bytes[[1L]], isPartOf = list(`@id` = metadata$identifier))
  })
  dataset <- list(
    `@id` = metadata$identifier, `@type` = "Dataset", name = metadata$title,
    description = metadata$description, datePublished = as.character(metadata$publication_year),
    license = metadata$rights_uri %||% metadata$license, keywords = metadata$keywords,
    creator = creator_refs, hasPart = lapply(artifact_entities, function(x) list(`@id` = x$`@id`)),
    softwareRequirements = list(`@id` = "https://github.com/duceppemo/popgenVCF")
  )
  ro_crate <- list(
    `@context` = "https://w3id.org/ro/crate/1.1/context",
    `@graph` = c(list(
      list(`@id` = "ro-crate-metadata.json", `@type` = "CreativeWork", about = list(`@id` = metadata$identifier),
           conformsTo = list(`@id` = "https://w3id.org/ro/crate/1.1")),
      dataset,
      list(`@id` = "https://github.com/duceppemo/popgenVCF", `@type` = "SoftwareApplication",
           name = "popgenVCF", version = metadata$package_version,
           codeRepository = "https://github.com/duceppemo/popgenVCF", license = "MIT")
    ), creators, artifact_entities)
  )
  codemeta <- list(
    `@context` = "https://doi.org/10.5063/schema/codemeta-2.0",
    `@type` = "SoftwareSourceCode", name = "popgenVCF", version = metadata$package_version,
    codeRepository = "https://github.com/duceppemo/popgenVCF", license = "https://spdx.org/licenses/MIT.html",
    programmingLanguage = "R", author = creators,
    keywords = c("population genetics", "population genomics", "VCF", "bioinformatics")
  )
  datacite_creators <- lapply(metadata$creators, function(x) {
    out <- list(name = x$name, nameType = "Personal")
    if (!is.na(x$orcid)) out$nameIdentifiers <- list(list(
      nameIdentifier = paste0("https://orcid.org/", x$orcid),
      nameIdentifierScheme = "ORCID", schemeUri = "https://orcid.org"))
    if (!is.na(x$affiliation) && nzchar(x$affiliation)) out$affiliation <- list(list(name = x$affiliation))
    out
  })
  datacite <- list(data = list(type = "dois", attributes = list(
    titles = list(list(title = metadata$title)), creators = datacite_creators,
    publisher = metadata$publisher, publicationYear = metadata$publication_year,
    types = list(resourceTypeGeneral = "Dataset", resourceType = "Reproducible analysis project"),
    descriptions = list(list(description = metadata$description, descriptionType = "Abstract")),
    subjects = lapply(metadata$keywords, function(x) list(subject = x)),
    rightsList = list(list(rights = metadata$license, rightsUri = metadata$rights_uri)),
    alternateIdentifiers = list(list(alternateIdentifier = metadata$identifier,
                                     alternateIdentifierType = "URN"))
  )))
  cff_authors <- lapply(metadata$creators, function(x) {
    parts <- strsplit(x$name, "[[:space:]]+")[[1L]]
    out <- list(`family-names` = tail(parts, 1L), `given-names` = paste(head(parts, -1L), collapse = " "))
    if (!is.na(x$orcid)) out$orcid <- paste0("https://orcid.org/", x$orcid)
    if (!is.na(x$affiliation) && nzchar(x$affiliation)) out$affiliation <- x$affiliation
    out
  })
  cff <- list(`cff-version` = "1.2.0", message = "Please cite this reproducible popgenVCF analysis project.",
              title = metadata$title, type = "dataset", version = metadata$package_version,
              authors = cff_authors, license = metadata$license,
              `date-released` = paste0(metadata$publication_year, "-01-01"),
              identifiers = list(list(type = "other", value = metadata$identifier, description = "popgenVCF project URN")))
  list(ro_crate = ro_crate, codemeta = codemeta, datacite = datacite, citation_cff = cff)
}

cff_quote <- function(x) paste0('"', gsub('"', '\\"', as.character(x), fixed = TRUE), '"')
cff_lines <- function(x, indent = 0L) {
  prefix <- paste(rep(" ", indent), collapse = "")
  if (is.list(x) && is.null(names(x))) {
    return(unlist(lapply(x, function(z) {
      if (is.list(z)) c(paste0(prefix, "-"), cff_lines(z, indent + 2L)) else paste0(prefix, "- ", cff_quote(z))
    }), use.names = FALSE))
  }
  unlist(Map(function(nm, z) {
    if (is.list(z)) c(paste0(prefix, nm, ":"), cff_lines(z, indent + 2L))
    else paste0(prefix, nm, ": ", cff_quote(z))
  }, names(x), x), use.names = FALSE)
}

#' Write and validate a FAIR research-object directory
#'
#' @param metadata FAIR metadata.
#' @param directory Destination directory.
#' @param overwrite Permit replacement.
#' @return Named paths, invisibly.
#' @export
write_fair_bundle <- function(metadata, directory, overwrite = FALSE) {
  validate_fair_metadata(metadata)
  if (dir.exists(directory) && length(list.files(directory, all.files = TRUE, no.. = TRUE)) && !isTRUE(overwrite)) {
    stop("FAIR bundle directory is not empty", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  docs <- fair_documents(metadata)
  paths <- c(ro_crate = file.path(directory, "ro-crate-metadata.json"),
             codemeta = file.path(directory, "codemeta.json"),
             datacite = file.path(directory, "datacite.json"),
             citation = file.path(directory, "CITATION.cff"),
             metadata = file.path(directory, "fair-metadata.rds"))
  jsonlite::write_json(docs$ro_crate, paths[["ro_crate"]], pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  jsonlite::write_json(docs$codemeta, paths[["codemeta"]], pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  jsonlite::write_json(docs$datacite, paths[["datacite"]], pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  writeLines(cff_lines(docs$citation_cff), paths[["citation"]])
  saveRDS(metadata, paths[["metadata"]], version = 3)
  files <- unname(paths)
  manifest <- data.table::data.table(
    path = basename(files), size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE))
  manifest_path <- file.path(directory, "fair-manifest.tsv")
  data.table::fwrite(manifest, manifest_path, sep = "\t")
  paths <- c(paths, manifest = manifest_path)
  validate_fair_bundle(directory)
  invisible(normalizePath(paths, winslash = "/", mustWork = TRUE))
}

#' Validate a FAIR research-object directory
#'
#' @param directory FAIR directory.
#' @return `TRUE`, or an error.
#' @export
validate_fair_bundle <- function(directory) {
  required <- c("ro-crate-metadata.json", "codemeta.json", "datacite.json",
                "CITATION.cff", "fair-metadata.rds", "fair-manifest.tsv")
  missing <- required[!file.exists(file.path(directory, required))]
  if (length(missing)) stop("FAIR bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  manifest <- data.table::fread(file.path(directory, "fair-manifest.tsv"))
  for (i in seq_len(nrow(manifest))) {
    path <- file.path(directory, manifest$path[[i]])
    if (!file.exists(path)) stop("FAIR file is missing: ", manifest$path[[i]], call. = FALSE)
    actual <- digest::digest(path, algo = "sha256", file = TRUE)
    if (!identical(actual, manifest$sha256[[i]])) stop("FAIR checksum mismatch: ", manifest$path[[i]], call. = FALSE)
  }
  metadata <- readRDS(file.path(directory, "fair-metadata.rds"))
  validate_fair_metadata(metadata)
  invisible(lapply(c("ro-crate-metadata.json", "codemeta.json", "datacite.json"), function(path) {
    jsonlite::read_json(file.path(directory, path), simplifyVector = FALSE)
  }))
  TRUE
}

#' Attach FAIR metadata to a project
#'
#' @param project A reproducible project.
#' @param metadata FAIR metadata for the same project.
#' @return Updated project.
#' @export
set_project_fair_metadata <- function(project, metadata) {
  validate_popgenvcf_project(project)
  validate_fair_metadata(metadata)
  if (!identical(metadata$project_id, project$project_id)) stop("FAIR metadata belongs to another project", call. = FALSE)
  project$provenance$fair <- list(identifier = metadata$identifier,
                                  license = metadata$license,
                                  creators = metadata$creators,
                                  lineage_digest = metadata$lineage_digest)
  project$artifacts$fair_metadata <- metadata
  project$component_digests$fair_metadata <- digest::digest(metadata, algo = "sha256", serialize = TRUE)
  project
}
