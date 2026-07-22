# Apply the repository software-identity contract after the FAIR document
# implementation is loaded. Analysis-project creators remain authoritative for
# DataCite and dataset CFF records; CodeMeta and the RO-Crate software entity
# describe popgenVCF itself.
.fair_documents_without_software_identity <- fair_documents

fair_documents <- function(metadata) {
  docs <- .fair_documents_without_software_identity(metadata)
  identity <- popgenvcf_software_identity()

  graph <- docs$ro_crate$`@graph`
  software_index <- which(vapply(graph, function(entity) {
    is.list(entity) && identical(entity$`@id`, identity$repository)
  }, logical(1L)))
  if (length(software_index) != 1L) {
    stop("RO-Crate must contain exactly one canonical popgenVCF software entity", call. = FALSE)
  }
  graph[[software_index]] <- list(
    `@id` = identity$repository,
    `@type` = "SoftwareApplication",
    name = identity$name,
    headline = identity$title,
    description = identity$description,
    version = metadata$package_version,
    url = identity$documentation,
    codeRepository = identity$repository,
    issueTracker = identity$issue_tracker,
    license = identity$license$url,
    applicationCategory = identity$application_category,
    programmingLanguage = identity$programming_language,
    softwareRequirements = identity$runtime_platform
  )
  docs$ro_crate$`@graph` <- graph

  software_author <- list(
    `@type` = "Person",
    givenName = identity$author$given_name,
    familyName = identity$author$family_name,
    email = identity$author$email
  )
  docs$codemeta <- list(
    `@context` = "https://doi.org/10.5063/schema/codemeta-2.0",
    `@type` = "SoftwareSourceCode",
    name = identity$name,
    version = metadata$package_version,
    description = identity$description,
    identifier = identity$repository,
    url = identity$documentation,
    codeRepository = identity$repository,
    issueTracker = identity$issue_tracker,
    downloadUrl = identity$release_archive,
    license = identity$license$url,
    programmingLanguage = identity$programming_language,
    runtimePlatform = identity$runtime_platform,
    applicationCategory = identity$application_category,
    developmentStatus = identity$development_status,
    keywords = identity$keywords,
    author = list(software_author),
    maintainer = software_author
  )

  docs
}
