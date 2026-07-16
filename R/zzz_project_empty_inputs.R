# Compatibility wrapper loaded after projects.R. It preserves the public
# constructor signature while supplying a typed zero-row input manifest.
.project_new_popgenvcf_project <- new_popgenvcf_project

new_popgenvcf_project <- function(
    name, results = list(), inputs = data.table::data.table(), parameters = list(),
    modules = list(), artifacts = list(), reports = list(), provenance = list(),
    rng = new_project_rng(), project_id = project_uuid(),
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = tryCatch(as.character(utils::packageVersion("popgenVCF")),
                               error = function(e) NA_character_),
    git_sha = Sys.getenv("GITHUB_SHA", unset = NA_character_)) {
  inputs <- data.table::as.data.table(inputs)
  if (!nrow(inputs) && !ncol(inputs)) {
    inputs <- data.table::data.table(
      role = character(),
      path = character(),
      exists = logical(),
      size_bytes = numeric(),
      sha256 = character()
    )
  }

  .project_new_popgenvcf_project(
    name = name,
    results = results,
    inputs = inputs,
    parameters = parameters,
    modules = modules,
    artifacts = artifacts,
    reports = reports,
    provenance = provenance,
    rng = rng,
    project_id = project_id,
    created_at = created_at,
    package_version = package_version,
    git_sha = git_sha
  )
}
