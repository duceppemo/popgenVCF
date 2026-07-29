#' Create a reproducible popgenVCF analysis project
#'
#' @param name Human-readable project name.
#' @param results Named list of canonical analysis results.
#' @param inputs Data frame of input records or named character paths.
#' @param parameters,modules,artifacts,reports,provenance Named project components.
#' @param rng RNG metadata from `new_project_rng()`.
#' @param project_id Stable UUID; generated when omitted.
#' @param created_at UTC timestamp.
#' @param package_version,git_sha Software identity.
#' @return A validated `PopgenVCFProject`.
#' @export
new_popgenvcf_project <- function(
    name, results = list(), inputs = data.table::data.table(), parameters = list(),
    modules = list(), artifacts = list(), reports = list(), provenance = list(),
    rng = new_project_rng(), project_id = project_uuid(),
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = tryCatch(as.character(utils::packageVersion("popgenVCF")),
                               error = function(e) NA_character_),
    git_sha = Sys.getenv("GITHUB_SHA", unset = NA_character_)) {
  # Preserve character vectors so the original constructor can convert named
  # input paths into checksummed records. Only a genuinely empty object is
  # replaced by the typed zero-row manifest.
  if (length(inputs) == 0L) {
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
