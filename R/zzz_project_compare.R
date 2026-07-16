# Loaded after projects.R to make result-set comparisons safe when a result is
# present in only one project.
compare_popgenvcf_projects <- function(current, baseline) {
  if (is.character(current)) current <- read_popgenvcf_project(current)
  if (is.character(baseline)) baseline <- read_popgenvcf_project(baseline)
  validate_popgenvcf_project(current)
  validate_popgenvcf_project(baseline)

  scalar <- function(field) data.table::data.table(
    category = "identity",
    item = field,
    baseline = as.character(baseline[[field]]),
    current = as.character(current[[field]]),
    changed = !identical(baseline[[field]], current[[field]])
  )

  rows <- lapply(c("project_id", "name", "package_version", "git_sha"), scalar)
  keys <- union(names(baseline$results), names(current$results))

  digest_or_na <- function(x, id) {
    value <- unname(x[id])
    if (!length(value) || is.na(value)) NA_character_ else value[[1L]]
  }

  rows[[length(rows) + 1L]] <- data.table::rbindlist(lapply(keys, function(id) {
    b <- digest_or_na(baseline$component_digests$results, id)
    c <- digest_or_na(current$component_digests$results, id)
    data.table::data.table(
      category = "result",
      item = id,
      baseline = b,
      current = c,
      changed = !identical(b, c)
    )
  }), fill = TRUE)

  input_key <- function(tab) paste(tab$role, tab$path, tab$sha256, sep = "|")
  rows[[length(rows) + 1L]] <- data.table::data.table(
    category = "inputs",
    item = "input_set",
    baseline = digest::digest(sort(input_key(baseline$inputs)), algo = "sha256"),
    current = digest::digest(sort(input_key(current$inputs)), algo = "sha256"),
    changed = !identical(sort(input_key(baseline$inputs)), sort(input_key(current$inputs)))
  )

  changes <- data.table::rbindlist(rows, fill = TRUE)
  structure(
    list(
      schema_version = "1.0",
      current_id = current$project_id,
      baseline_id = baseline$project_id,
      changed = any(changes$changed),
      changes = changes
    ),
    class = "PopgenVCFProjectComparison"
  )
}
