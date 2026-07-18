#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L && nzchar(args[[1L]])) args[[1L]] else "scientific-release-integration"
release_id <- if (length(args) >= 2L && nzchar(args[[2L]])) args[[2L]] else "integration-release"

required <- c("popgenVCF", "data.table", "digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)

release_date <- Sys.getenv("POPGENVCF_RELEASE_DATE", "2026-07-18")
git_commit <- Sys.getenv("GITHUB_SHA", paste(rep("0", 40L), collapse = ""))
git_branch <- Sys.getenv("GITHUB_REF_NAME", "integration")
git_remote <- Sys.getenv("GITHUB_SERVER_URL", "https://github.com")

canonical_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!ncol(x)) return(x)
  x <- x[, sort(names(x)), drop = FALSE]
  for (name in names(x)) {
    if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
  }
  if (nrow(x)) {
    keys <- names(x)[vapply(x, function(column) is.atomic(column) && !is.list(column), logical(1))]
    if (length(keys)) {
      order_args <- lapply(x[keys], function(column) {
        ifelse(is.na(column), "<NA>", as.character(column))
      })
      x <- x[do.call(order, c(order_args, list(na.last = TRUE))), , drop = FALSE]
    }
  }
  rownames(x) <- NULL
  x
}

write_record <- function(path, payload) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  digest::digest(path, algo = "sha256", file = TRUE)
}

write_validation_table <- function(x, path) {
  table <- canonical_table(x)
  data.table::fwrite(table, path, sep = "\t", na = "NA", quote = FALSE)
  digest::digest(path, algo = "sha256", file = TRUE)
}

build_release <- function(root) {
  if (dir.exists(root)) unlink(root, recursive = TRUE, force = TRUE)
  records_dir <- file.path(root, "records")
  dir.create(records_dir, recursive = TRUE, showWarnings = FALSE)

  core <- popgenVCF::run_scientific_validation(integration = TRUE, threads = 2)
  structure <- popgenVCF::run_population_structure_validation(integration = TRUE)
  stopifnot(isTRUE(core$passed), isTRUE(structure$passed))

  core_path <- file.path(records_dir, "scientific-validation.tsv")
  structure_path <- file.path(records_dir, "population-structure-validation.tsv")
  core_digest <- write_validation_table(core$checks, core_path)
  structure_digest <- write_validation_table(structure$checks, structure_path)

  registry <- popgenVCF::default_analysis_registry()
  analysis_listing <- popgenVCF::list_analyses(registry)
  if (is.data.frame(analysis_listing)) {
    analysis_listing <- canonical_table(analysis_listing)
  } else {
    analysis_listing <- sort(unique(as.character(unlist(analysis_listing, use.names = FALSE))))
  }

  identities <- character()
  identities[["analysis_registry"]] <- write_record(
    file.path(records_dir, "analysis-registry.json"),
    list(schema_version = "1.0", record_type = "analysis_registry", analyses = analysis_listing)
  )
  identities[["provenance_dag"]] <- write_record(
    file.path(records_dir, "provenance-dag.json"),
    list(schema_version = "1.0", record_type = "provenance_dag", parents = list(analysis_registry = identities[["analysis_registry"]]))
  )
  identities[["artifact_lineage"]] <- write_record(
    file.path(records_dir, "artifact-lineage.json"),
    list(schema_version = "1.0", record_type = "artifact_lineage", parents = list(provenance_dag = identities[["provenance_dag"]]))
  )
  identities[["fair_bundle"]] <- write_record(
    file.path(records_dir, "fair-bundle.json"),
    list(schema_version = "1.0", record_type = "fair_bundle", parents = list(artifact_lineage = identities[["artifact_lineage"]]))
  )
  identities[["manuscript"]] <- write_record(
    file.path(records_dir, "manuscript.json"),
    list(schema_version = "1.0", record_type = "manuscript", manuscript_id = "integration-manuscript", parents = list(fair_bundle = identities[["fair_bundle"]]))
  )
  identities[["regeneration_plan"]] <- write_record(
    file.path(records_dir, "regeneration-plan.json"),
    list(schema_version = "1.0", record_type = "regeneration_plan", manuscript_id = "integration-manuscript", revision_id = "integration-revision", parents = list(manuscript = identities[["manuscript"]]))
  )
  identities[["regeneration_execution"]] <- write_record(
    file.path(records_dir, "regeneration-execution.json"),
    list(schema_version = "1.0", record_type = "regeneration_execution", status = "completed", parents = list(regeneration_plan = identities[["regeneration_plan"]]))
  )
  identities[["regeneration_verification"]] <- write_record(
    file.path(records_dir, "regeneration-verification.json"),
    list(schema_version = "1.0", record_type = "regeneration_verification", decision = "accepted", reviewer_id = "integration-workflow", parents = list(regeneration_execution = identities[["regeneration_execution"]]))
  )
  identities[["benchmark"]] <- write_record(
    file.path(records_dir, "benchmark.json"),
    list(
      schema_version = "1.0",
      record_type = "benchmark",
      deterministic = TRUE,
      core_check_count = nrow(as.data.frame(core$checks)),
      structure_check_count = nrow(as.data.frame(structure$checks)),
      validation_artifacts = list(core = core_digest, population_structure = structure_digest)
    )
  )
  identities[["scientific_validation"]] <- write_record(
    file.path(records_dir, "scientific-validation.json"),
    list(
      schema_version = "1.0",
      record_type = "scientific_validation",
      passed = TRUE,
      core = list(passed = isTRUE(core$passed), sha256 = core_digest),
      population_structure = list(passed = isTRUE(structure$passed), sha256 = structure_digest)
    )
  )

  record_files <- sort(list.files(records_dir, recursive = TRUE, full.names = TRUE))
  artifacts <- data.frame(
    path = sub(paste0("^", normalizePath(root, winslash = "/"), "/"), "", normalizePath(record_files, winslash = "/")),
    size_bytes = as.numeric(file.info(record_files)$size),
    sha256 = vapply(record_files, digest::digest, character(1), algo = "sha256", file = TRUE),
    stringsAsFactors = FALSE
  )

  dependency_names <- c("popgenVCF", "data.table", "digest", "jsonlite")
  installed <- utils::installed.packages()
  dependencies <- data.frame(
    package = dependency_names,
    version = unname(installed[dependency_names, "Version"]),
    stringsAsFactors = FALSE
  )

  release <- popgenVCF::new_scientific_release_bundle(
    release_id = release_id,
    package_version = as.character(utils::packageVersion("popgenVCF")),
    git_commit = git_commit,
    git_tag = release_id,
    release_date = release_date,
    digest_chain = identities,
    artifacts = artifacts,
    dependencies = dependencies,
    git_branch = git_branch,
    git_remote = git_remote,
    git_dirty = FALSE
  )

  bundle_dir <- file.path(root, "scientific-release")
  popgenVCF::write_scientific_release_bundle(release, bundle_dir)
  stopifnot(isTRUE(popgenVCF::validate_scientific_release_bundle(release)))
  stopifnot(isTRUE(popgenVCF::validate_scientific_release_bundle(bundle_dir)))

  summary <- list(
    schema_version = "1.0",
    release_id = release_id,
    release_digest = release$digest,
    digest_chain = as.list(release$digest_chain),
    artifact_count = nrow(artifacts),
    core_validation_passed = isTRUE(core$passed),
    population_structure_validation_passed = isTRUE(structure$passed)
  )
  jsonlite::write_json(summary, file.path(root, "integration-summary.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  list(release = release, summary = summary, bundle_dir = bundle_dir)
}

if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE, force = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

first <- build_release(file.path(output_dir, "run-a"))
second <- build_release(file.path(output_dir, "run-b"))

stopifnot(identical(first$release$digest, second$release$digest))
stopifnot(identical(unname(first$release$digest_chain), unname(second$release$digest_chain)))

comparison <- data.frame(
  component = c(names(first$release$digest_chain), "scientific_release"),
  run_a = c(unname(first$release$digest_chain), first$release$digest),
  run_b = c(unname(second$release$digest_chain), second$release$digest),
  identical = c(
    unname(first$release$digest_chain) == unname(second$release$digest_chain),
    identical(first$release$digest, second$release$digest)
  ),
  stringsAsFactors = FALSE
)
data.table::fwrite(comparison, file.path(output_dir, "determinism-comparison.tsv"), sep = "\t")
stopifnot(all(comparison$identical))

tampered <- file.path(output_dir, "tamper-check")
dir.create(tampered, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(first$bundle_dir, full.names = TRUE), tampered, recursive = TRUE)
cat("tampered\n", file = file.path(tampered, "scientific-release.md"), append = TRUE)
tamper_detected <- inherits(
  try(popgenVCF::validate_scientific_release_bundle(tampered), silent = TRUE),
  "try-error"
)
stopifnot(tamper_detected)

final_summary <- list(
  schema_version = "1.0",
  release_id = release_id,
  release_digest = first$release$digest,
  deterministic = TRUE,
  tamper_detected = TRUE,
  digest_chain = as.list(first$release$digest_chain)
)
jsonlite::write_json(final_summary, file.path(output_dir, "integration-summary.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")

cat("Scientific release integration passed\n")
cat("Release digest:", first$release$digest, "\n")
cat("Output:", normalizePath(output_dir, winslash = "/", mustWork = TRUE), "\n")
