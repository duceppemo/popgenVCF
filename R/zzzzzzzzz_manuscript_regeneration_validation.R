# Canonical validation for manuscript regeneration plans.
#
# data.table maintains internal reference attributes that may change after
# ordinary read-only subsetting. Those implementation attributes are not part
# of the scientific plan and must not affect equality or digest validation.

regeneration_digest_payload <- function(x) {
  payload <- unclass(x)
  payload$digest <- NULL
  for (name in intersect(c("dependencies", "changes", "plan"), names(payload))) {
    payload[[name]] <- as.data.frame(payload[[name]], stringsAsFactors = FALSE)
    rownames(payload[[name]]) <- NULL
  }
  payload
}

#' Create a deterministic manuscript regeneration plan
#'
#' @inheritParams new_manuscript_regeneration_plan
#' @return A validated `PopgenVCFRegenerationPlan`.
#' @export
new_manuscript_regeneration_plan <- function(manuscript_id, revision_id, dependencies,
                                             changes, generator_id = "popgenVCF-manuscript") {
  dependencies <- regeneration_dependencies(dependencies)
  changes <- regeneration_changes(changes)
  known_inputs <- dependencies[dependencies$dependency_type == "input", unique(dependency_id)]
  unknown <- setdiff(changes$dependency_id, known_inputs)
  if (length(unknown)) {
    stop("changes reference unknown input dependencies: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  payload <- list(
    schema_version = "1.0",
    manuscript_id = regeneration_id(manuscript_id, "manuscript_id"),
    revision_id = regeneration_id(revision_id, "revision_id"),
    generator_id = regeneration_id(generator_id, "generator_id"),
    dependencies = dependencies,
    changes = changes
  )
  payload$plan <- manuscript_regeneration_table(payload)
  payload$digest <- digest::digest(
    regeneration_digest_payload(payload), algo = "sha256", serialize = TRUE
  )
  out <- structure(payload, class = "PopgenVCFRegenerationPlan")
  validate_manuscript_regeneration_plan(out)
  out
}

#' Validate a manuscript regeneration plan or written directory
#'
#' @inheritParams validate_manuscript_regeneration_plan
#' @return `TRUE` invisibly.
#' @export
validate_manuscript_regeneration_plan <- function(x, strict = FALSE) {
  if (is.character(x) && length(x) == 1L) {
    required <- c(
      "regeneration-plan.json", "regeneration-plan.md",
      "regeneration-plan.tsv", "regeneration-plan-manifest.tsv"
    )
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) {
      stop("regeneration plan directory is missing: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    manifest <- data.table::fread(file.path(x, "regeneration-plan-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      actual <- if (file.exists(path)) {
        digest::digest(path, algo = "sha256", file = TRUE)
      } else {
        NA_character_
      }
      if (!identical(actual, manifest$sha256[[i]])) {
        stop("regeneration plan checksum mismatch: ",
             manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }

  if (!inherits(x, "PopgenVCFRegenerationPlan")) {
    stop("x must be a PopgenVCFRegenerationPlan or directory", call. = FALSE)
  }
  regeneration_dependencies(x$dependencies)
  regeneration_changes(x$changes)

  actual_plan <- as.data.frame(x$plan, stringsAsFactors = FALSE)
  expected_plan <- as.data.frame(
    manuscript_regeneration_table(x), stringsAsFactors = FALSE
  )
  rownames(actual_plan) <- NULL
  rownames(expected_plan) <- NULL
  if (!identical(actual_plan, expected_plan)) {
    stop("regeneration plan table mismatch", call. = FALSE)
  }

  expected <- digest::digest(
    regeneration_digest_payload(x), algo = "sha256", serialize = TRUE
  )
  if (!identical(expected, x$digest)) {
    stop("regeneration plan digest mismatch", call. = FALSE)
  }
  if (isTRUE(strict) && any(x$plan$state == "blocked")) {
    blocked <- x$plan$section_id[x$plan$state == "blocked"]
    stop("blocked manuscript sections require resolution: ",
         paste(blocked, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
