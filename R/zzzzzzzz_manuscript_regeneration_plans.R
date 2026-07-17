regeneration_scalar <- function(x, label) {
  x <- trimws(as.character(x)[1L])
  if (is.na(x) || !nzchar(x)) stop(label, " must be non-empty", call. = FALSE)
  x
}

regeneration_id <- function(x, label) {
  x <- regeneration_scalar(x, label)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:-]*$", x)) {
    stop(label, " contains unsupported characters", call. = FALSE)
  }
  x
}

regeneration_dependencies <- function(x) {
  x <- data.table::as.data.table(x)
  required <- c("section_id", "dependency_id", "dependency_type", "policy")
  if (!all(required %in% names(x))) {
    stop("dependencies must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  x <- data.table::copy(x)[, ..required]
  for (column in required) x[[column]] <- trimws(as.character(x[[column]]))
  if (!nrow(x)) stop("dependencies must contain at least one row", call. = FALSE)
  if (anyNA(x) || any(!nzchar(unlist(x, use.names = FALSE)))) {
    stop("dependencies must contain non-empty values", call. = FALSE)
  }
  if (anyDuplicated(x[, .(section_id, dependency_id)])) {
    stop("section/dependency mappings must be unique", call. = FALSE)
  }
  allowed_types <- c("input", "section")
  if (any(!x$dependency_type %in% allowed_types)) {
    stop("dependency_type must be input or section", call. = FALSE)
  }
  allowed_policies <- c("regenerate", "manual_review", "blocked")
  if (any(!x$policy %in% allowed_policies)) {
    stop("policy must be regenerate, manual_review, or blocked", call. = FALSE)
  }
  section_nodes <- unique(x$section_id)
  unknown_sections <- x[x$dependency_type == "section" & !x$dependency_id %in% section_nodes, dependency_id]
  if (length(unknown_sections)) {
    stop("section dependencies reference unknown sections: ", paste(sort(unique(unknown_sections)), collapse = ", "), call. = FALSE)
  }
  edges <- x[x$dependency_type == "section", .(from = dependency_id, to = section_id)]
  if (nrow(edges)) {
    remaining <- sort(unique(c(edges$from, edges$to)))
    indegree <- setNames(integer(length(remaining)), remaining)
    for (target in edges$to) indegree[[target]] <- indegree[[target]] + 1L
    queue <- sort(names(indegree)[indegree == 0L])
    visited <- character()
    while (length(queue)) {
      node <- queue[[1L]]
      queue <- queue[-1L]
      visited <- c(visited, node)
      targets <- sort(edges[edges$from == node, to])
      for (target in targets) {
        indegree[[target]] <- indegree[[target]] - 1L
        if (indegree[[target]] == 0L) queue <- sort(c(queue, target))
      }
    }
    if (length(visited) != length(remaining)) stop("section dependency mappings must be acyclic", call. = FALSE)
  }
  data.table::setorderv(x, c("section_id", "dependency_type", "dependency_id"))
  x
}

regeneration_changes <- function(x) {
  x <- data.table::as.data.table(x)
  required <- c("dependency_id", "before_identity", "after_identity", "change_type")
  if (!all(required %in% names(x))) {
    stop("changes must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  x <- data.table::copy(x)[, ..required]
  for (column in required) x[[column]] <- trimws(as.character(x[[column]]))
  if (anyNA(x) || any(!nzchar(unlist(x, use.names = FALSE)))) {
    stop("changes must contain non-empty values", call. = FALSE)
  }
  if (anyDuplicated(x$dependency_id)) stop("changes must contain unique dependency_id values", call. = FALSE)
  allowed <- c("added", "removed", "modified", "identity_changed")
  if (any(!x$change_type %in% allowed)) stop("invalid change_type", call. = FALSE)
  data.table::setorderv(x, "dependency_id")
  x
}

#' Create a deterministic manuscript regeneration plan
#'
#' @param manuscript_id Stable manuscript identifier.
#' @param revision_id Stable target revision identifier.
#' @param dependencies Data frame describing section dependencies.
#' @param changes Data frame describing explicit changed dependencies.
#' @param generator_id Stable generator contract identifier.
#' @return A validated `PopgenVCFRegenerationPlan`.
#' @export
new_manuscript_regeneration_plan <- function(manuscript_id, revision_id, dependencies,
                                             changes, generator_id = "popgenVCF-manuscript") {
  dependencies <- regeneration_dependencies(dependencies)
  changes <- regeneration_changes(changes)
  known_inputs <- dependencies[dependencies$dependency_type == "input", unique(dependency_id)]
  unknown <- setdiff(changes$dependency_id, known_inputs)
  if (length(unknown)) stop("changes reference unknown input dependencies: ", paste(unknown, collapse = ", "), call. = FALSE)
  payload <- list(
    schema_version = "1.0",
    manuscript_id = regeneration_id(manuscript_id, "manuscript_id"),
    revision_id = regeneration_id(revision_id, "revision_id"),
    generator_id = regeneration_id(generator_id, "generator_id"),
    dependencies = dependencies,
    changes = changes
  )
  payload$plan <- manuscript_regeneration_table(payload)
  payload$digest <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  out <- structure(payload, class = "PopgenVCFRegenerationPlan")
  validate_manuscript_regeneration_plan(out)
  out
}

#' Compute the deterministic regeneration table
#'
#' @param x A regeneration plan or plan-like list.
#' @return A deterministic data table.
#' @export
manuscript_regeneration_table <- function(x) {
  deps <- regeneration_dependencies(x$dependencies)
  changes <- regeneration_changes(x$changes)
  sections <- sort(unique(deps$section_id))
  result <- data.table::data.table(
    section_id = sections,
    state = "unaffected",
    reason = "No changed dependency",
    source_changes = ""
  )
  changed_ids <- sort(changes$dependency_id)
  direct <- deps[deps$dependency_type == "input" & deps$dependency_id %in% changed_ids, ]
  for (section_name in sort(unique(direct$section_id))) {
    rows <- direct[direct$section_id == section_name, ]
    policy_rank <- c(regenerate = 1L, manual_review = 2L, blocked = 3L)
    chosen <- rows$policy[[which.max(unname(policy_rank[rows$policy]))]]
    state <- switch(chosen, regenerate = "affected", manual_review = "manual_review", blocked = "blocked")
    ids <- sort(unique(rows$dependency_id))
    result[result$section_id == section_name, `:=`(
      state = state,
      reason = paste0("Direct changed input: ", paste(ids, collapse = ", ")),
      source_changes = paste(ids, collapse = ";")
    )]
  }
  section_edges <- deps[deps$dependency_type == "section", ]
  changed_state <- function(value) value %in% c("affected", "manual_review", "blocked")
  repeat {
    previous <- result$state
    for (i in seq_len(nrow(section_edges))) {
      upstream <- section_edges$dependency_id[[i]]
      downstream <- section_edges$section_id[[i]]
      upstream_row <- result[result$section_id == upstream, ]
      if (!nrow(upstream_row) || !changed_state(upstream_row$state[[1L]])) next
      policy <- section_edges$policy[[i]]
      propagated <- switch(policy, regenerate = "affected", manual_review = "manual_review", blocked = "blocked")
      rank <- c(unaffected = 0L, affected = 1L, manual_review = 2L, blocked = 3L)
      current <- result[result$section_id == downstream, state]
      if (rank[[propagated]] >= rank[[current]]) {
        inherited <- upstream_row$source_changes[[1L]]
        result[result$section_id == downstream, `:=`(
          state = propagated,
          reason = paste0("Depends on changed section: ", upstream),
          source_changes = inherited
        )]
      }
    }
    if (identical(previous, result$state)) break
  }
  data.table::setorderv(result, "section_id")
  result
}

#' Validate a manuscript regeneration plan or written directory
#'
#' @param x A `PopgenVCFRegenerationPlan` or directory.
#' @param strict Whether blocked sections raise an error.
#' @return `TRUE` invisibly.
#' @export
validate_manuscript_regeneration_plan <- function(x, strict = FALSE) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("regeneration-plan.json", "regeneration-plan.md", "regeneration-plan.tsv", "regeneration-plan-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("regeneration plan directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "regeneration-plan-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      if (!file.exists(path) || !identical(digest::digest(path, algo = "sha256", file = TRUE), manifest$sha256[[i]])) {
        stop("regeneration plan checksum mismatch: ", manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFRegenerationPlan")) stop("x must be a PopgenVCFRegenerationPlan or directory", call. = FALSE)
  regeneration_dependencies(x$dependencies)
  regeneration_changes(x$changes)
  expected_plan <- manuscript_regeneration_table(x)
  if (!identical(data.table::as.data.table(x$plan), expected_plan)) stop("regeneration plan table mismatch", call. = FALSE)
  expected <- digest::digest(x[setdiff(names(x), "digest")], algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("regeneration plan digest mismatch", call. = FALSE)
  if (isTRUE(strict) && any(x$plan$state == "blocked")) {
    stop("blocked manuscript sections require resolution: ", paste(x$plan[x$plan$state == "blocked", section_id], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Render a manuscript regeneration plan as Markdown
#'
#' @param x A validated regeneration plan.
#' @return Markdown lines.
#' @export
render_manuscript_regeneration_plan <- function(x) {
  validate_manuscript_regeneration_plan(x)
  rows <- vapply(seq_len(nrow(x$plan)), function(i) {
    row <- x$plan[i]
    paste0("| `", row$section_id, "` | ", row$state, " | ", row$reason, " | ", row$source_changes, " |")
  }, character(1))
  c(
    "# Manuscript regeneration plan", "",
    paste0("- Manuscript ID: `", x$manuscript_id, "`"),
    paste0("- Revision ID: `", x$revision_id, "`"),
    paste0("- Generator ID: `", x$generator_id, "`"),
    paste0("- Digest: `", x$digest, "`"), "",
    "This plan identifies affected sections only. It does not rewrite manuscript content.", "",
    "| Section | State | Reason | Source changes |",
    "|---|---|---|---|",
    rows
  )
}

#' Write a deterministic manuscript regeneration plan bundle
#'
#' @param x A validated regeneration plan.
#' @param path Output directory.
#' @param overwrite Whether an existing directory may be replaced.
#' @return Normalized output path invisibly.
#' @export
write_manuscript_regeneration_plan <- function(x, path, overwrite = FALSE) {
  validate_manuscript_regeneration_plan(x)
  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) stop("output directory already exists", call. = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(path, "regeneration-plan.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(render_manuscript_regeneration_plan(x), file.path(path, "regeneration-plan.md"), useBytes = TRUE)
  data.table::fwrite(x$plan, file.path(path, "regeneration-plan.tsv"), sep = "\t")
  files <- c("regeneration-plan.json", "regeneration-plan.md", "regeneration-plan.tsv")
  manifest <- data.table::data.table(
    path = files,
    size_bytes = as.numeric(file.info(file.path(path, files))$size),
    sha256 = vapply(file.path(path, files), digest::digest, character(1), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(path, "regeneration-plan-manifest.tsv"), sep = "\t")
  validate_manuscript_regeneration_plan(path)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
