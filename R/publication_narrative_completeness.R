publication_narrative_kinds <- function() {
  c("pca", "ibs", "tree", "diversity", "fst", "amova", "dapc", "ibd", "ancestry")
}

publication_narrative_state <- function(result = NULL, module = NULL) {
  status <- tolower(as.character(module$status %||% result$status %||% "present")[1L])
  allowed <- c("present", "skipped", "unavailable", "incomplete", "failed", "diagnostic-only")
  if (is.null(result) && identical(status, "present")) status <- "unavailable"
  if (!status %in% allowed) status <- if (is.null(result)) "unavailable" else "incomplete"
  status
}

publication_narrative_reason <- function(result = NULL, module = NULL, state) {
  reason <- module$reason %||% result$reason %||% result$message %||% NA_character_
  if (!is.na(reason) && nzchar(as.character(reason)[1L])) return(as.character(reason)[1L])
  switch(state,
    present = "Canonical result is present.",
    skipped = "Analysis was intentionally skipped by the recorded execution plan.",
    unavailable = "No canonical result was available for this analysis family.",
    incomplete = "The recorded result did not contain complete publication evidence.",
    failed = "Analysis execution failed; no scientific claim was generated.",
    `diagnostic-only` = "Evidence is diagnostic-only and is not release-certifying.",
    "Publication state was not resolved."
  )
}

publication_supplementary_summary <- function(kind, result, state, reason) {
  if (!identical(state, "present")) return(paste0(toupper(kind), ": ", state, ". ", reason))
  values <- switch(kind,
    pca = c(samples = publication_count(result, c("n_samples", "sample_count")),
            variants = publication_count(result, c("n_snps", "variant_count", "snp_count"))),
    ancestry = c(selected_k = suppressWarnings(as.integer(result$selected_k %||% result$k %||% NA_integer_))),
    dapc = c(retained_pcs = suppressWarnings(as.integer(publication_result_parameters(result)$n_pca %||% NA_integer_))),
    numeric()
  )
  values <- values[is.finite(values)]
  detail <- if (length(values)) paste(paste(names(values), values, sep = "="), collapse = "; ") else
    "Canonical parameters, provenance, and source tables are retained in the publication bundle."
  paste0(toupper(kind), ": present. ", detail)
}

#' Build the canonical publication narrative inventory
#'
#' @param project A reproducible `PopgenVCFProject`.
#' @return A nine-row data table describing publication evidence and fallback states.
#' @export
publication_narrative_inventory <- function(project) {
  validate_popgenvcf_project(project)
  results <- project$results %||% list()
  modules <- project$modules %||% list()
  kinds <- publication_narrative_kinds()
  result_names <- names(results) %||% rep("", length(results))
  rows <- lapply(kinds, function(kind) {
    hits <- which(vapply(seq_along(results), function(i) {
      identical(publication_result_kind(results[[i]], result_names[[i]]), kind)
    }, logical(1L)))
    if (length(hits) > 1L) stop("duplicate narrative ownership for analysis family: ", kind, call. = FALSE)
    result <- if (length(hits)) results[[hits[[1L]]]] else NULL
    result_name <- if (length(hits) && nzchar(result_names[[hits[[1L]]]])) result_names[[hits[[1L]]]] else kind
    module <- modules[[result_name]] %||% modules[[kind]] %||% list()
    state <- publication_narrative_state(result, module)
    reason <- publication_narrative_reason(result, module, state)
    narrative <- if (!is.null(result) && state %in% c("present", "diagnostic-only")) {
      publication_analysis_narrative(result, result_name)
    } else {
      data.table::data.table(analysis = result_name, kind = kind, method = NA_character_,
                             legend = NA_character_, citation_keys = NA_character_)
    }
    data.table::data.table(
      analysis = narrative$analysis[[1L]], kind = kind, state = state, reason = reason,
      method = narrative$method[[1L]], legend = narrative$legend[[1L]],
      citation_keys = narrative$citation_keys[[1L]],
      supplementary_summary = publication_supplementary_summary(kind, result, state, reason),
      method_complete = state %in% c("present", "diagnostic-only") && !is.na(narrative$method[[1L]]) && nzchar(narrative$method[[1L]]),
      caption_complete = state %in% c("present", "diagnostic-only") && !is.na(narrative$legend[[1L]]) && nzchar(narrative$legend[[1L]]),
      citation_complete = state %in% c("present", "diagnostic-only") && !is.na(narrative$citation_keys[[1L]]) && nzchar(narrative$citation_keys[[1L]])
    )
  })
  inventory <- data.table::rbindlist(rows, fill = TRUE)
  inventory[, narrative_order := match(kind, kinds)]
  data.table::setorder(inventory, narrative_order)
  inventory[, narrative_order := NULL]
  inventory[]
}

#' Summarize publication narrative completeness
#'
#' @param inventory Output from `publication_narrative_inventory()`.
#' @return A one-row data table with deterministic completeness counts.
#' @export
publication_narrative_completeness <- function(inventory) {
  required <- c("kind", "state", "method_complete", "caption_complete", "citation_complete", "supplementary_summary")
  missing <- setdiff(required, names(inventory))
  if (length(missing)) stop("narrative inventory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (nrow(inventory) != length(publication_narrative_kinds()) || anyDuplicated(inventory$kind)) {
    stop("narrative inventory must contain one row for each canonical analysis family", call. = FALSE)
  }
  present <- inventory$state %in% c("present", "diagnostic-only")
  complete <- !present | (inventory$method_complete & inventory$caption_complete &
                            inventory$citation_complete & nzchar(inventory$supplementary_summary))
  data.table::data.table(
    required_families = nrow(inventory), present_families = sum(present),
    fallback_families = sum(!present), complete_families = sum(complete),
    diagnostic_only = sum(inventory$state == "diagnostic-only"), passed = all(complete)
  )
}

publication_validate_caption_ownership <- function(artifacts, inventory) {
  if (!nrow(artifacts)) return(invisible(TRUE))
  active <- inventory[inventory$state %in% c("present", "diagnostic-only")]
  for (id in artifacts$id) {
    id_lower <- tolower(id)
    owned <- vapply(seq_len(nrow(active)), function(i) {
      grepl(active$kind[[i]], id_lower, fixed = TRUE) ||
        grepl(tolower(active$analysis[[i]]), id_lower, fixed = TRUE)
    }, logical(1L))
    if (sum(owned) > 1L) stop("conflicting caption ownership for artifact: ", id, call. = FALSE)
  }
  invisible(TRUE)
}
