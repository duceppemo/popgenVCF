# Phase 9.14 — final closure review and roadmap handoff

.phase9_required_evidence_domains <- c(
  "schemas",
  "runtime",
  "cache",
  "replay",
  "checkpointing",
  "recovery",
  "provenance",
  "validation",
  "publication",
  "performance",
  "documentation",
  "migration"
)

phase9_closure_review <- function(
    milestone_commits,
    evidence_ids,
    unresolved_blockers = character(),
    deferred_items = character(),
    acknowledged_risks = character(),
    release_readiness_id,
    migration_registry_id,
    deprecation_portfolio_id,
    ci_evidence_id,
    closure_approved = FALSE,
    fingerprint = NULL) {
  milestone_commits <- phase9_named_scalar_ids(
    milestone_commits,
    "milestone_commits"
  )
  evidence_ids <- phase9_named_scalar_ids(evidence_ids, "evidence_ids")
  unresolved_blockers <- sort(unique(unresolved_blockers))
  deferred_items <- sort(unique(deferred_items))
  acknowledged_risks <- sort(unique(acknowledged_risks))

  missing_domains <- setdiff(
    .phase9_required_evidence_domains,
    names(evidence_ids)
  )
  if (length(missing_domains) > 0L) {
    stop(
      sprintf(
        "Phase 9 closure is missing evidence domains: %s",
        paste(missing_domains, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  required_ids <- c(
    release_readiness_id,
    migration_registry_id,
    deprecation_portfolio_id,
    ci_evidence_id
  )
  if (length(required_ids) != 4L ||
      any(!is.character(required_ids)) ||
      any(lengths(required_ids) != 1L) ||
      any(!nzchar(required_ids))) {
    stop("All Phase 9 closure identities are required.", call. = FALSE)
  }

  false_claim <- isTRUE(closure_approved) &&
    (length(unresolved_blockers) > 0L ||
       length(milestone_commits) == 0L)
  if (false_claim) {
    stop(
      "Phase 9 closure cannot pass with unresolved blockers or no milestones.",
      call. = FALSE
    )
  }

  record <- list(
    record_type = "phase9_closure_review",
    schema_version = "1.0.0",
    milestone_commits = milestone_commits,
    evidence_ids = evidence_ids[
      .phase9_required_evidence_domains
    ],
    unresolved_blockers = unresolved_blockers,
    deferred_items = deferred_items,
    acknowledged_risks = acknowledged_risks,
    release_readiness_id = release_readiness_id,
    migration_registry_id = migration_registry_id,
    deprecation_portfolio_id = deprecation_portfolio_id,
    ci_evidence_id = ci_evidence_id,
    closure_approved = isTRUE(closure_approved)
  )

  record$fingerprint <- fingerprint %||%
    phase9_closure_fingerprint(record)
  class(record) <- c("phase9_closure_review", "list")
  record
}

phase9_roadmap_handoff <- function(
    closure_review_id,
    next_phase_id,
    goal,
    scope_boundary,
    dependency_ids,
    entry_criteria,
    fingerprint = NULL) {
  scalar_ids <- c(closure_review_id, next_phase_id, goal, scope_boundary)
  if (any(!is.character(scalar_ids)) ||
      any(lengths(scalar_ids) != 1L) ||
      any(!nzchar(scalar_ids))) {
    stop("Closure, phase, goal, and scope identities are required.",
         call. = FALSE)
  }

  dependency_ids <- sort(unique(dependency_ids))
  entry_criteria <- sort(unique(entry_criteria))
  if (length(dependency_ids) == 0L || length(entry_criteria) == 0L ||
      any(!nzchar(dependency_ids)) || any(!nzchar(entry_criteria))) {
    stop(
      "Roadmap handoff requires dependencies and entry criteria.",
      call. = FALSE
    )
  }

  record <- list(
    record_type = "phase9_roadmap_handoff",
    schema_version = "1.0.0",
    closure_review_id = closure_review_id,
    next_phase_id = next_phase_id,
    goal = goal,
    scope_boundary = scope_boundary,
    dependency_ids = dependency_ids,
    entry_criteria = entry_criteria
  )

  record$fingerprint <- fingerprint %||%
    phase9_closure_fingerprint(record)
  class(record) <- c("phase9_roadmap_handoff", "list")
  record
}

phase9_named_scalar_ids <- function(x, argument) {
  if (!is.character(x) || is.null(names(x)) ||
      any(!nzchar(names(x))) || any(!nzchar(x))) {
    stop(
      sprintf("%s must be a named character vector of non-empty IDs.", argument),
      call. = FALSE
    )
  }
  if (anyDuplicated(names(x))) {
    stop(sprintf("%s contains duplicate names.", argument), call. = FALSE)
  }
  x[order(names(x))]
}

phase9_closure_fingerprint <- function(x) {
  payload <- x
  payload$fingerprint <- NULL
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}
