# Phase 10.2.4 - compatibility closure audit and roadmap handoff

.phase10_2_milestone_commits <- c(
  "10.2.1" = "268a57e897e85c62a0e51a8723e5d336df9afd1f",
  "10.2.2" = "2eb51961c19ee10ef2fb27a29cdfd6e55d9b3668",
  "10.2.3" = "9d6494c1b41a5404c5c26fdb1e96447061a7684c"
)

#' Audit the completed Phase 10.2 compatibility evidence chain
#'
#' @param descriptor Candidate public API descriptor.
#' @param compatibility Phase 10.2.1 compatibility record.
#' @param migration_plan Phase 10.2.2 migration plan.
#' @param conformance Phase 10.2.3 release-conformance manifest.
#' @param policy API evolution policy.
#' @return A deterministic Phase 10.2 closure-audit record.
#' @export
phase10_2_audit_compatibility_closure <- function(
    descriptor,
    compatibility,
    migration_plan,
    conformance,
    policy = phase10_api_evolution_policy()) {
  validate_phase10_api_descriptor(descriptor)
  validate_phase10_api_compatibility(compatibility, allow_breaking = TRUE)
  validate_phase10_api_migration_plan(migration_plan, compatibility, policy)
  validate_phase10_release_conformance(
    conformance, descriptor, compatibility, migration_plan, policy
  )

  if (!identical(compatibility$candidate_fingerprint, descriptor$fingerprint)) {
    stop("Compatibility evidence is not bound to the supplied candidate descriptor.",
         call. = FALSE)
  }
  if (!identical(migration_plan$compatibility_fingerprint, compatibility$fingerprint)) {
    stop("Migration evidence is not bound to the compatibility record.", call. = FALSE)
  }
  if (!identical(conformance$compatibility_fingerprint, compatibility$fingerprint) ||
      !identical(conformance$migration_plan_fingerprint, migration_plan$fingerprint) ||
      !identical(conformance$descriptor_fingerprint, descriptor$fingerprint) ||
      !identical(conformance$policy_fingerprint, policy$fingerprint)) {
    stop("Release conformance evidence bindings do not close the Phase 10.2 chain.",
         call. = FALSE)
  }

  channels <- sort(conformance$release_identities$channel)
  required_channels <- sort(.phase10_required_release_channels())
  if (!identical(channels, required_channels)) {
    stop("Phase 10.2 closure requires every authoritative release channel.",
         call. = FALSE)
  }

  blockers <- sort(unique(c(
    conformance$blockers,
    if (!isTRUE(conformance$release_ready)) "release_conformance_not_ready" else character()
  )))

  audit <- list(
    record_type = "phase10_2_compatibility_closure_audit",
    schema_version = "1.0.0",
    milestone_commits = .phase10_2_milestone_commits,
    api_version = descriptor$api_version,
    release_version = conformance$release_version,
    descriptor_fingerprint = descriptor$fingerprint,
    compatibility_fingerprint = compatibility$fingerprint,
    migration_plan_fingerprint = migration_plan$fingerprint,
    policy_fingerprint = policy$fingerprint,
    conformance_fingerprint = conformance$fingerprint,
    release_channels = required_channels,
    compatibility_classification = compatibility$classification,
    blockers = blockers,
    passed = length(blockers) == 0L
  )
  audit$fingerprint <- phase10_public_fingerprint(audit)
  class(audit) <- c("PopgenVCFPhase10CompatibilityClosureAudit", "list")
  audit
}

#' Assemble deterministic Phase 10.2 closure evidence
#'
#' @param audit A Phase 10.2 compatibility closure audit.
#' @param ci_evidence_id Immutable identity for the green authoritative CI run.
#' @param unresolved_blockers Additional unresolved closure blockers.
#' @return A deterministic Phase 10.2 closure-evidence record.
#' @export
phase10_2_closure_evidence <- function(
    audit,
    ci_evidence_id,
    unresolved_blockers = character()) {
  .phase10_2_validate_audit(audit)
  .phase10_scalar_string(ci_evidence_id, "ci_evidence_id")

  blockers <- sort(unique(c(audit$blockers, as.character(unresolved_blockers))))
  blockers <- blockers[!is.na(blockers) & nzchar(blockers)]
  record <- list(
    record_type = "phase10_2_closure_evidence",
    schema_version = "1.0.0",
    milestone_commits = audit$milestone_commits,
    compatibility_closure_audit_id = audit$fingerprint,
    api_descriptor_id = audit$descriptor_fingerprint,
    release_conformance_id = audit$conformance_fingerprint,
    ci_evidence_id = ci_evidence_id,
    unresolved_blockers = blockers,
    closure_approved = isTRUE(audit$passed) && length(blockers) == 0L,
    next_milestone = "0.9.1-publication-report-rendering"
  )
  record$fingerprint <- phase10_public_fingerprint(record)
  class(record) <- c("PopgenVCFPhase10CompatibilityClosureEvidence", "list")
  record
}

#' Render a deterministic Phase 10.2 closure report
#'
#' @param closure Phase 10.2 closure evidence.
#' @return Character lines containing a Markdown closure report.
#' @export
phase10_2_closure_report <- function(closure) {
  if (!inherits(closure, "PopgenVCFPhase10CompatibilityClosureEvidence")) {
    stop("closure must be Phase 10.2 compatibility closure evidence.", call. = FALSE)
  }
  expected <- phase10_public_fingerprint(closure)
  if (!identical(closure$fingerprint, expected)) {
    stop("Phase 10.2 closure evidence fingerprint mismatch.", call. = FALSE)
  }
  c(
    "# Phase 10.2 compatibility closure review",
    "",
    sprintf("- Closure approved: `%s`", closure$closure_approved),
    sprintf("- Milestones bound: `%d`", length(closure$milestone_commits)),
    sprintf("- Unresolved blockers: `%d`", length(closure$unresolved_blockers)),
    sprintf("- Closure fingerprint: `%s`", closure$fingerprint),
    sprintf("- Next milestone: `%s`", closure$next_milestone)
  )
}

.phase10_2_validate_audit <- function(audit) {
  if (!inherits(audit, "PopgenVCFPhase10CompatibilityClosureAudit")) {
    stop("audit must be a Phase 10.2 compatibility closure audit.", call. = FALSE)
  }
  expected <- phase10_public_fingerprint(audit)
  if (!identical(audit$fingerprint, expected)) {
    stop("Phase 10.2 compatibility closure audit fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}
