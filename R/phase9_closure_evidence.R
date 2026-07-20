# Phase 9.14.1 — closure evidence assembly and roadmap synchronization

.phase9_milestone_commits <- c(
  "9.1" = "e3d730f9b6f92075e597e1c72dc0cff86d83ed10",
  "9.2" = "e3ea85bef632d0b75fdd17460fcb481659b76f50",
  "9.3" = "972a21b4069be3c7f148ed4b42494869227cd4aa",
  "9.4" = "370eb0ff059a85111527e63693340eb4a2849387",
  "9.5" = "636db0ec8407efa040e8e21323b9a2bcf89d2ae7",
  "9.6" = "c18472cad58196cb23bcf56553ad3a21a66b6375",
  "9.7" = "19a8798ea835217d5965e51cf6abd03b71f23a2c",
  "9.8" = "a3b8271a5e3697315e5923c40ae625848f570819",
  "9.9" = "da3bbadd3c38b15677f7851ac6054195775b0499",
  "9.10" = "c0b76809560537c59d4c136ed35f05a633e37136",
  "9.11" = "b7d16797f2650f5b55a620f245e4c5486bae23b4",
  "9.12" = "dffbddc16c4591d61571fb7414fd0fa6002c440b",
  "9.13" = "289108329966967a590c81c7d6b02e6de3f63ebd",
  "9.14" = "ac6b35f4742ce57e771dd290d3890cc20b595284"
)

.phase9_closure_evidence_ids <- c(
  schemas = "phase9.4.schema-registry",
  runtime = "phase9.9.runtime-adapter",
  cache = "phase9.5.scientific-object-cache",
  replay = "phase9.10.replay-equivalence",
  checkpointing = "phase9.7.execution-checkpoints",
  recovery = "phase9.7.recovery-decisions",
  provenance = "phase9.10.runtime-provenance",
  validation = "phase9.11.production-equivalence",
  publication = "phase9.2.publication-artifacts",
  performance = "phase9.12.cutover-performance-gate",
  documentation = "phase9.14.1.closure-guidance",
  migration = "phase9.12.production-migration-registry"
)

phase9_milestone_manifest <- function() {
  data.frame(
    milestone_id = names(.phase9_milestone_commits),
    merge_commit = unname(.phase9_milestone_commits),
    stringsAsFactors = FALSE
  )
}

phase9_closure_evidence_manifest <- function() {
  data.frame(
    evidence_domain = names(.phase9_closure_evidence_ids),
    evidence_id = unname(.phase9_closure_evidence_ids),
    stringsAsFactors = FALSE
  )
}

phase9_assemble_closure <- function(
    release_readiness_id,
    migration_registry_id,
    deprecation_portfolio_id,
    ci_evidence_id,
    release_ready = FALSE,
    unresolved_blockers = character(),
    deferred_items = c(
      "Phase 10 scientific-interface implementation",
      "post-closure legacy retirement decisions"
    ),
    acknowledged_risks = c(
      "legacy compatibility remains governed by explicit rollback records"
    )) {
  scalar_ids <- c(
    release_readiness_id,
    migration_registry_id,
    deprecation_portfolio_id,
    ci_evidence_id
  )
  if (any(!is.character(scalar_ids)) || any(lengths(scalar_ids) != 1L) ||
      any(!nzchar(scalar_ids))) {
    stop("All closure assembly identities are required.", call. = FALSE)
  }

  unresolved_blockers <- sort(unique(unresolved_blockers))
  closure_approved <- isTRUE(release_ready) && length(unresolved_blockers) == 0L

  review <- phase9_closure_review(
    milestone_commits = .phase9_milestone_commits,
    evidence_ids = .phase9_closure_evidence_ids,
    unresolved_blockers = unresolved_blockers,
    deferred_items = deferred_items,
    acknowledged_risks = acknowledged_risks,
    release_readiness_id = release_readiness_id,
    migration_registry_id = migration_registry_id,
    deprecation_portfolio_id = deprecation_portfolio_id,
    ci_evidence_id = ci_evidence_id,
    closure_approved = closure_approved
  )

  handoff <- phase9_roadmap_handoff(
    closure_review_id = review$fingerprint,
    next_phase_id = "10.1",
    goal = paste(
      "Define the canonical public analysis and artifact API that exposes",
      "the unified runtime through stable scientific interfaces."
    ),
    scope_boundary = paste(
      "Public module, result, artifact, report, and provenance contracts;",
      "no new statistical methods or alternate execution runtime."
    ),
    dependency_ids = c(
      review$fingerprint,
      release_readiness_id,
      migration_registry_id
    ),
    entry_criteria = c(
      "Phase 9 closure review is approved",
      "required CI and scientific-validation evidence is immutable",
      "migration blockers are empty",
      "rollback and compatibility obligations are documented"
    )
  )

  record <- list(
    record_type = "phase9_closure_evidence_bundle",
    schema_version = "1.0.0",
    closure_review = review,
    roadmap_handoff = handoff,
    assembled = TRUE
  )
  record$fingerprint <- phase9_closure_fingerprint(record)
  class(record) <- c("phase9_closure_evidence_bundle", "list")
  record
}

phase9_closure_report <- function(bundle) {
  if (!inherits(bundle, "phase9_closure_evidence_bundle")) {
    stop("bundle must be a Phase 9 closure evidence bundle.", call. = FALSE)
  }
  review <- bundle$closure_review
  c(
    "# Phase 9 closure review",
    "",
    sprintf("- Closure approved: `%s`", review$closure_approved),
    sprintf("- Milestones bound: `%d`", length(review$milestone_commits)),
    sprintf("- Evidence domains: `%d`", length(review$evidence_ids)),
    sprintf("- Unresolved blockers: `%d`", length(review$unresolved_blockers)),
    sprintf("- Closure fingerprint: `%s`", review$fingerprint),
    sprintf("- Next milestone: `%s`", bundle$roadmap_handoff$next_phase_id),
    "",
    "Closure remains fail-closed unless release readiness is true and the",
    "unresolved-blocker set is empty."
  )
}
