# Phase 10.1.5 — public API closure audit and roadmap synchronization

.phase10_1_milestone_commits <- c(
  "10.1" = "b326a095b2a2d09c7fa3ff329513990aad5d095c",
  "10.1.1" = "1b4f5f0f76bd34fd262467269fbed47d5e96ba56",
  "10.1.2" = "3512669e2297457dadebcc306c0f04450d60747b",
  "10.1.3" = "b2826c873307159691766cedaadd6317411cde1e",
  "10.1.4" = "39e3892c65343bd064aa2746f71cd5ac4094aec6"
)

.phase10_1_public_surface <- data.frame(
  operation_id = c(
    "analysis.execute", "artifact.list", "provenance.inspect",
    "report.render", "result.inspect"
  ),
  adapter = c(
    "execute_public_analysis", "list_public_artifacts",
    "inspect_public_provenance", "render_public_report",
    "inspect_public_result"
  ),
  documentation = c(
    "execute_public_analysis", "list_public_artifacts",
    "inspect_public_provenance", "render_public_report",
    "inspect_public_result"
  ),
  stringsAsFactors = FALSE
)

#' Return the Phase 10.1 public-surface manifest
#'
#' @return A deterministic data frame binding public operations to adapters and
#'   documentation targets.
#' @export
phase10_1_public_surface_manifest <- function() {
  .phase10_1_public_surface[order(.phase10_1_public_surface$operation_id), , drop = FALSE]
}

#' Audit the completed Phase 10.1 public API surface
#'
#' @param descriptor A validated Phase 10 public API descriptor.
#' @param exported_symbols Character vector of exported package symbols.
#' @param available_symbols Character vector of symbols available in the package
#'   namespace.
#' @return A deterministic closure-audit record.
#' @export
phase10_1_audit_public_surface <- function(
    descriptor = phase10_api_descriptor(),
    exported_symbols = getNamespaceExports("popgenVCF"),
    available_symbols = ls(asNamespace("popgenVCF"), all.names = TRUE)) {
  validate_phase10_api_descriptor(descriptor)
  manifest <- phase10_1_public_surface_manifest()
  operations <- phase10_api_operations(descriptor)

  if (anyDuplicated(manifest$operation_id) || anyDuplicated(manifest$adapter)) {
    stop("Phase 10.1 public-surface manifest contains duplicates.", call. = FALSE)
  }
  if (!identical(sort(operations$operation_id), manifest$operation_id)) {
    stop("Phase 10.1 operation registry does not match the public-surface manifest.", call. = FALSE)
  }
  if (any(operations$lifecycle != "stable")) {
    stop("Phase 10.1 closure requires every public operation to be stable.", call. = FALSE)
  }
  if (any(!nzchar(operations$request_schema)) || any(!nzchar(operations$response_schema))) {
    stop("Phase 10.1 operations require request and response schemas.", call. = FALSE)
  }

  missing_adapters <- setdiff(manifest$adapter, available_symbols)
  missing_exports <- setdiff(manifest$adapter, exported_symbols)
  if (length(missing_adapters)) {
    stop(sprintf(
      "Missing Phase 10.1 adapter(s): %s",
      paste(sort(missing_adapters), collapse = ", ")
    ), call. = FALSE)
  }
  if (length(missing_exports)) {
    stop(sprintf(
      "Unexported Phase 10.1 adapter(s): %s",
      paste(sort(missing_exports), collapse = ", ")
    ), call. = FALSE)
  }

  audit <- list(
    record_type = "phase10_1_public_surface_audit",
    schema_version = "1.0.0",
    api_version = descriptor$api_version,
    descriptor_fingerprint = descriptor$fingerprint,
    operations = operations[order(operations$operation_id), , drop = FALSE],
    surface = manifest,
    passed = TRUE
  )
  audit$fingerprint <- phase10_public_fingerprint(audit)
  class(audit) <- c("PopgenVCFPhase10PublicSurfaceAudit", "list")
  audit
}

#' Assemble deterministic Phase 10.1 closure evidence
#'
#' @param ci_evidence_id Immutable identity for the green authoritative CI run.
#' @param unresolved_blockers Character vector of unresolved closure blockers.
#' @param audit A successful Phase 10.1 public-surface audit.
#' @return A deterministic Phase 10.1 closure evidence record.
#' @export
phase10_1_closure_evidence <- function(
    ci_evidence_id,
    unresolved_blockers = character(),
    audit = phase10_1_audit_public_surface()) {
  if (!is.character(ci_evidence_id) || length(ci_evidence_id) != 1L ||
      is.na(ci_evidence_id) || !nzchar(ci_evidence_id)) {
    stop("ci_evidence_id must be one non-empty immutable identity.", call. = FALSE)
  }
  if (!inherits(audit, "PopgenVCFPhase10PublicSurfaceAudit") || !isTRUE(audit$passed)) {
    stop("audit must be a successful Phase 10.1 public-surface audit.", call. = FALSE)
  }
  expected <- audit$fingerprint
  candidate <- audit
  candidate$fingerprint <- NULL
  if (!identical(expected, phase10_public_fingerprint(candidate))) {
    stop("Phase 10.1 public-surface audit fingerprint mismatch.", call. = FALSE)
  }

  blockers <- sort(unique(as.character(unresolved_blockers)))
  blockers <- blockers[nzchar(blockers)]
  record <- list(
    record_type = "phase10_1_closure_evidence",
    schema_version = "1.0.0",
    milestone_commits = .phase10_1_milestone_commits,
    public_surface_audit_id = audit$fingerprint,
    api_descriptor_id = audit$descriptor_fingerprint,
    ci_evidence_id = ci_evidence_id,
    unresolved_blockers = blockers,
    closure_approved = length(blockers) == 0L,
    next_milestone = "10.2"
  )
  record$fingerprint <- phase10_public_fingerprint(record)
  class(record) <- c("PopgenVCFPhase10ClosureEvidence", "list")
  record
}

#' Render a Phase 10.1 closure report
#'
#' @param closure A Phase 10.1 closure evidence record.
#' @return Character lines containing a Markdown closure report.
#' @export
phase10_1_closure_report <- function(closure) {
  if (!inherits(closure, "PopgenVCFPhase10ClosureEvidence")) {
    stop("closure must be a Phase 10.1 closure evidence record.", call. = FALSE)
  }
  candidate <- closure
  fingerprint <- candidate$fingerprint
  candidate$fingerprint <- NULL
  if (!identical(fingerprint, phase10_public_fingerprint(candidate))) {
    stop("Phase 10.1 closure evidence fingerprint mismatch.", call. = FALSE)
  }
  c(
    "# Phase 10.1 closure review",
    "",
    sprintf("- Closure approved: `%s`", closure$closure_approved),
    sprintf("- Milestones bound: `%d`", length(closure$milestone_commits)),
    sprintf("- Public operations: `%d`", nrow(phase10_1_public_surface_manifest())),
    sprintf("- Unresolved blockers: `%d`", length(closure$unresolved_blockers)),
    sprintf("- Closure fingerprint: `%s`", closure$fingerprint),
    sprintf("- Next milestone: `%s`", closure$next_milestone)
  )
}
