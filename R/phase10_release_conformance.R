# Phase 10.2.3 - deterministic release conformance manifests and gating

#' Create a deterministic release-channel identity
#'
#' @param channel Release channel identifier.
#' @param release_version Package release semantic version.
#' @param artifact_digest Stable artifact digest.
#' @param api_fingerprint Canonical public API descriptor fingerprint.
#' @return A one-row release identity data frame.
#' @export
new_phase10_release_identity <- function(
    channel,
    release_version,
    artifact_digest,
    api_fingerprint) {
  .phase10_scalar_string(channel, "channel")
  .phase10_validate_semver(release_version, "release_version")
  .phase10_scalar_string(artifact_digest, "artifact_digest")
  .phase10_scalar_string(api_fingerprint, "api_fingerprint")
  if (!channel %in% .phase10_required_release_channels()) {
    stop("Unsupported Phase 10 release channel.", call. = FALSE)
  }
  data.frame(
    channel = channel,
    release_version = release_version,
    artifact_digest = artifact_digest,
    api_fingerprint = api_fingerprint,
    stringsAsFactors = FALSE
  )
}

#' Build a deterministic public API release-conformance manifest
#'
#' @param descriptor Candidate public API descriptor.
#' @param compatibility Phase 10.2.1 compatibility record.
#' @param migration_plan Phase 10.2.2 migration plan.
#' @param identities Release-channel identity rows.
#' @param policy Phase 10.2.2 evolution policy.
#' @param allow_breaking Whether a separately reviewed breaking release is approved.
#' @return A deterministic release-conformance manifest.
#' @export
new_phase10_release_conformance <- function(
    descriptor,
    compatibility,
    migration_plan,
    identities,
    policy = phase10_api_evolution_policy(),
    allow_breaking = FALSE) {
  validate_phase10_api_descriptor(descriptor)
  validate_phase10_api_compatibility(compatibility, allow_breaking = TRUE)
  validate_phase10_api_migration_plan(migration_plan, compatibility, policy)
  .phase10_validate_release_identities(identities, descriptor)
  if (!is.logical(allow_breaking) || length(allow_breaking) != 1L || is.na(allow_breaking)) {
    stop("allow_breaking must be TRUE or FALSE.", call. = FALSE)
  }
  identities <- identities[order(identities$channel), , drop = FALSE]
  rownames(identities) <- NULL
  blockers <- character()
  if (identical(compatibility$classification, "breaking") && !allow_breaking) {
    blockers <- c(blockers, "breaking_change_requires_explicit_approval")
  }
  manifest <- list(
    record_type = "popgenvcf_public_api_release_conformance",
    schema_version = "1.0.0",
    release_version = unique(identities$release_version),
    api_version = descriptor$api_version,
    descriptor_fingerprint = descriptor$fingerprint,
    compatibility_fingerprint = compatibility$fingerprint,
    migration_plan_fingerprint = migration_plan$fingerprint,
    policy_fingerprint = policy$fingerprint,
    release_identities = identities,
    breaking_approved = isTRUE(allow_breaking),
    blockers = sort(unique(blockers)),
    release_ready = length(blockers) == 0L
  )
  manifest$fingerprint <- phase10_public_fingerprint(manifest)
  class(manifest) <- c("PopgenVCFPublicAPIReleaseConformance", "list")
  validate_phase10_release_conformance(
    manifest, descriptor, compatibility, migration_plan, policy
  )
  manifest
}

#' Validate a public API release-conformance manifest
#'
#' @param manifest Release-conformance manifest.
#' @param descriptor Candidate public API descriptor.
#' @param compatibility Compatibility record.
#' @param migration_plan Migration plan.
#' @param policy API evolution policy.
#' @return `TRUE`, invisibly.
#' @export
validate_phase10_release_conformance <- function(
    manifest,
    descriptor,
    compatibility,
    migration_plan,
    policy = phase10_api_evolution_policy()) {
  if (!inherits(manifest, "PopgenVCFPublicAPIReleaseConformance")) {
    stop("manifest must be a public API release-conformance manifest.", call. = FALSE)
  }
  validate_phase10_api_descriptor(descriptor)
  validate_phase10_api_compatibility(compatibility, allow_breaking = TRUE)
  validate_phase10_api_migration_plan(migration_plan, compatibility, policy)
  .phase10_validate_release_identities(manifest$release_identities, descriptor)
  expected_bindings <- list(
    descriptor$fingerprint,
    compatibility$fingerprint,
    migration_plan$fingerprint,
    policy$fingerprint
  )
  actual_bindings <- list(
    manifest$descriptor_fingerprint,
    manifest$compatibility_fingerprint,
    manifest$migration_plan_fingerprint,
    manifest$policy_fingerprint
  )
  if (!identical(actual_bindings, expected_bindings)) {
    stop("Release conformance evidence bindings do not match.", call. = FALSE)
  }
  expected <- phase10_public_fingerprint(manifest)
  if (!identical(manifest$fingerprint, expected)) {
    stop("Release conformance fingerprint verification failed.", call. = FALSE)
  }
  expected_blocked <- identical(compatibility$classification, "breaking") &&
    !isTRUE(manifest$breaking_approved)
  if (!identical(manifest$release_ready, !expected_blocked)) {
    stop("Release readiness does not agree with compatibility approval.", call. = FALSE)
  }
  if (expected_blocked &&
      !"breaking_change_requires_explicit_approval" %in% manifest$blockers) {
    stop("Breaking release blocker is missing.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Enforce the Phase 10 public API release gate
#'
#' @param manifest Validated release-conformance manifest.
#' @param descriptor Candidate public API descriptor.
#' @param compatibility Compatibility record.
#' @param migration_plan Migration plan.
#' @param policy API evolution policy.
#' @return `TRUE`, invisibly, when the release is conformant.
#' @export
assert_phase10_release_conformance <- function(
    manifest,
    descriptor,
    compatibility,
    migration_plan,
    policy = phase10_api_evolution_policy()) {
  validate_phase10_release_conformance(
    manifest, descriptor, compatibility, migration_plan, policy
  )
  if (!isTRUE(manifest$release_ready)) {
    stop(
      sprintf("Phase 10 release conformance failed: %s",
              paste(manifest$blockers, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Render a deterministic release-conformance report
#'
#' @param manifest Release-conformance manifest.
#' @param descriptor Candidate public API descriptor.
#' @param compatibility Compatibility record.
#' @param migration_plan Migration plan.
#' @param policy API evolution policy.
#' @return Character vector containing Markdown report lines.
#' @export
phase10_release_conformance_report <- function(
    manifest,
    descriptor,
    compatibility,
    migration_plan,
    policy = phase10_api_evolution_policy()) {
  validate_phase10_release_conformance(
    manifest, descriptor, compatibility, migration_plan, policy
  )
  rows <- apply(manifest$release_identities, 1L, function(x) {
    sprintf("- `%s`: `%s`", x[["channel"]], x[["artifact_digest"]])
  })
  blockers <- if (length(manifest$blockers)) {
    paste(manifest$blockers, collapse = ", ")
  } else {
    "none"
  }
  c(
    "# Phase 10 public API release conformance",
    "",
    sprintf("Release version: `%s`", manifest$release_version),
    sprintf("API version: `%s`", manifest$api_version),
    sprintf("Compatibility: **%s**", compatibility$classification),
    sprintf("Release ready: `%s`", tolower(as.character(manifest$release_ready))),
    sprintf("Blockers: `%s`", blockers),
    sprintf("Fingerprint: `%s`", manifest$fingerprint),
    "", "## Release identities", "", rows
  )
}

.phase10_required_release_channels <- function() {
  c("package", "container", "apptainer", "documentation", "scientific_validation")
}

.phase10_validate_release_identities <- function(identities, descriptor) {
  required <- c("channel", "release_version", "artifact_digest", "api_fingerprint")
  if (!is.data.frame(identities) || !identical(names(identities), required) ||
      anyDuplicated(identities$channel)) {
    stop("Malformed or duplicate release identities.", call. = FALSE)
  }
  expected_channels <- sort(.phase10_required_release_channels())
  if (!identical(sort(identities$channel), expected_channels)) {
    stop("Release identities must cover every required release channel exactly once.", call. = FALSE)
  }
  if (length(unique(identities$release_version)) != 1L) {
    stop("Release channels disagree on the release version.", call. = FALSE)
  }
  invisible(lapply(identities$release_version, .phase10_validate_semver,
                   name = "release_version"))
  if (any(!nzchar(identities$artifact_digest))) {
    stop("Release artifact digests must be non-empty.", call. = FALSE)
  }
  if (any(identities$api_fingerprint != descriptor$fingerprint)) {
    stop("Release channels disagree with the canonical public API fingerprint.", call. = FALSE)
  }
  invisible(TRUE)
}
