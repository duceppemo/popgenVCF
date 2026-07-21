# Phase 10.2.2 - deterministic API evolution and migration planning

#' Create the canonical public API evolution policy
#'
#' @param minimum_deprecation_minor_releases Minimum supported minor releases
#'   between deprecation and removal.
#' @return A deterministic API evolution policy record.
#' @export
phase10_api_evolution_policy <- function(minimum_deprecation_minor_releases = 2L) {
  if (!is.numeric(minimum_deprecation_minor_releases) ||
      length(minimum_deprecation_minor_releases) != 1L ||
      is.na(minimum_deprecation_minor_releases) ||
      minimum_deprecation_minor_releases < 1 ||
      minimum_deprecation_minor_releases != as.integer(minimum_deprecation_minor_releases)) {
    stop("minimum_deprecation_minor_releases must be a positive integer.", call. = FALSE)
  }
  policy <- list(
    record_type = "popgenvcf_public_api_evolution_policy",
    schema_version = "1.0.0",
    minimum_deprecation_minor_releases = as.integer(minimum_deprecation_minor_releases),
    additive_change = "document",
    deprecated_change = "migration_required",
    breaking_change = "explicit_approval_and_migration_required",
    removal_without_deprecation = "reject"
  )
  policy$fingerprint <- phase10_public_fingerprint(policy)
  class(policy) <- c("PopgenVCFPublicAPIEvolutionPolicy", "list")
  policy
}

#' Create deterministic public API migration guidance
#'
#' @param operation_id Public operation identifier.
#' @param action Migration action: `none`, `adopt`, `migrate`, or `replace`.
#' @param successor_operation Optional successor operation identifier.
#' @param schema_guidance Stable schema migration guidance.
#' @param deprecated_in Optional API version where deprecation begins.
#' @param removal_not_before Optional earliest removal API version.
#' @return A one-row migration-guidance data frame.
#' @export
new_phase10_migration_guidance <- function(
    operation_id,
    action,
    successor_operation = NA_character_,
    schema_guidance = "No migration required.",
    deprecated_in = NA_character_,
    removal_not_before = NA_character_) {
  .phase10_scalar_string(operation_id, "operation_id")
  .phase10_scalar_string(action, "action")
  if (!action %in% c("none", "adopt", "migrate", "replace")) {
    stop("Unsupported migration action.", call. = FALSE)
  }
  values <- list(successor_operation, deprecated_in, removal_not_before)
  if (any(vapply(values, function(x) length(x) != 1L || (!is.na(x) && !is.character(x)), logical(1)))) {
    stop("Optional migration fields must be scalar character values or NA.", call. = FALSE)
  }
  .phase10_scalar_string(schema_guidance, "schema_guidance")
  if (!is.na(deprecated_in)) .phase10_validate_semver(deprecated_in, "deprecated_in")
  if (!is.na(removal_not_before)) .phase10_validate_semver(removal_not_before, "removal_not_before")
  data.frame(
    operation_id = operation_id,
    action = action,
    successor_operation = successor_operation,
    schema_guidance = schema_guidance,
    deprecated_in = deprecated_in,
    removal_not_before = removal_not_before,
    stringsAsFactors = FALSE
  )
}

#' Build a deterministic public API migration plan
#'
#' @param compatibility A validated Phase 10.2.1 compatibility record.
#' @param guidance Migration-guidance rows.
#' @param policy API evolution policy.
#' @return A deterministic migration-plan record.
#' @export
new_phase10_api_migration_plan <- function(
    compatibility,
    guidance,
    policy = phase10_api_evolution_policy()) {
  validate_phase10_api_compatibility(compatibility, allow_breaking = TRUE)
  .phase10_validate_evolution_policy(policy)
  required <- c(
    "operation_id", "action", "successor_operation", "schema_guidance",
    "deprecated_in", "removal_not_before"
  )
  if (!is.data.frame(guidance) || !identical(names(guidance), required) ||
      anyDuplicated(guidance$operation_id)) {
    stop("Malformed or duplicate migration guidance.", call. = FALSE)
  }
  guidance <- guidance[order(guidance$operation_id), , drop = FALSE]
  plan <- list(
    record_type = "popgenvcf_public_api_migration_plan",
    schema_version = "1.0.0",
    baseline_api_version = compatibility$baseline_api_version,
    candidate_api_version = compatibility$candidate_api_version,
    compatibility_fingerprint = compatibility$fingerprint,
    policy_fingerprint = policy$fingerprint,
    classification = compatibility$classification,
    guidance = guidance
  )
  plan$fingerprint <- phase10_public_fingerprint(plan)
  class(plan) <- c("PopgenVCFPublicAPIMigrationPlan", "list")
  validate_phase10_api_migration_plan(plan, compatibility, policy)
  plan
}

#' Validate a public API migration plan
#'
#' @param plan Migration plan.
#' @param compatibility Originating compatibility record.
#' @param policy API evolution policy.
#' @return `TRUE`, invisibly.
#' @export
validate_phase10_api_migration_plan <- function(
    plan,
    compatibility,
    policy = phase10_api_evolution_policy()) {
  if (!inherits(plan, "PopgenVCFPublicAPIMigrationPlan")) {
    stop("plan must be a public API migration plan.", call. = FALSE)
  }
  validate_phase10_api_compatibility(compatibility, allow_breaking = TRUE)
  .phase10_validate_evolution_policy(policy)
  if (!identical(plan$compatibility_fingerprint, compatibility$fingerprint) ||
      !identical(plan$policy_fingerprint, policy$fingerprint)) {
    stop("Migration plan is not bound to the supplied compatibility evidence and policy.", call. = FALSE)
  }
  expected <- phase10_public_fingerprint(plan)
  if (!identical(plan$fingerprint, expected)) {
    stop("Public API migration plan fingerprint verification failed.", call. = FALSE)
  }
  changed <- compatibility$changes[
    compatibility$changes$classification %in% c("deprecated", "breaking"), , drop = FALSE
  ]
  for (id in changed$operation_id) {
    row <- plan$guidance[plan$guidance$operation_id == id, , drop = FALSE]
    if (nrow(row) != 1L || !row$action %in% c("migrate", "replace") ||
        !nzchar(row$schema_guidance)) {
      stop(sprintf("Operation %s requires a documented migration path.", id), call. = FALSE)
    }
    if (identical(row$action, "replace") &&
        (is.na(row$successor_operation) || !nzchar(row$successor_operation))) {
      stop(sprintf("Operation %s requires a successor operation.", id), call. = FALSE)
    }
    if (identical(changed$classification[changed$operation_id == id], "deprecated")) {
      if (is.na(row$deprecated_in) || is.na(row$removal_not_before)) {
        stop(sprintf("Deprecated operation %s requires a removal schedule.", id), call. = FALSE)
      }
      .phase10_validate_deprecation_window(
        row$deprecated_in, row$removal_not_before,
        policy$minimum_deprecation_minor_releases
      )
    }
  }
  invisible(TRUE)
}

#' Render a deterministic public API migration report
#'
#' @param plan Validated migration plan.
#' @param compatibility Originating compatibility record.
#' @param policy API evolution policy.
#' @return Character vector containing Markdown report lines.
#' @export
phase10_api_migration_report <- function(
    plan,
    compatibility,
    policy = phase10_api_evolution_policy()) {
  validate_phase10_api_migration_plan(plan, compatibility, policy)
  rows <- apply(plan$guidance, 1L, function(x) {
    successor <- if (is.na(x[["successor_operation"]])) "none" else x[["successor_operation"]]
    sprintf("- `%s`: **%s**; successor: `%s`; %s",
            x[["operation_id"]], x[["action"]], successor, x[["schema_guidance"]])
  })
  c(
    "# Phase 10 public API migration plan",
    "",
    sprintf("Baseline API: `%s`", plan$baseline_api_version),
    sprintf("Candidate API: `%s`", plan$candidate_api_version),
    sprintf("Classification: **%s**", plan$classification),
    sprintf("Fingerprint: `%s`", plan$fingerprint),
    "", "## Migration guidance", "", rows
  )
}

.phase10_validate_evolution_policy <- function(policy) {
  if (!inherits(policy, "PopgenVCFPublicAPIEvolutionPolicy") ||
      !identical(policy$fingerprint, phase10_public_fingerprint(policy))) {
    stop("Invalid public API evolution policy.", call. = FALSE)
  }
  invisible(TRUE)
}

.phase10_validate_deprecation_window <- function(deprecated_in, removal_not_before, minimum_minor) {
  old <- as.integer(strsplit(deprecated_in, ".", fixed = TRUE)[[1L]])
  new <- as.integer(strsplit(removal_not_before, ".", fixed = TRUE)[[1L]])
  if (new[[1L]] < old[[1L]] ||
      (new[[1L]] == old[[1L]] && new[[2L]] - old[[2L]] < minimum_minor)) {
    stop("Deprecation removal schedule is shorter than policy allows.", call. = FALSE)
  }
  invisible(TRUE)
}
