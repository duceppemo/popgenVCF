#' Create a canonical release gate policy
#'
#' @param require_validation Require a canonical validation-suite result.
#' @param require_baselines Require a canonical baseline result.
#' @param require_drift Require a canonical drift assessment.
#' @param require_reconciliation Require scientific change reconciliation.
#' @return A validated `PopgenVCFCanonicalReleaseGatePolicy`.
#' @export
new_canonical_release_gate_policy <- function(require_validation = TRUE,
                                              require_baselines = TRUE,
                                              require_drift = TRUE,
                                              require_reconciliation = TRUE) {
  values <- c(validation = require_validation, baselines = require_baselines,
              drift = require_drift, reconciliation = require_reconciliation)
  if (!is.logical(values) || anyNA(values))
    stop("release gate policy values must be non-missing logicals", call. = FALSE)
  structure(list(schema_version = "1.0", required = values),
            class = "PopgenVCFCanonicalReleaseGatePolicy")
}

.validate_release_gate_policy <- function(policy) {
  if (!inherits(policy, "PopgenVCFCanonicalReleaseGatePolicy") ||
      !identical(policy$schema_version, "1.0") ||
      !identical(names(policy$required), c("validation", "baselines", "drift", "reconciliation")))
    stop("policy must be a canonical release gate policy", call. = FALSE)
  invisible(policy)
}

.release_gate_component <- function(component, result, required, passed, detail) {
  data.frame(component = component, required = required,
             supplied = !is.null(result), passed = passed,
             detail = detail, stringsAsFactors = FALSE)
}

#' Evaluate the canonical scientific release gate
#'
#' @param release_id Stable release candidate identifier.
#' @param validation Optional canonical validation-suite result.
#' @param baselines Optional canonical baseline result.
#' @param drift Optional canonical drift assessment.
#' @param reconciliation Optional canonical change reconciliation.
#' @param policy Canonical release gate policy.
#' @param evaluated_at ISO-8601 evaluation timestamp.
#' @param provenance Optional named provenance metadata.
#' @return A `PopgenVCFCanonicalReleaseGateResult`.
#' @export
evaluate_canonical_release_gate <- function(release_id, validation = NULL,
  baselines = NULL, drift = NULL, reconciliation = NULL,
  policy = new_canonical_release_gate_policy(), evaluated_at, provenance = list()) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  .validate_release_gate_policy(policy)
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance))))
    stop("provenance must be a named list", call. = FALSE)

  validation_valid <- !is.null(validation) &&
    inherits(validation, "PopgenVCFCanonicalValidationSuiteResult")
  validation_table <- if (validation_valid) canonical_validation_suite_table(validation) else NULL
  validation_pass <- validation_valid && nrow(validation_table) > 0L && all(validation_table$passed)
  baseline_pass <- !is.null(baselines) && inherits(baselines, "PopgenVCFCanonicalBaselineResult") &&
    isTRUE(baselines$passed)
  reconciliation_pass <- !is.null(reconciliation) &&
    inherits(reconciliation, "PopgenVCFCanonicalChangeReconciliation") &&
    isTRUE(reconciliation$release_ready)
  drift_valid <- !is.null(drift) && inherits(drift, "PopgenVCFCanonicalDriftAssessment")
  drift_pass <- drift_valid && (identical(drift$classification, "stable") || reconciliation_pass)

  supplied <- list(validation = validation, baselines = baselines,
                   drift = drift, reconciliation = reconciliation)
  passes <- c(validation = validation_pass, baselines = baseline_pass,
              drift = drift_pass, reconciliation = reconciliation_pass)
  details <- c(
    validation = if (is.null(validation)) "validation result missing" else if (!validation_valid)
      "validation result has invalid type" else if (!nrow(validation_table))
      "canonical validation suite contains no executed datasets" else if (validation_pass)
      "canonical validation suite passed" else "canonical validation suite failed",
    baselines = if (is.null(baselines)) "baseline result missing" else if (baseline_pass)
      "canonical baselines conform" else "canonical baseline comparison failed or has invalid type",
    drift = if (is.null(drift)) "drift assessment missing" else if (!drift_valid)
      "drift assessment has invalid type" else if (identical(drift$classification, "stable"))
      "canonical drift is stable" else if (reconciliation_pass)
      paste0("canonical drift classified ", drift$classification, " and formally reconciled") else
      paste0("canonical drift classified ", drift$classification, " without release-ready reconciliation"),
    reconciliation = if (is.null(reconciliation)) "change reconciliation missing" else if (reconciliation_pass)
      "scientific change reconciliation is release ready" else
      "scientific change reconciliation failed or has invalid type"
  )

  rows <- lapply(names(policy$required), function(component) {
    required <- unname(policy$required[[component]])
    present <- !is.null(supplied[[component]])
    passed <- if (required) isTRUE(passes[[component]]) else if (present) isTRUE(passes[[component]]) else TRUE
    .release_gate_component(component, supplied[[component]], required, passed, details[[component]])
  })
  components <- do.call(rbind, rows)
  rownames(components) <- NULL
  blocking <- components[components$required & !components$passed,
                         c("component", "detail"), drop = FALSE]
  if (nrow(blocking)) blocking$reason <- paste0(blocking$component, ": ", blocking$detail)
  else blocking$reason <- character()
  release_ready <- nrow(blocking) == 0L

  structure(list(schema_version = "1.0", release_id = scalar(release_id, "release_id"),
    evaluated_at = scalar(evaluated_at, "evaluated_at"), policy = policy,
    components = components, blocking_reasons = blocking,
    release_ready = release_ready, provenance = provenance),
    class = "PopgenVCFCanonicalReleaseGateResult")
}

#' Return canonical release gate component status
#' @param result Canonical release gate result.
#' @return Deterministically ordered component table.
#' @export
canonical_release_gate_table <- function(result) {
  if (!inherits(result, "PopgenVCFCanonicalReleaseGateResult"))
    stop("result must be a canonical release gate result", call. = FALSE)
  component_order <- c("validation", "baselines", "drift", "reconciliation")
  out <- result$components[match(component_order, result$components$component), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Create a canonical release certificate
#' @param result Canonical release gate result.
#' @return Deterministic release certificate list.
#' @export
canonical_release_certificate <- function(result) {
  table <- canonical_release_gate_table(result)
  list(schema_version = "1.0", release_id = result$release_id,
       evaluated_at = result$evaluated_at, release_ready = result$release_ready,
       required_components = table$component[table$required],
       passed_components = table$component[table$passed],
       blocking_reasons = unname(result$blocking_reasons$reason),
       provenance = result$provenance)
}

#' Write canonical release gate evidence
#' @param result Canonical release gate result.
#' @param output_dir Evidence directory.
#' @return Named normalized paths.
#' @export
write_canonical_release_gate_evidence <- function(result, output_dir) {
  table <- canonical_release_gate_table(result)
  certificate <- canonical_release_certificate(result)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(components = file.path(output_dir, "canonical_release_gate_components.tsv"),
    blocking = file.path(output_dir, "canonical_release_gate_blocking.tsv"),
    certificate = file.path(output_dir, "canonical_release_certificate.json"),
    report = file.path(output_dir, "canonical_release_gate.md"))
  data.table::fwrite(table, paths[["components"]], sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(result$blocking_reasons, paths[["blocking"]], sep = "\t", quote = FALSE, na = "NA")
  jsonlite::write_json(certificate, paths[["certificate"]], auto_unbox = TRUE,
                       pretty = TRUE, na = "null")
  status <- if (result$release_ready) "READY" else "BLOCKED"
  lines <- c(paste0("# Canonical release gate: ", status), "",
    paste0("Release: `", result$release_id, "`"),
    paste0("Evaluated: `", result$evaluated_at, "`"), "",
    paste0("Required components passed: ", sum(table$required & table$passed),
           "/", sum(table$required), "."))
  if (nrow(result$blocking_reasons)) {
    lines <- c(lines, "", "## Blocking reasons", "",
               paste0("- ", result$blocking_reasons$reason))
  }
  writeLines(lines, paths[["report"]], useBytes = TRUE)
  vapply(paths, normalizePath, character(1))
}
