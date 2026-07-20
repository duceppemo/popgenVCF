#' Validate a runtime API manifest
#'
#' Validates the machine-readable contract describing runtime-facing symbols.
#' The manifest is intentionally strict so accidental exports, duplicate entries,
#' undocumented symbols, and unsupported stability classifications fail closed.
#'
#' @param manifest A data frame with one row per runtime API symbol.
#' @return The validated manifest, invisibly.
#' @export
validate_runtime_api_manifest <- function(manifest) {
  required <- c(
    "symbol", "stability", "signature_fingerprint",
    "documentation", "lifecycle_status"
  )

  if (!is.data.frame(manifest)) {
    stop("`manifest` must be a data frame.", call. = FALSE)
  }

  missing_columns <- setdiff(required, names(manifest))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "Runtime API manifest is missing required columns: %s.",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(manifest) == 0L) {
    stop("Runtime API manifest must contain at least one symbol.", call. = FALSE)
  }

  for (column in required) {
    values <- manifest[[column]]
    if (!is.character(values) || anyNA(values) || any(!nzchar(trimws(values)))) {
      stop(
        sprintf("Runtime API manifest column `%s` must contain non-empty strings.", column),
        call. = FALSE
      )
    }
  }

  duplicated_symbols <- unique(manifest$symbol[duplicated(manifest$symbol)])
  if (length(duplicated_symbols) > 0L) {
    stop(
      sprintf(
        "Runtime API manifest contains duplicate symbols: %s.",
        paste(duplicated_symbols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  allowed_stability <- c("stable", "experimental", "deprecated", "internal")
  unsupported_stability <- setdiff(unique(manifest$stability), allowed_stability)
  if (length(unsupported_stability) > 0L) {
    stop(
      sprintf(
        "Unsupported runtime API stability classes: %s.",
        paste(unsupported_stability, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  allowed_lifecycle <- c("active", "deprecated", "retired")
  unsupported_lifecycle <- setdiff(
    unique(manifest$lifecycle_status),
    allowed_lifecycle
  )
  if (length(unsupported_lifecycle) > 0L) {
    stop(
      sprintf(
        "Unsupported runtime API lifecycle states: %s.",
        paste(unsupported_lifecycle, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invalid_deprecation <- manifest$stability == "deprecated" &
    manifest$lifecycle_status != "deprecated"
  if (any(invalid_deprecation)) {
    stop(
      "Deprecated runtime API symbols must use lifecycle status `deprecated`.",
      call. = FALSE
    )
  }

  invisible(manifest)
}

#' Compare runtime API manifests
#'
#' Detects incompatible changes to stable runtime interfaces. Stable symbols may
#' not disappear or change signature without an explicit compatibility action.
#'
#' @param baseline Previously released validated runtime API manifest.
#' @param candidate Candidate validated runtime API manifest.
#' @return A list describing added, removed, and changed symbols.
#' @export
compare_runtime_api_manifests <- function(baseline, candidate) {
  validate_runtime_api_manifest(baseline)
  validate_runtime_api_manifest(candidate)

  baseline_stable <- baseline[baseline$stability == "stable", , drop = FALSE]
  candidate_symbols <- candidate$symbol

  removed <- setdiff(baseline_stable$symbol, candidate_symbols)
  if (length(removed) > 0L) {
    stop(
      sprintf(
        "Stable runtime API symbols were removed: %s.",
        paste(removed, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  common <- intersect(baseline_stable$symbol, candidate_symbols)
  baseline_index <- match(common, baseline$symbol)
  candidate_index <- match(common, candidate$symbol)
  changed <- common[
    baseline$signature_fingerprint[baseline_index] !=
      candidate$signature_fingerprint[candidate_index]
  ]

  if (length(changed) > 0L) {
    stop(
      sprintf(
        "Stable runtime API signatures changed: %s.",
        paste(changed, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    added = setdiff(candidate$symbol, baseline$symbol),
    removed = character(),
    changed = character()
  )
}
