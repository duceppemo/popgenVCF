ancestry_three_backend_scalar <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  trimws(x)
}

ancestry_three_backend_iso_timestamp <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", x)) {
    stop(label, " must be an ISO-8601 UTC timestamp", call. = FALSE)
  }
  x
}

ancestry_three_backend_iso_date <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    stop(label, " must be an ISO-8601 date", call. = FALSE)
  }
  x
}

#' Create ancestry backend evidence for one backend's K sweep
#'
#' Summarizes one backend's replicate stability and fit-metric behavior
#' across its declared K range. This does not retain raw Q matrices; those
#' remain in the run's own evidence directory. Stability and alignment
#' figures come from `consensus_ancestry()` run separately for each K.
#'
#' @param backend One of `admixture`, `faststructure`, or `snmf`.
#' @param tool_version Non-empty backend tool/package version string.
#' @param command Representative command or invocation template.
#' @param k_values Sorted, unique integer K values, each at least two.
#' @param replicates Positive integer replicate count per K.
#' @param seed Base deterministic seed.
#' @param metric_name Backend fit-metric name, or `NA` if unavailable.
#' @param stability_by_k Data frame with one row per `k_values` entry and
#'   columns `k`, `metric_mean`, `global_stability`, `mean_alignment_score`.
#' @param provenance Named list of additional provenance fields.
#' @return A validated `PopgenVCFAncestryBackendEvidence`.
#' @export
new_ancestry_backend_evidence <- function(
    backend, tool_version, command, k_values, replicates, seed,
    metric_name, stability_by_k, provenance = list()) {
  backend <- tolower(ancestry_three_backend_scalar(backend, "backend"))
  if (!backend %in% c("admixture", "faststructure", "snmf")) {
    stop("backend must be one of admixture, faststructure, or snmf", call. = FALSE)
  }
  tool_version <- ancestry_three_backend_scalar(tool_version, "tool_version")
  command <- ancestry_three_backend_scalar(command, "command")
  k_values <- sort(unique(as.integer(k_values)))
  if (!length(k_values) || anyNA(k_values) || any(k_values < 2L)) {
    stop("k_values must contain unique integers of at least two", call. = FALSE)
  }
  replicates <- as.integer(replicates)[1L]
  if (is.na(replicates) || replicates < 1L) stop("replicates must be a positive integer", call. = FALSE)
  seed <- as.integer(seed)[1L]
  if (is.na(seed)) stop("seed must be a finite integer", call. = FALSE)
  metric_name <- if (is.null(metric_name) || is.na(metric_name)) {
    NA_character_
  } else {
    ancestry_three_backend_scalar(metric_name, "metric_name")
  }
  stability_by_k <- as.data.frame(stability_by_k, stringsAsFactors = FALSE)
  provenance <- as.list(provenance)
  if (length(provenance) && (is.null(names(provenance)) || any(!nzchar(names(provenance))))) {
    stop("provenance must be a named list", call. = FALSE)
  }
  x <- structure(list(
    schema_version = "1.0",
    backend = backend,
    tool_version = tool_version,
    command = command,
    k_values = k_values,
    replicates = replicates,
    seed = seed,
    metric_name = metric_name,
    stability_by_k = stability_by_k,
    provenance = provenance[sort(names(provenance))]
  ), class = "PopgenVCFAncestryBackendEvidence")
  validate_ancestry_backend_evidence(x)
}

#' Validate ancestry backend evidence
#' @param x A `PopgenVCFAncestryBackendEvidence`.
#' @return `x`, invisibly.
#' @export
validate_ancestry_backend_evidence <- function(x) {
  if (!inherits(x, "PopgenVCFAncestryBackendEvidence")) {
    stop("x must be a PopgenVCFAncestryBackendEvidence", call. = FALSE)
  }
  required <- c("schema_version", "backend", "tool_version", "command", "k_values",
                "replicates", "seed", "metric_name", "stability_by_k", "provenance")
  if (!all(required %in% names(x)) || !identical(x$schema_version, "1.0")) {
    stop("invalid ancestry backend evidence schema", call. = FALSE)
  }
  if (!x$backend %in% c("admixture", "faststructure", "snmf")) {
    stop("invalid ancestry backend name", call. = FALSE)
  }
  ancestry_three_backend_scalar(x$tool_version, "tool_version")
  ancestry_three_backend_scalar(x$command, "command")
  if (!is.integer(x$k_values) || !length(x$k_values) || anyNA(x$k_values) ||
      any(x$k_values < 2L) || anyDuplicated(x$k_values) ||
      !identical(x$k_values, sort(x$k_values))) {
    stop("k_values must be sorted, unique integers of at least two", call. = FALSE)
  }
  if (!is.integer(x$replicates) || length(x$replicates) != 1L || is.na(x$replicates) || x$replicates < 1L) {
    stop("replicates must be a positive integer", call. = FALSE)
  }
  if (!is.integer(x$seed) || length(x$seed) != 1L || is.na(x$seed)) {
    stop("seed must be a finite integer", call. = FALSE)
  }
  if (!is.na(x$metric_name)) ancestry_three_backend_scalar(x$metric_name, "metric_name")
  required_cols <- c("k", "metric_mean", "global_stability", "mean_alignment_score")
  if (!is.data.frame(x$stability_by_k) || !identical(names(x$stability_by_k), required_cols) ||
      !identical(nrow(x$stability_by_k), length(x$k_values)) ||
      !identical(as.integer(x$stability_by_k$k), x$k_values)) {
    stop("stability_by_k must have one ordered row per k_values entry", call. = FALSE)
  }
  if (any(!is.finite(x$stability_by_k$global_stability)) ||
      any(x$stability_by_k$global_stability < 0 | x$stability_by_k$global_stability > 1)) {
    stop("stability_by_k$global_stability must lie in [0, 1]", call. = FALSE)
  }
  if (!is.list(x$provenance) ||
      (length(x$provenance) && !identical(names(x$provenance), sort(names(x$provenance))))) {
    stop("provenance must be an alphabetically ordered named list", call. = FALSE)
  }
  invisible(x)
}

#' Create a cross-backend Q-matrix comparison record
#'
#' @param backend_a,backend_b Distinct backend names being compared.
#' @param k Shared K value at which both backends were compared.
#' @param alignment Alignment result from `align_ancestry_replicate()`, or a
#'   list with `alignment_score`, `correlation_score`, `cosine_score`, `rmsd`.
#' @param minimum_alignment_score Minimum acceptable alignment score.
#' @param role `equivalence` for release-gating comparisons or `diagnostic`
#'   for transparent non-gating context.
#' @param interpretation Scientific interpretation of the comparison.
#' @return A validated `PopgenVCFAncestryCrossBackendComparison`.
#' @export
new_ancestry_cross_backend_comparison <- function(
    backend_a, backend_b, k, alignment, minimum_alignment_score,
    role = c("equivalence", "diagnostic"), interpretation = "") {
  backend_a <- tolower(ancestry_three_backend_scalar(backend_a, "backend_a"))
  backend_b <- tolower(ancestry_three_backend_scalar(backend_b, "backend_b"))
  if (!backend_a %in% c("admixture", "faststructure", "snmf") ||
      !backend_b %in% c("admixture", "faststructure", "snmf")) {
    stop("backend_a and backend_b must be admixture, faststructure, or snmf", call. = FALSE)
  }
  if (identical(backend_a, backend_b)) stop("backend_a and backend_b must differ", call. = FALSE)
  k <- as.integer(k)[1L]
  if (is.na(k) || k < 2L) stop("k must be an integer of at least two", call. = FALSE)
  role <- match.arg(role)
  minimum_alignment_score <- as.numeric(minimum_alignment_score)[1L]
  if (!is.finite(minimum_alignment_score) || minimum_alignment_score < 0 || minimum_alignment_score > 1) {
    stop("minimum_alignment_score must lie in [0, 1]", call. = FALSE)
  }
  scores <- list(
    alignment_score = as.numeric(alignment$alignment_score)[1L],
    correlation_score = as.numeric(alignment$correlation_score)[1L],
    cosine_score = as.numeric(alignment$cosine_score)[1L],
    rmsd = as.numeric(alignment$rmsd)[1L]
  )
  if (any(!vapply(scores[c("alignment_score", "correlation_score", "cosine_score", "rmsd")],
                   function(v) is.finite(v), logical(1L)))) {
    stop("alignment scores must be finite", call. = FALSE)
  }
  ordered <- sort(c(backend_a, backend_b))
  x <- structure(list(
    schema_version = "1.0",
    backend_a = ordered[[1L]],
    backend_b = ordered[[2L]],
    k = k,
    alignment_score = scores$alignment_score,
    correlation_score = scores$correlation_score,
    cosine_score = scores$cosine_score,
    rmsd = scores$rmsd,
    minimum_alignment_score = minimum_alignment_score,
    role = role,
    passed = scores$alignment_score >= minimum_alignment_score,
    interpretation = as.character(interpretation)[1L]
  ), class = "PopgenVCFAncestryCrossBackendComparison")
  validate_ancestry_cross_backend_comparison(x)
}

#' Validate a cross-backend Q-matrix comparison record
#' @param x A `PopgenVCFAncestryCrossBackendComparison`.
#' @return `x`, invisibly.
#' @export
validate_ancestry_cross_backend_comparison <- function(x) {
  if (!inherits(x, "PopgenVCFAncestryCrossBackendComparison")) {
    stop("x must be a PopgenVCFAncestryCrossBackendComparison", call. = FALSE)
  }
  required <- c("schema_version", "backend_a", "backend_b", "k", "alignment_score",
                "correlation_score", "cosine_score", "rmsd", "minimum_alignment_score",
                "role", "passed", "interpretation")
  if (!all(required %in% names(x)) || !identical(x$schema_version, "1.0")) {
    stop("invalid ancestry cross-backend comparison schema", call. = FALSE)
  }
  if (!identical(c(x$backend_a, x$backend_b), sort(c(x$backend_a, x$backend_b))) ||
      identical(x$backend_a, x$backend_b)) {
    stop("backend_a and backend_b must be distinct and alphabetically ordered", call. = FALSE)
  }
  if (!is.integer(x$k) || length(x$k) != 1L || is.na(x$k) || x$k < 2L) {
    stop("k must be an integer of at least two", call. = FALSE)
  }
  for (field in c("alignment_score", "correlation_score", "cosine_score", "rmsd")) {
    if (!is.numeric(x[[field]]) || length(x[[field]]) != 1L || !is.finite(x[[field]])) {
      stop(field, " must be one finite numeric value", call. = FALSE)
    }
  }
  if (!is.numeric(x$minimum_alignment_score) || length(x$minimum_alignment_score) != 1L ||
      !is.finite(x$minimum_alignment_score) || x$minimum_alignment_score < 0 ||
      x$minimum_alignment_score > 1) {
    stop("minimum_alignment_score must lie in [0, 1]", call. = FALSE)
  }
  if (!x$role %in% c("equivalence", "diagnostic")) stop("invalid comparison role", call. = FALSE)
  if (!is.logical(x$passed) || length(x$passed) != 1L || is.na(x$passed) ||
      !identical(x$passed, x$alignment_score >= x$minimum_alignment_score)) {
    stop("passed is inconsistent with alignment_score and minimum_alignment_score", call. = FALSE)
  }
  if (!is.character(x$interpretation) || length(x$interpretation) != 1L || is.na(x$interpretation)) {
    stop("interpretation must be one character value", call. = FALSE)
  }
  invisible(x)
}

#' Create canonical ancestry three-backend release evidence
#'
#' Binds one approved canonical dataset, an exact sample order, per-backend
#' K-sweep evidence for ADMIXTURE, fastStructure, and sNMF, cross-backend
#' Q-matrix comparisons at one selected K, and the cross-backend K-selection
#' consensus, under the same proposed/approved review contract used by the
#' `production_baseline` and `external_concordance` gates.
#'
#' @param dataset_id Canonical dataset identifier.
#' @param dataset_version Canonical dataset version.
#' @param sample_ids Sample identifiers in exact Q-matrix row order.
#' @param region Description of the genomic interval or scope analyzed.
#' @param backend_evidence List of exactly three `PopgenVCFAncestryBackendEvidence`
#'   objects, one each for `admixture`, `faststructure`, and `snmf`.
#' @param cross_backend_comparisons List of exactly three
#'   `PopgenVCFAncestryCrossBackendComparison` objects (one per backend pair)
#'   all at `selected_k`.
#' @param k_selection A validated `PopgenVCFKSelection` object.
#' @param selected_k K value the cross-backend comparisons were computed at.
#' @param generated_by Non-empty description of the generating workflow.
#' @param generated_at ISO-8601 UTC timestamp.
#' @param source_commit Full 40-character Git commit SHA.
#' @param approval One of `proposed` or `approved`.
#' @param approved_by Reviewer identity; required when approved.
#' @param approved_at ISO-8601 date; required when approved.
#' @param notes Optional review notes.
#' @return A validated `PopgenVCFCanonicalAncestryThreeBackendEvidence`.
#' @export
new_canonical_ancestry_three_backend_evidence <- function(
    dataset_id, dataset_version, sample_ids, region,
    backend_evidence, cross_backend_comparisons, k_selection, selected_k,
    generated_by, generated_at, source_commit,
    approval = c("proposed", "approved"), approved_by = NULL, approved_at = NULL,
    notes = NULL) {
  dataset_id <- tolower(ancestry_three_backend_scalar(dataset_id, "dataset_id"))
  dataset_version <- ancestry_three_backend_scalar(dataset_version, "dataset_version")
  sample_ids <- as.character(sample_ids)
  if (!length(sample_ids) || anyNA(sample_ids) || anyDuplicated(sample_ids)) {
    stop("sample_ids must be non-empty, unique, and non-missing", call. = FALSE)
  }
  region <- ancestry_three_backend_scalar(region, "region")
  approval <- match.arg(approval)
  selected_k <- as.integer(selected_k)[1L]
  if (is.na(selected_k) || selected_k < 2L) stop("selected_k must be an integer of at least two", call. = FALSE)
  generated_by <- ancestry_three_backend_scalar(generated_by, "generated_by")
  generated_at <- ancestry_three_backend_iso_timestamp(generated_at, "generated_at")
  if (!grepl("^[0-9a-f]{40}$", source_commit)) {
    stop("source_commit must be a full lowercase Git SHA", call. = FALSE)
  }
  if (approval == "approved") {
    approved_by <- ancestry_three_backend_scalar(approved_by, "approved_by")
    approved_at <- ancestry_three_backend_iso_date(approved_at, "approved_at")
  } else if (!is.null(approved_by) || !is.null(approved_at)) {
    stop("proposed evidence cannot contain approval metadata", call. = FALSE)
  }

  lapply(backend_evidence, validate_ancestry_backend_evidence)
  backend_names <- vapply(backend_evidence, `[[`, character(1L), "backend")
  if (!identical(sort(backend_names), c("admixture", "faststructure", "snmf"))) {
    stop("backend_evidence must contain exactly one admixture, faststructure, and snmf record", call. = FALSE)
  }
  backend_evidence <- backend_evidence[order(backend_names)]
  if (!all(vapply(backend_evidence, function(e) selected_k %in% e$k_values, logical(1L)))) {
    stop("selected_k must be present in every backend's k_values", call. = FALSE)
  }

  lapply(cross_backend_comparisons, validate_ancestry_cross_backend_comparison)
  if (length(cross_backend_comparisons) != 3L) {
    stop("cross_backend_comparisons must contain exactly three backend-pair records", call. = FALSE)
  }
  pair_keys <- vapply(cross_backend_comparisons, function(c) paste(c$backend_a, c$backend_b, sep = "::"), character(1L))
  expected_pairs <- sort(apply(utils::combn(sort(c("admixture", "faststructure", "snmf")), 2L), 2L,
                                paste, collapse = "::"))
  if (!identical(sort(pair_keys), expected_pairs)) {
    stop("cross_backend_comparisons must cover each unordered backend pair exactly once", call. = FALSE)
  }
  if (!all(vapply(cross_backend_comparisons, function(c) identical(c$k, selected_k), logical(1L)))) {
    stop("all cross_backend_comparisons must use selected_k", call. = FALSE)
  }
  cross_backend_comparisons <- cross_backend_comparisons[order(pair_keys)]

  validate_ancestry_k_selection(k_selection)
  if (!identical(as.integer(k_selection$overall_k), selected_k)) {
    stop("selected_k must equal k_selection$overall_k", call. = FALSE)
  }

  sample_order_sha256 <- tolower(digest::digest(paste(sample_ids, collapse = "\n"), algo = "sha256", serialize = FALSE))

  x <- structure(list(
    schema_version = "1.0",
    dataset_id = dataset_id,
    dataset_version = dataset_version,
    region = region,
    sample_count = length(sample_ids),
    sample_order_sha256 = sample_order_sha256,
    backend_evidence = backend_evidence,
    cross_backend_comparisons = cross_backend_comparisons,
    k_selection = k_selection,
    selected_k = selected_k,
    generated_by = generated_by,
    generated_at = generated_at,
    source_commit = source_commit,
    approval = approval,
    approved_by = approved_by,
    approved_at = approved_at,
    notes = if (is.null(notes)) NULL else ancestry_three_backend_scalar(notes, "notes")
  ), class = "PopgenVCFCanonicalAncestryThreeBackendEvidence")
  validate_canonical_ancestry_three_backend_evidence(x)
}

#' Validate canonical ancestry three-backend release evidence
#' @param x A `PopgenVCFCanonicalAncestryThreeBackendEvidence`.
#' @param require_approved Fail unless the evidence is approved.
#' @return `x`, invisibly.
#' @export
validate_canonical_ancestry_three_backend_evidence <- function(x, require_approved = FALSE) {
  if (!inherits(x, "PopgenVCFCanonicalAncestryThreeBackendEvidence")) {
    stop("x must be a PopgenVCFCanonicalAncestryThreeBackendEvidence", call. = FALSE)
  }
  required <- c("schema_version", "dataset_id", "dataset_version", "region", "sample_count",
                "sample_order_sha256", "backend_evidence", "cross_backend_comparisons",
                "k_selection", "selected_k", "generated_by", "generated_at", "source_commit",
                "approval", "approved_by", "approved_at", "notes")
  if (!all(required %in% names(x)) || !identical(x$schema_version, "1.0")) {
    stop("invalid canonical ancestry three-backend evidence schema", call. = FALSE)
  }
  ancestry_three_backend_scalar(x$dataset_id, "dataset_id")
  ancestry_three_backend_scalar(x$dataset_version, "dataset_version")
  ancestry_three_backend_scalar(x$region, "region")
  if (!is.numeric(x$sample_count) || length(x$sample_count) != 1L || is.na(x$sample_count) || x$sample_count < 1L) {
    stop("sample_count must be a positive integer", call. = FALSE)
  }
  if (!is.character(x$sample_order_sha256) || length(x$sample_order_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", x$sample_order_sha256)) {
    stop("sample_order_sha256 must be a SHA-256 hex digest", call. = FALSE)
  }
  lapply(x$backend_evidence, validate_ancestry_backend_evidence)
  backend_names <- vapply(x$backend_evidence, `[[`, character(1L), "backend")
  if (!identical(backend_names, c("admixture", "faststructure", "snmf"))) {
    stop("backend_evidence must be exactly admixture, faststructure, and snmf, in that order", call. = FALSE)
  }
  if (!is.integer(x$selected_k) || length(x$selected_k) != 1L || is.na(x$selected_k) || x$selected_k < 2L) {
    stop("selected_k must be an integer of at least two", call. = FALSE)
  }
  if (!all(vapply(x$backend_evidence, function(e) x$selected_k %in% e$k_values, logical(1L)))) {
    stop("selected_k must be present in every backend's k_values", call. = FALSE)
  }
  lapply(x$cross_backend_comparisons, validate_ancestry_cross_backend_comparison)
  if (length(x$cross_backend_comparisons) != 3L) {
    stop("cross_backend_comparisons must contain exactly three backend-pair records", call. = FALSE)
  }
  pair_keys <- vapply(x$cross_backend_comparisons, function(c) paste(c$backend_a, c$backend_b, sep = "::"), character(1L))
  expected_pairs <- sort(apply(utils::combn(c("admixture", "faststructure", "snmf"), 2L), 2L, paste, collapse = "::"))
  if (!identical(pair_keys, expected_pairs)) {
    stop("cross_backend_comparisons must be the three ordered unordered-backend pairs", call. = FALSE)
  }
  if (!all(vapply(x$cross_backend_comparisons, function(c) identical(c$k, x$selected_k), logical(1L)))) {
    stop("all cross_backend_comparisons must use selected_k", call. = FALSE)
  }
  validate_ancestry_k_selection(x$k_selection)
  if (!identical(as.integer(x$k_selection$overall_k), x$selected_k)) {
    stop("selected_k must equal k_selection$overall_k", call. = FALSE)
  }
  ancestry_three_backend_scalar(x$generated_by, "generated_by")
  ancestry_three_backend_iso_timestamp(x$generated_at, "generated_at")
  if (!is.character(x$source_commit) || length(x$source_commit) != 1L ||
      !grepl("^[0-9a-f]{40}$", x$source_commit)) {
    stop("source_commit must be a full lowercase Git SHA", call. = FALSE)
  }
  if (!x$approval %in% c("proposed", "approved")) stop("invalid ancestry evidence approval state", call. = FALSE)
  if (identical(x$approval, "approved")) {
    ancestry_three_backend_scalar(x$approved_by, "approved_by")
    ancestry_three_backend_iso_date(x$approved_at, "approved_at")
  } else if (!is.null(x$approved_by) || !is.null(x$approved_at)) {
    stop("proposed evidence cannot contain approval metadata", call. = FALSE)
  }
  if (!is.null(x$notes)) ancestry_three_backend_scalar(x$notes, "notes")
  if (isTRUE(require_approved) && !identical(x$approval, "approved")) {
    stop("canonical ancestry three-backend evidence is not approved", call. = FALSE)
  }
  invisible(x)
}

#' Approve proposed canonical ancestry three-backend evidence
#' @param x A validated proposed `PopgenVCFCanonicalAncestryThreeBackendEvidence`.
#' @param approved_by Non-empty scientific reviewer identity.
#' @param approved_at ISO-8601 review date.
#' @param notes Optional approval notes; defaults to the proposal notes.
#' @return A validated approved evidence object.
#' @export
approve_canonical_ancestry_three_backend_evidence <- function(x, approved_by, approved_at, notes = x$notes) {
  validate_canonical_ancestry_three_backend_evidence(x)
  if (!identical(x$approval, "proposed")) stop("only proposed evidence can be approved", call. = FALSE)
  x$approval <- "approved"
  x$approved_by <- approved_by
  x$approved_at <- approved_at
  # Use `[<-` with a wrapped list rather than `$<-`: assigning NULL via `$<-`
  # deletes the list element outright instead of setting it to NULL, which
  # would silently drop "notes" from the schema when no notes are supplied.
  x["notes"] <- list(notes)
  validate_canonical_ancestry_three_backend_evidence(x, require_approved = TRUE)
  x
}

#' Write canonical ancestry three-backend evidence
#' @param x Evidence object.
#' @param path Destination JSON path.
#' @param require_approved Fail unless the evidence is approved.
#' @return Normalized output path.
#' @export
write_canonical_ancestry_three_backend_evidence <- function(x, path, require_approved = FALSE) {
  validate_canonical_ancestry_three_backend_evidence(x, require_approved = require_approved)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- unclass(x)
  payload$backend_evidence <- lapply(x$backend_evidence, unclass)
  payload$cross_backend_comparisons <- lapply(x$cross_backend_comparisons, unclass)
  payload$k_selection <- unclass(x$k_selection)
  jsonlite::write_json(
    payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null",
    null = "null", digits = 17
  )
  normalizePath(path)
}
