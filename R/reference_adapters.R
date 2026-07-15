reference_adapter_requirement <- function(kind, dependency) {
  force(kind); force(dependency)
  function() {
    if (kind == "package") {
      if (requireNamespace(dependency, quietly = TRUE)) return(TRUE)
      return(paste0("R package '", dependency, "' is not installed"))
    }
    path <- Sys.which(dependency)
    if (nzchar(path)) return(TRUE)
    paste0("executable '", dependency, "' is not available on PATH")
  }
}

reference_adapter_version <- function(kind, dependency) {
  if (kind == "package") {
    if (!requireNamespace(dependency, quietly = TRUE)) return(NA_character_)
    return(as.character(utils::packageVersion(dependency)))
  }
  path <- Sys.which(dependency)
  if (!nzchar(path)) return(NA_character_)
  out <- tryCatch(system2(path, "--version", stdout = TRUE, stderr = TRUE), error = function(e) character())
  if (!length(out)) out <- tryCatch(system2(path, "-v", stdout = TRUE, stderr = TRUE), error = function(e) character())
  if (!length(out)) NA_character_ else trimws(out[[1L]])
}

#' Create a real external-reference adapter
#'
#' @param id Stable adapter identifier.
#' @param analysis Analysis identifier.
#' @param tool Reference tool name.
#' @param kind `package` or `executable`.
#' @param dependency Package or executable identifier.
#' @param mode External-reference comparison mode.
#' @param role Scientific role: `equivalence` or `diagnostic`.
#' @param observed,reference Functions accepting a benchmark payload.
#' @param absolute_tolerance,relative_tolerance Comparison tolerances.
#' @param interpretation Scientific interpretation.
#' @param citations Character vector of citations.
#' @return A validated `PopgenVCFReferenceAdapter`.
#' @export
new_reference_adapter <- function(id, analysis, tool,
                                  kind = c("package", "executable"), dependency,
                                  mode = c("numeric", "exact", "matrix", "subspace", "q_matrix"),
                                  role = c("equivalence", "diagnostic"),
                                  observed, reference,
                                  absolute_tolerance = 1e-8,
                                  relative_tolerance = 1e-6,
                                  interpretation = "", citations = character()) {
  kind <- match.arg(kind); mode <- match.arg(mode); role <- match.arg(role)
  if (!is.function(observed) || !is.function(reference)) stop("observed and reference must be functions", call. = FALSE)
  x <- structure(list(
    schema_version = "1.0", id = tolower(as.character(id)[1L]),
    analysis = tolower(as.character(analysis)[1L]), tool = as.character(tool)[1L],
    kind = kind, dependency = as.character(dependency)[1L], mode = mode, role = role,
    observed = observed, reference = reference,
    absolute_tolerance = as.numeric(absolute_tolerance),
    relative_tolerance = as.numeric(relative_tolerance),
    interpretation = as.character(interpretation)[1L], citations = as.character(citations)
  ), class = "PopgenVCFReferenceAdapter")
  validate_reference_adapter(x)
}

#' Validate a real external-reference adapter
#' @param x A `PopgenVCFReferenceAdapter`.
#' @return `x`, invisibly.
#' @export
validate_reference_adapter <- function(x) {
  if (!inherits(x, "PopgenVCFReferenceAdapter")) stop("x must be a PopgenVCFReferenceAdapter", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported reference adapter schema", call. = FALSE)
  if (!x$kind %in% c("package", "executable")) stop("invalid adapter kind", call. = FALSE)
  if (!x$role %in% c("equivalence", "diagnostic")) stop("invalid scientific role", call. = FALSE)
  if (!is.function(x$observed) || !is.function(x$reference)) stop("adapter functions are invalid", call. = FALSE)
  invisible(x)
}

#' Inspect real external-reference adapter availability
#' @param adapters An adapter or list of adapters.
#' @return A data table with availability and discovered versions.
#' @export
reference_adapter_status <- function(adapters) {
  if (inherits(adapters, "PopgenVCFReferenceAdapter")) adapters <- list(adapters)
  data.table::rbindlist(lapply(adapters, function(x) {
    validate_reference_adapter(x)
    requirement <- reference_adapter_requirement(x$kind, x$dependency)()
    data.table::data.table(
      id = x$id, analysis = x$analysis, tool = x$tool, kind = x$kind,
      dependency = x$dependency, available = isTRUE(requirement),
      version = reference_adapter_version(x$kind, x$dependency),
      reason = if (isTRUE(requirement)) "" else as.character(requirement)[1L],
      role = x$role, mode = x$mode
    )
  }), fill = TRUE)
}

#' Convert a real adapter to an external-reference specification
#' @param adapter A `PopgenVCFReferenceAdapter`.
#' @return A `PopgenVCFExternalReferenceSpec`.
#' @export
reference_adapter_spec <- function(adapter) {
  validate_reference_adapter(adapter)
  new_external_reference_spec(
    id = adapter$id, analysis = adapter$analysis,
    reference_tool = adapter$tool,
    reference_version = reference_adapter_version(adapter$kind, adapter$dependency),
    mode = adapter$mode, role = adapter$role,
    observed = adapter$observed, reference = adapter$reference,
    requirements = reference_adapter_requirement(adapter$kind, adapter$dependency),
    absolute_tolerance = adapter$absolute_tolerance,
    relative_tolerance = adapter$relative_tolerance,
    interpretation = adapter$interpretation, citations = adapter$citations
  )
}

payload_value <- function(data, ...) {
  keys <- c(...)
  current <- data
  for (key in keys) {
    if (!is.list(current) || is.null(current[[key]])) stop("benchmark payload is missing: ", paste(keys, collapse = "$"), call. = FALSE)
    current <- current[[key]]
  }
  current
}

precomputed_reference <- function(name) {
  force(name)
  function(data) payload_value(data, "references", name)
}

precomputed_observed <- function(name) {
  force(name)
  function(data) payload_value(data, "observed", name)
}

#' Default registry of established scientific reference adapters
#'
#' The registry uses canonical benchmark payload keys. R-package adapters may be
#' supplied computed values under `references`; executable adapters accept
#' parsed reference output under the same contract. This keeps package checks
#' independent of external software while allowing integration workflows to
#' execute and parse those tools before comparison.
#'
#' @return A named list of `PopgenVCFReferenceAdapter` objects.
#' @export
default_reference_adapter_registry <- function() {
  adapter <- function(id, analysis, tool, kind, dependency, mode, role,
                      observed_key, reference_key, interpretation, citations = character(),
                      absolute_tolerance = 1e-8, relative_tolerance = 1e-6) {
    new_reference_adapter(
      id, analysis, tool, kind, dependency, mode, role,
      observed = precomputed_observed(observed_key),
      reference = precomputed_reference(reference_key),
      absolute_tolerance = absolute_tolerance,
      relative_tolerance = relative_tolerance,
      interpretation = interpretation, citations = citations
    )
  }
  out <- list(
    snprelate_pca = adapter("snprelate_pca", "pca", "SNPRelate", "package", "SNPRelate", "subspace", "equivalence", "pca_scores", "snprelate_pca_scores", "PCA sample eigenspaces should agree up to sign and rotation.", absolute_tolerance = 1e-7),
    snprelate_ibs = adapter("snprelate_ibs", "ibs", "SNPRelate", "package", "SNPRelate", "matrix", "equivalence", "ibs", "snprelate_ibs", "Pairwise IBS coefficients should be numerically equivalent.", absolute_tolerance = 1e-8),
    plink2_pca = adapter("plink2_pca", "pca", "PLINK 2", "executable", "plink2", "subspace", "diagnostic", "pca_scores", "plink2_pca_scores", "PLINK 2 PCA is retained as a cross-implementation eigenspace diagnostic.", absolute_tolerance = 1e-5),
    plink2_king = adapter("plink2_king", "ibs", "PLINK 2", "executable", "plink2", "matrix", "diagnostic", "ibs", "plink2_king", "KING kinship and IBS-derived similarity are related but not identical estimands.", absolute_tolerance = 1e-5),
    hierfstat_fst = adapter("hierfstat_fst", "fst", "hierfstat", "package", "hierfstat", "numeric", "diagnostic", "fst", "hierfstat_fst", "Alternative Weir-Cockerham implementations are reported transparently as a diagnostic.", absolute_tolerance = 1e-6),
    adegenet_diversity = adapter("adegenet_diversity", "diversity", "adegenet", "package", "adegenet", "numeric", "diagnostic", "diversity", "adegenet_diversity", "Diversity summaries are compared only where definitions and missing-data handling match.", absolute_tolerance = 1e-8),
    adegenet_dapc = adapter("adegenet_dapc", "dapc", "adegenet", "package", "adegenet", "numeric", "equivalence", "dapc_assignment", "adegenet_dapc_assignment", "DAPC classification accuracy should match the adegenet reference workflow.", absolute_tolerance = 1e-8),
    poppr_amova = adapter("poppr_amova", "amova", "poppr", "package", "poppr", "numeric", "diagnostic", "amova", "poppr_amova", "AMOVA components are diagnostic unless model and distance definitions are identical.", absolute_tolerance = 1e-6),
    pegas_amova = adapter("pegas_amova", "amova", "pegas", "package", "pegas", "numeric", "diagnostic", "amova", "pegas_amova", "pegas AMOVA is retained as an independent cross-method diagnostic.", absolute_tolerance = 1e-6),
    vegan_mantel = adapter("vegan_mantel", "ibd", "vegan", "package", "vegan", "numeric", "equivalence", "mantel", "vegan_mantel", "Mantel statistic and permutation significance should match when seeds and permutations are identical.", absolute_tolerance = 1e-8),
    admixture_q = adapter("admixture_q", "ancestry", "ADMIXTURE", "executable", "admixture", "q_matrix", "diagnostic", "ancestry_q", "admixture_q", "Ancestry coefficients are compared after label alignment; model optima may differ.", absolute_tolerance = 1e-3),
    faststructure_q = adapter("faststructure_q", "ancestry", "fastStructure", "executable", "structure.py", "q_matrix", "diagnostic", "ancestry_q", "faststructure_q", "fastStructure ancestry coefficients are a non-gating cross-model diagnostic.", absolute_tolerance = 1e-3),
    lea_snmf_q = adapter("lea_snmf_q", "ancestry", "LEA/sNMF", "package", "LEA", "q_matrix", "diagnostic", "ancestry_q", "lea_snmf_q", "sNMF ancestry coefficients are compared after label alignment as a diagnostic.", absolute_tolerance = 1e-3)
  )
  out[order(names(out))]
}

#' Run installed real-reference adapters
#' @param data Canonical benchmark payload with `observed` and `references` lists.
#' @param adapters Adapter list, defaulting to the built-in registry.
#' @param analyses Optional analysis filter.
#' @return A list containing results, status, and a long-form table.
#' @export
run_reference_adapters <- function(data, adapters = default_reference_adapter_registry(), analyses = NULL) {
  if (!is.null(analyses)) {
    requested <- tolower(as.character(analyses))
    adapters <- Filter(function(x) x$analysis %in% requested, adapters)
  }
  results <- lapply(adapters, function(x) run_external_reference(reference_adapter_spec(x), data))
  names(results) <- names(adapters)
  list(results = results, status = reference_adapter_status(adapters), table = external_reference_table(results))
}
