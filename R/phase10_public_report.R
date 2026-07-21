# Phase 10.1.3 — public report rendering adapter

.phase10_public_report_projection <- function(plan) {
  validate_population_genomics_report_plan(plan)
  sections <- as.data.frame(plan$sections)
  sections <- sections[, c(
    "order", "analysis", "title", "class", "has_metadata", "validation_passed"
  ), drop = FALSE]
  sections <- sections[
    order(sections$order, sections$analysis, method = "radix"),
    , drop = FALSE
  ]
  rownames(sections) <- NULL
  list(
    schema_version = plan$schema_version,
    title = plan$title,
    sections = sections
  )
}

.phase10_public_report_id <- function(plan) {
  paste0("report::", phase10_public_fingerprint(
    .phase10_public_report_projection(plan)
  ))
}

.phase10_public_report_artifact_ids <- function(manifest) {
  validate_artifact_manifest(manifest, must_exist = TRUE)
  if (!length(manifest)) return(stats::setNames(character(), character()))
  ids <- vapply(manifest, .phase10_public_artifact_id, character(1L))
  ids <- sort(ids, method = "radix")
  stats::setNames(ids, ids)
}

#' Render a canonical public population-genomics report
#'
#' Delegates report generation to the existing population-genomics report
#' engine and translates its output into the stable Phase 10 public API. Output
#' directories, generated paths, Quarto process details, timestamps, platform
#' metadata, and renderer state remain internal.
#'
#' @param request A canonical public request for `report.render`.
#' @param report A `PopgenVCFReportPlan` or a named list of canonical results.
#' @param output_dir Internal report output directory.
#' @param title Report title used when `report` is a result list.
#' @param formats Requested output formats (`html` and/or `pdf`).
#' @param render Render through Quarto. When `FALSE`, deterministic report
#'   sources and manifests are generated without invoking Quarto.
#' @param include,exclude Optional analysis filters used when building a plan.
#' @return A validated `PopgenVCFPublicAPIResponse`.
#' @export
render_public_report <- function(
    request,
    report,
    output_dir,
    title = "Population genomics report",
    formats = "html",
    render = FALSE,
    include = NULL,
    exclude = NULL) {
  validate_public_analysis_request(request)
  if (!identical(request$operation_id, "report.render")) {
    return(.phase10_public_failure(
      request, "unsupported_operation",
      "This adapter accepts only report.render requests."
    ))
  }

  formats <- sort(unique(tolower(as.character(formats))), method = "radix")
  if (!length(formats) || anyNA(formats) || any(!nzchar(formats)) ||
      any(!formats %in% c("html", "pdf"))) {
    return(.phase10_public_failure(
      request, "unsupported_report_format",
      "Report formats must be one or more of html or pdf."
    ))
  }
  if (!is.logical(render) || length(render) != 1L || is.na(render)) {
    return(.phase10_public_failure(
      request, "invalid_render_option", "render must be TRUE or FALSE."
    ))
  }

  plan <- tryCatch(
    if (inherits(report, "PopgenVCFReportPlan")) {
      validate_population_genomics_report_plan(report)
      report
    } else {
      build_population_genomics_report_plan(
        report, title = title, include = include, exclude = exclude
      )
    },
    error = function(e) e
  )
  if (inherits(plan, "error")) {
    return(.phase10_public_failure(
      request, "invalid_report_input", conditionMessage(plan)
    ))
  }

  written <- tryCatch(
    write_population_genomics_report(
      results = plan,
      output_dir = output_dir,
      formats = formats,
      render = render
    ),
    error = function(e) e
  )
  if (inherits(written, "error")) {
    return(.phase10_public_failure(
      request, "report_render_failed", conditionMessage(written),
      status = "failed"
    ))
  }

  report_id <- .phase10_public_report_id(plan)
  section_ids <- as.character(plan$sections$analysis)
  section_ids <- stats::setNames(section_ids, section_ids)
  artifact_ids <- .phase10_public_report_artifact_ids(written$artifacts)

  new_public_analysis_response(
    request = request,
    status = "completed",
    scientific_values = list(
      report_id = report_id,
      rendered = isTRUE(render),
      requested_formats = formats,
      section_ids = section_ids,
      title = plan$title
    ),
    artifact_ids = artifact_ids,
    provenance_ids = stats::setNames(report_id, "report_plan")
  )
}
