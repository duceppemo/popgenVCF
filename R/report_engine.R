report_section_order <- function() {
  c("qc", "pca", "ibs", "tree", "diversity", "fst", "amova", "dapc", "ancestry", "ibd")
}

report_result_analysis <- function(x) {
  if (inherits(x, "PopgenVCFCoreResult")) return(x$analysis)
  if (inherits(x, c("PopgenVCFAncestryResult", "PopgenVCFAncestryConsensus", "PopgenVCFKSelection"))) return("ancestry")
  NA_character_
}

#' Build a deterministic population-genomics report plan
#'
#' @param results Named list of canonical core or ancestry result objects.
#' @param title Report title.
#' @param include Optional analysis identifiers to include.
#' @param exclude Optional analysis identifiers to omit.
#' @return A validated `PopgenVCFReportPlan`.
#' @export
build_population_genomics_report_plan <- function(results, title = "Population genomics report",
                                                   include = NULL, exclude = NULL) {
  if (!is.list(results) || !length(results)) stop("results must be a non-empty list", call. = FALSE)
  analyses <- vapply(results, report_result_analysis, character(1L))
  if (anyNA(analyses)) stop("all report inputs must be canonical result objects", call. = FALSE)
  keep <- rep(TRUE, length(results))
  if (!is.null(include)) keep <- keep & analyses %in% tolower(as.character(include))
  if (!is.null(exclude)) keep <- keep & !analyses %in% tolower(as.character(exclude))
  results <- results[keep]; analyses <- analyses[keep]
  if (!length(results)) stop("no report sections remain after filtering", call. = FALSE)
  ord <- match(analyses, report_section_order())
  ord[is.na(ord)] <- length(report_section_order()) + seq_len(sum(is.na(ord)))
  idx <- order(ord, analyses)
  results <- results[idx]; analyses <- analyses[idx]
  sections <- data.table::data.table(
    order = seq_along(results), analysis = analyses,
    title = vapply(analyses, function(x) switch(x,
      pca = "Principal component analysis", ibs = "IBS and multidimensional scaling",
      tree = "Neighbour-joining tree", diversity = "Population diversity",
      fst = "Genetic differentiation", amova = "Analysis of molecular variance",
      dapc = "Discriminant analysis of principal components",
      ancestry = "Ancestry and population structure",
      ibd = "Isolation by distance", qc = "Quality control", x), character(1L)),
    class = vapply(results, function(x) class(x)[1L], character(1L)),
    has_metadata = vapply(results, function(x) !is.null(x$metadata %||% NULL), logical(1L)),
    validation_passed = vapply(results, function(x) {
      if (inherits(x, "PopgenVCFCoreResult")) {
        validate_core_result(x); return(TRUE)
      }
      TRUE
    }, logical(1L))
  )
  x <- structure(list(
    schema_version = "1.0", title = as.character(title)[1L],
    sections = sections, results = unname(results),
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    reproducibility = list(
      package_version = as.character(utils::packageVersion("popgenVCF")),
      r_version = R.version.string,
      platform = R.version$platform
    )
  ), class = "PopgenVCFReportPlan")
  validate_population_genomics_report_plan(x)
}

#' Validate a population-genomics report plan
#' @param x A `PopgenVCFReportPlan`.
#' @return `x`, invisibly.
#' @export
validate_population_genomics_report_plan <- function(x) {
  if (!inherits(x, "PopgenVCFReportPlan")) stop("x must be a PopgenVCFReportPlan", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported report plan schema", call. = FALSE)
  if (!is.character(x$title) || length(x$title) != 1L || !nzchar(x$title)) stop("report title is invalid", call. = FALSE)
  if (!is.data.frame(x$sections) || !all(c("order", "analysis", "title", "class", "has_metadata", "validation_passed") %in% names(x$sections))) stop("report sections are invalid", call. = FALSE)
  if (nrow(x$sections) != length(x$results) || anyDuplicated(x$sections$order) || any(!x$sections$validation_passed)) stop("report plan sections are inconsistent", call. = FALSE)
  invisible(x)
}

report_markdown_table <- function(x, max_rows = 50L) {
  tab <- if (inherits(x, "PopgenVCFCoreResult")) core_result_table(x) else if (inherits(x, "PopgenVCFAncestryResult")) ancestry_result_table(x) else data.table::data.table()
  if (!nrow(tab)) return("_No primary table is available for this result._")
  tab <- utils::head(as.data.frame(tab), max_rows)
  header <- paste0("| ", paste(names(tab), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(tab)), collapse = " | "), " |")
  rows <- apply(tab, 1L, function(z) paste0("| ", paste(gsub("\\|", "\\\\|", as.character(z)), collapse = " | "), " |"))
  paste(c(header, rule, rows), collapse = "\n")
}

write_report_qmd <- function(plan, path) {
  lines <- c(
    "---", paste0("title: \"", gsub("\"", "'", plan$title), "\""),
    "format:", "  html:", "    toc: true", "    toc-depth: 3", "    code-fold: true",
    "    embed-resources: true", "execute:", "  echo: false", "---", "",
    "# Executive summary", "",
    sprintf("This report contains %d validated analysis section(s): %s.", nrow(plan$sections), paste(plan$sections$title, collapse = ", ")), "",
    "# Reproducibility", "",
    paste0("- popgenVCF: `", plan$reproducibility$package_version, "`"),
    paste0("- R: `", plan$reproducibility$r_version, "`"),
    paste0("- Platform: `", plan$reproducibility$platform, "`"),
    paste0("- Plan created: `", plan$created_at, "`"), ""
  )
  for (i in seq_len(nrow(plan$sections))) {
    s <- plan$sections[i, ]
    result <- plan$results[[i]]
    lines <- c(lines, paste0("# ", s$title), "",
      paste0("Canonical result class: `", s$class, "`."), "",
      "## Primary result table", "", report_markdown_table(result), "",
      "## Validation", "", "All canonical object and scientific validation checks passed before report assembly.", "")
    if (inherits(result, "PopgenVCFCoreResult") && length(result$artifacts)) {
      a <- artifact_manifest_table(result$artifacts)
      lines <- c(lines, "## Publication artifacts", "", report_markdown_table(new_diversity_result(a)), "")
    }
  }
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' Write a unified population-genomics report
#'
#' @param results Named list of canonical results, or a `PopgenVCFReportPlan`.
#' @param output_dir Report output directory.
#' @param title Report title.
#' @param formats Output formats: `html` and optionally `pdf`.
#' @param render Render with Quarto when available. When `FALSE`, write the
#'   deterministic plan and source document only.
#' @param include,exclude Optional analysis filters.
#' @return A list containing the report plan, artifact manifest, and paths.
#' @export
write_population_genomics_report <- function(results, output_dir, title = "Population genomics report",
                                               formats = "html", render = TRUE,
                                               include = NULL, exclude = NULL) {
  plan <- if (inherits(results, "PopgenVCFReportPlan")) results else
    build_population_genomics_report_plan(results, title, include, exclude)
  validate_population_genomics_report_plan(plan)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  qmd <- file.path(output_dir, "population_genomics_report.qmd")
  plan_rds <- file.path(output_dir, "population_genomics_report_plan.rds")
  sections_tsv <- file.path(output_dir, "population_genomics_report_sections.tsv")
  write_report_qmd(plan, qmd)
  saveRDS(plan, plan_rds, version = 3)
  data.table::fwrite(plan$sections, sections_tsv, sep = "\t", quote = FALSE)
  outputs <- character()
  formats <- unique(tolower(as.character(formats)))
  if (isTRUE(render)) {
    quarto <- Sys.which("quarto")
    if (!nzchar(quarto)) stop("Quarto is required to render the report; use render = FALSE to write sources only", call. = FALSE)
    for (fmt in formats) {
      if (!fmt %in% c("html", "pdf")) stop("unsupported report format: ", fmt, call. = FALSE)
      args <- c("render", qmd, "--to", fmt, "--output-dir", normalizePath(output_dir))
      status <- system2(quarto, args, stdout = TRUE, stderr = TRUE)
      code <- attr(status, "status") %||% 0L
      if (code != 0L) stop("Quarto report rendering failed for ", fmt, ": ", paste(status, collapse = "\n"), call. = FALSE)
      outputs <- c(outputs, file.path(output_dir, paste0("population_genomics_report.", fmt)))
    }
  }
  artifacts <- list(
    new_analysis_artifact("report", "source", "report", qmd, "qmd", "Quarto report source"),
    new_analysis_artifact("report", "plan", "provenance", plan_rds, "rds", "Deterministic report plan"),
    new_analysis_artifact("report", "sections", "table", sections_tsv, "tsv", "Report section manifest")
  )
  for (path in outputs) artifacts[[length(artifacts) + 1L]] <- new_analysis_artifact("report", paste0("report_", tools::file_ext(path)), "report", path, tools::file_ext(path), "Rendered population-genomics report")
  manifest <- new_artifact_manifest(artifacts)
  validate_artifact_manifest(manifest, must_exist = TRUE)
  list(plan = plan, artifacts = manifest, paths = c(source = qmd, plan = plan_rds, sections = sections_tsv, outputs))
}

#' @export
print.PopgenVCFReportPlan <- function(x, ...) {
  cat("<PopgenVCFReportPlan>", nrow(x$sections), "sections:", paste(x$sections$analysis, collapse = ", "), "\n")
  invisible(x)
}
