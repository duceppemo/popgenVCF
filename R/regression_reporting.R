#' Compare two archived release benchmark records
#'
#' @param current,baseline `PopgenVCFReleaseBenchmarkRecord` objects.
#' @return A validated `PopgenVCFReleaseComparison`.
#' @export
compare_release_benchmarks <- function(current, baseline) {
  validate_release_benchmark_record(current)
  validate_release_benchmark_record(baseline)
  common <- intersect(names(current$components), names(baseline$components))
  current_only <- setdiff(names(current$components), names(baseline$components))
  baseline_only <- setdiff(names(baseline$components), names(current$components))
  rows <- lapply(common, function(component) {
    observed <- current$components[[component]]
    reference <- baseline$components[[component]]
    if (inherits(observed, "PopgenVCFPerformanceResult") &&
        inherits(reference, "PopgenVCFPerformanceResult")) {
      comparison <- tryCatch(
        compare_performance_baseline(observed, reference),
        error = function(e) NULL
      )
      if (!is.null(comparison)) {
        tab <- performance_benchmark_table(comparison)
        tab[, `:=`(component = component, comparison_type = "performance")]
        return(tab[])
      }
    }
    identical_digest <- identical(
      current$component_digests[[component]],
      baseline$component_digests[[component]]
    )
    data.table::data.table(
      component = component,
      comparison_type = "digest",
      status = if (identical_digest) "unchanged" else "changed",
      current_digest = current$component_digests[[component]],
      baseline_digest = baseline$component_digests[[component]]
    )
  })
  details <- data.table::rbindlist(rows, fill = TRUE)
  status <- if (nrow(details) && any(details$status %in% c("failed", "error"))) {
    "failed"
  } else {
    "passed"
  }
  structure(list(
    schema_version = "1.0",
    current_release = current$release,
    baseline_release = baseline$release,
    status = status,
    common_components = common,
    current_only = current_only,
    baseline_only = baseline_only,
    details = details
  ), class = "PopgenVCFReleaseComparison")
}

#' Convert a release comparison to a stable table
#' @param x A `PopgenVCFReleaseComparison`.
#' @return A data table.
#' @export
release_comparison_table <- function(x) {
  if (!inherits(x, "PopgenVCFReleaseComparison")) {
    stop("x must be a PopgenVCFReleaseComparison", call. = FALSE)
  }
  tab <- data.table::copy(x$details)
  tab[, `:=`(
    current_release = x$current_release,
    baseline_release = x$baseline_release,
    overall_status = x$status
  )]
  data.table::setcolorder(tab, c(
    "current_release", "baseline_release", "overall_status",
    setdiff(names(tab), c("current_release", "baseline_release", "overall_status"))
  ))
  tab[]
}

#' Select the latest archived release
#' @param archive A `PopgenVCFBenchmarkArchive`.
#' @param exclude Optional release identifiers to exclude.
#' @return A `PopgenVCFReleaseBenchmarkRecord`.
#' @export
latest_release_benchmark <- function(archive, exclude = character()) {
  if (!inherits(archive, "PopgenVCFBenchmarkArchive")) {
    stop("archive is invalid", call. = FALSE)
  }
  releases <- setdiff(names(archive$records), as.character(exclude))
  if (!length(releases)) stop("archive contains no eligible releases", call. = FALSE)
  versions <- sub("^v", "", releases)
  parsed <- suppressWarnings(lapply(versions, package_version))
  valid <- vapply(parsed, function(x) !inherits(x, "try-error"), logical(1L))
  if (all(valid)) {
    return(archive$records[[releases[[order(vapply(parsed, as.character, character(1L)), decreasing = TRUE)[1L]]]]])
  }
  records <- archive$records[releases]
  created <- vapply(records, `[[`, character(1L), "created_at")
  records[[order(created, decreasing = TRUE)[1L]]]
}

regression_report_qmd <- function(archive, comparison = NULL, title) {
  releases <- benchmark_archive_table(archive)
  comparison_code <- if (is.null(comparison)) {
    "No baseline comparison was supplied."
  } else {
    paste0(
      "## Current versus baseline\n\n",
      "Current release: **", comparison$current_release, "**  \n",
      "Baseline release: **", comparison$baseline_release, "**  \n",
      "Overall status: **", toupper(comparison$status), "**\n\n",
      "```{r}\nknitr::kable(comparison_table)\n```\n"
    )
  }
  paste0(
    "---\ntitle: ", dQuote(title), "\nformat:\n  html:\n    toc: true\n    embed-resources: true\nexecute:\n  echo: false\n---\n\n",
    "# Scientific regression archive\n\n",
    "This report summarizes the immutable, checksummed popgenVCF benchmark archive.\n\n",
    "## Archived releases\n\n```{r}\nknitr::kable(releases_table)\n```\n\n",
    comparison_code,
    "\n## Archive integrity\n\nAll files are recorded in `manifest.tsv` with SHA256 checksums.\n\n",
    "## Reproducibility\n\nRelease records retain Git identity, package version, container digest, datasets, parameters, environment, and complete canonical benchmark components.\n"
  )
}

#' Write a scientific regression report
#'
#' @param archive A benchmark archive or archive directory.
#' @param output_dir Destination directory.
#' @param comparison Optional release comparison.
#' @param render Render HTML with Quarto when available.
#' @param title Report title.
#' @return A named list of generated files.
#' @export
write_regression_report <- function(
    archive, output_dir, comparison = NULL, render = TRUE,
    title = "popgenVCF scientific regression report") {
  if (is.character(archive) && length(archive) == 1L) {
    archive <- read_benchmark_archive(archive, verify = TRUE)
  }
  if (!inherits(archive, "PopgenVCFBenchmarkArchive")) stop("archive is invalid", call. = FALSE)
  if (!is.null(comparison) && !inherits(comparison, "PopgenVCFReleaseComparison")) {
    stop("comparison is invalid", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  releases_table <- benchmark_archive_table(archive)
  comparison_table <- if (is.null(comparison)) data.table::data.table() else release_comparison_table(comparison)
  releases_path <- file.path(output_dir, "release_history.tsv")
  comparison_path <- file.path(output_dir, "release_comparison.tsv")
  data.table::fwrite(releases_table, releases_path, sep = "\t")
  data.table::fwrite(comparison_table, comparison_path, sep = "\t")
  data_path <- file.path(output_dir, "report_data.rds")
  saveRDS(list(archive = archive, comparison = comparison,
               releases_table = releases_table, comparison_table = comparison_table),
          data_path, version = 3)
  qmd_path <- file.path(output_dir, "regression_report.qmd")
  writeLines(c(
    "```{r}",
    "report_data <- readRDS('report_data.rds')",
    "releases_table <- report_data$releases_table",
    "comparison_table <- report_data$comparison_table",
    "```",
    regression_report_qmd(archive, comparison, title)
  ), qmd_path)
  html_path <- file.path(output_dir, "regression_report.html")
  rendered <- FALSE
  if (isTRUE(render) && nzchar(Sys.which("quarto"))) {
    status <- system2("quarto", c("render", basename(qmd_path), "--output", basename(html_path)),
                      stdout = TRUE, stderr = TRUE, wd = output_dir)
    rendered <- file.exists(html_path)
    if (!rendered) warning(paste(status, collapse = "\n"), call. = FALSE)
  }
  list(qmd = qmd_path, html = if (rendered) html_path else NA_character_,
       releases = releases_path, comparison = comparison_path, data = data_path,
       rendered = rendered)
}
