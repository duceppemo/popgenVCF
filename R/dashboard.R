dashboard_sample_ids <- function(x) {
  if (inherits(x, "PopgenVCFCoreResult")) {
    p <- x$payload
    if (!is.null(p$coordinates) && is.data.frame(p$coordinates) && "sample_id" %in% names(p$coordinates)) {
      return(as.character(p$coordinates$sample_id))
    }
    if (!is.null(p$similarity) && is.matrix(p$similarity)) {
      ids <- rownames(p$similarity)
      if (!is.null(ids)) return(as.character(ids))
      return(as.character(seq_len(nrow(p$similarity))))
    }
  }
  if (inherits(x, "PopgenVCFAncestryConsensus")) return(as.character(x$sample_ids))
  if (inherits(x, "PopgenVCFAncestryResult") && length(x$replicates)) {
    return(as.character(x$replicates[[1L]]$sample_ids))
  }
  character()
}

dashboard_validation_rows <- function(plan) {
  rows <- list()
  for (i in seq_len(nrow(plan$sections))) {
    result <- plan$results[[i]]
    analysis <- plan$sections$analysis[[i]]
    if (inherits(result, "PopgenVCFCoreResult")) {
      tab <- data.table::as.data.table(result$validation)
      tab[, analysis := analysis]
      rows[[length(rows) + 1L]] <- tab[, .(analysis, check, passed)]
    } else {
      rows[[length(rows) + 1L]] <- data.table::data.table(
        analysis = analysis,
        check = "canonical_object_validation",
        passed = TRUE
      )
    }
  }
  data.table::rbindlist(rows, fill = TRUE)
}

dashboard_artifact_rows <- function(plan) {
  rows <- list()
  for (i in seq_len(nrow(plan$sections))) {
    result <- plan$results[[i]]
    if (inherits(result, "PopgenVCFCoreResult") && length(result$artifacts)) {
      tab <- artifact_manifest_table(result$artifacts)
      tab[, analysis := plan$sections$analysis[[i]]]
      rows[[length(rows) + 1L]] <- tab
    }
  }
  if (!length(rows)) return(data.table::data.table())
  data.table::rbindlist(rows, fill = TRUE)
}

#' Build dashboard summary metrics
#'
#' @param plan A `PopgenVCFReportPlan`.
#' @return A one-row data table of dashboard metrics.
#' @export
build_dashboard_summary <- function(plan) {
  validate_population_genomics_report_plan(plan)
  ids <- unique(unlist(lapply(plan$results, dashboard_sample_ids), use.names = FALSE))
  metadata <- lapply(plan$results, function(x) x$metadata %||% NULL)
  populations <- unique(unlist(lapply(metadata, function(z) {
    if (is.data.frame(z) && "population" %in% names(z)) as.character(z$population) else character()
  }), use.names = FALSE))
  populations <- populations[!is.na(populations) & nzchar(populations)]
  recommended_k <- NA_integer_
  fst <- NA_real_
  for (x in plan$results) {
    if (inherits(x, "PopgenVCFKSelection")) recommended_k <- as.integer(x$overall_k)
    if (inherits(x, "PopgenVCFCoreResult") && identical(x$analysis, "fst")) {
      fst <- as.numeric(x$payload$global_fst)
    }
  }
  data.table::data.table(
    analyses = nrow(plan$sections),
    samples = length(ids),
    populations = length(populations),
    recommended_k = recommended_k,
    global_fst = fst
  )
}

#' Calculate a transparent scientific quality score
#'
#' @param plan A `PopgenVCFReportPlan`.
#' @return A list containing component scores and an overall percentage.
#' @export
calculate_scientific_quality <- function(plan) {
  validate_population_genomics_report_plan(plan)
  validations <- dashboard_validation_rows(plan)
  artifacts <- dashboard_artifact_rows(plan)

  schema_fraction <- mean(plan$sections$validation_passed)
  scientific_fraction <- if (nrow(validations)) mean(validations$passed) else NA_real_
  provenance_applicable <- vapply(plan$results, inherits, logical(1L), "PopgenVCFCoreResult")
  provenance_fraction <- if (any(provenance_applicable)) {
    mean(vapply(plan$results[provenance_applicable], function(x) length(x$provenance) > 0L, logical(1L)))
  } else NA_real_
  artifact_fraction <- if (nrow(artifacts)) {
    mean(file.exists(artifacts$path[artifacts$required]))
  } else NA_real_

  components <- data.table::data.table(
    component = c("canonical_schema", "scientific_validation", "provenance_completeness", "artifact_integrity"),
    weight = c(0.30, 0.35, 0.20, 0.15),
    applicable = c(TRUE, nrow(validations) > 0L, any(provenance_applicable), nrow(artifacts) > 0L),
    fraction = c(schema_fraction, scientific_fraction, provenance_fraction, artifact_fraction),
    reason = c(
      sprintf("%d of %d report sections passed canonical validation", sum(plan$sections$validation_passed), nrow(plan$sections)),
      if (nrow(validations)) sprintf("%d of %d explicit validation checks passed", sum(validations$passed), nrow(validations)) else "No explicit validation records were supplied",
      if (any(provenance_applicable)) sprintf("%d of %d core results include provenance", sum(vapply(plan$results[provenance_applicable], function(x) length(x$provenance) > 0L, logical(1L))), sum(provenance_applicable)) else "Not applicable",
      if (nrow(artifacts)) sprintf("%d required artifact files were checked", sum(artifacts$required)) else "No publication artifacts were declared"
    )
  )
  used <- components$applicable & is.finite(components$fraction)
  score <- if (any(used)) 100 * sum(components$weight[used] * components$fraction[used]) / sum(components$weight[used]) else NA_real_
  list(score = score, components = components, validations = validations)
}

dashboard_json_ready <- function(x) {
  if (data.table::is.data.table(x)) return(as.data.frame(x))
  if (is.list(x)) return(lapply(x, dashboard_json_ready))
  x
}

write_dashboard_qmd <- function(plan, summary, quality, path) {
  lines <- c(
    "---",
    paste0("title: \"", gsub("\"", "'", plan$title), "\""),
    "format:",
    "  html:",
    "    theme: cosmo",
    "    toc: true",
    "    toc-location: left",
    "    toc-depth: 3",
    "    code-fold: true",
    "    embed-resources: true",
    "    page-layout: full",
    "execute:",
    "  echo: false",
    "---",
    "",
    "```{r}",
    "plan <- readRDS('population_genomics_dashboard_plan.rds')",
    "```",
    "",
    "# Overview",
    "",
    "::: {.grid}",
    sprintf("::: {.g-col-2 .card}\n### Samples\n**%s**\n:::", summary$samples[[1L]]),
    sprintf("::: {.g-col-2 .card}\n### Analyses\n**%s**\n:::", summary$analyses[[1L]]),
    sprintf("::: {.g-col-2 .card}\n### Populations\n**%s**\n:::", summary$populations[[1L]]),
    sprintf("::: {.g-col-2 .card}\n### Recommended K\n**%s**\n:::", ifelse(is.na(summary$recommended_k[[1L]]), "N/A", summary$recommended_k[[1L]])),
    sprintf("::: {.g-col-2 .card}\n### Global FST\n**%s**\n:::", ifelse(is.na(summary$global_fst[[1L]]), "N/A", sprintf("%.4f", summary$global_fst[[1L]]))),
    sprintf("::: {.g-col-2 .card}\n### Quality score\n**%.1f%%**\n:::", quality$score),
    ":::",
    "",
    "The quality score is calculated only from explicit canonical validation, scientific checks, provenance completeness, and declared artifact integrity. Components marked not applicable are excluded from the denominator.",
    "",
    "## Quality components",
    "",
    "```{r}",
    "quality <- data.table::fread('scientific_quality_components.tsv')",
    "if (requireNamespace('DT', quietly = TRUE)) DT::datatable(quality, filter = 'top', options = list(pageLength = 10)) else knitr::kable(quality)",
    "```",
    "",
    "# Analyses"
  )
  for (i in seq_len(nrow(plan$sections))) {
    s <- plan$sections[i, ]
    lines <- c(lines,
      "",
      paste0("## ", s$title),
      "",
      paste0("Canonical class: `", s$class, "`."),
      "",
      "```{r}",
      paste0("result <- plan$results[[", i, "]]"),
      "tab <- if (inherits(result, 'PopgenVCFCoreResult')) core_result_table(result) else if (inherits(result, 'PopgenVCFAncestryResult')) ancestry_result_table(result) else if (inherits(result, 'PopgenVCFAncestryConsensus')) ancestry_consensus_table(result) else data.table::data.table()",
      "if (nrow(tab)) { if (requireNamespace('DT', quietly = TRUE)) DT::datatable(tab, filter = 'top', options = list(pageLength = 15, scrollX = TRUE)) else knitr::kable(utils::head(tab, 50)) }",
      "```"
    )
    if (identical(s$analysis, "pca")) {
      lines <- c(lines, "", "### Interactive PCA", "", "```{r}",
        "if (inherits(result, 'PopgenVCFPCAResult') && requireNamespace('plotly', quietly = TRUE)) {",
        "  d <- result$payload$coordinates",
        "  pc <- grep('^PC[0-9]+$', names(d), value = TRUE)",
        "  if (length(pc) >= 2L) print(plotly::plot_ly(d, x = d[[pc[1L]]], y = d[[pc[2L]]], text = d$sample_id, type = 'scatter', mode = 'markers', hoverinfo = 'text'))",
        "} else cat('Install plotly to enable the interactive PCA view.')",
        "```")
    }
    if (identical(s$analysis, "ancestry")) {
      lines <- c(lines, "", "### Interactive ancestry", "", "```{r}",
        "if (inherits(result, 'PopgenVCFAncestryConsensus') && requireNamespace('plotly', quietly = TRUE)) {",
        "  d <- ancestry_consensus_table(result)",
        "  print(plotly::plot_ly(d, x = ~sample_id, y = ~mean, color = ~cluster, type = 'bar') |> plotly::layout(barmode = 'stack'))",
        "} else if (inherits(result, 'PopgenVCFKSelection') && requireNamespace('plotly', quietly = TRUE)) {",
        "  d <- result$summary",
        "  print(plotly::plot_ly(d, x = ~k, y = ~mean, color = ~backend, type = 'scatter', mode = 'lines+markers'))",
        "} else cat('Install plotly and provide a consensus or K-selection object to enable the interactive ancestry view.')",
        "```")
    }
  }
  lines <- c(lines,
    "",
    "# Provenance and reproducibility",
    "",
    "The downloadable reproducibility bundle contains the report plan, section manifest, artifact manifest, validation records, parameters, provenance, environment information, command line, and input hashes when supplied.",
    "",
    "- [Dashboard summary JSON](dashboard_summary.json)",
    "- [Scientific quality JSON](scientific_quality.json)",
    "- [Provenance JSON](provenance.json)",
    "- [Parameters JSON](parameters.json)",
    "- [Reproducibility bundle](population_genomics_reproducibility.tar.gz)"
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' Write an interactive population-genomics dashboard
#'
#' @param results Named list of canonical results, or a `PopgenVCFReportPlan`.
#' @param output_dir Dashboard output directory.
#' @param title Dashboard title.
#' @param render Render the HTML dashboard with Quarto.
#' @param include,exclude Optional analysis filters.
#' @return A list containing plan, summary, quality, artifacts, and paths.
#' @export
write_population_genomics_dashboard <- function(results, output_dir,
                                                 title = "Population genomics dashboard",
                                                 render = TRUE,
                                                 include = NULL, exclude = NULL) {
  plan <- if (inherits(results, "PopgenVCFReportPlan")) results else
    build_population_genomics_report_plan(results, title, include, exclude)
  validate_population_genomics_report_plan(plan)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  summary <- build_dashboard_summary(plan)
  quality <- calculate_scientific_quality(plan)
  validations <- quality$validations
  artifacts_table <- dashboard_artifact_rows(plan)
  parameters <- lapply(plan$results, function(x) if (inherits(x, "PopgenVCFCoreResult")) x$parameters else list())
  names(parameters) <- plan$sections$analysis
  provenance <- lapply(plan$results, function(x) if (inherits(x, "PopgenVCFCoreResult")) x$provenance else list(class = class(x)[1L]))
  names(provenance) <- plan$sections$analysis

  paths <- list(
    qmd = file.path(output_dir, "population_genomics_dashboard.qmd"),
    plan = file.path(output_dir, "population_genomics_dashboard_plan.rds"),
    sections = file.path(output_dir, "dashboard_sections.tsv"),
    summary_tsv = file.path(output_dir, "dashboard_summary.tsv"),
    summary_json = file.path(output_dir, "dashboard_summary.json"),
    quality_tsv = file.path(output_dir, "scientific_quality_components.tsv"),
    quality_json = file.path(output_dir, "scientific_quality.json"),
    validation = file.path(output_dir, "validation_records.tsv"),
    artifact_manifest = file.path(output_dir, "analysis_artifact_manifest.tsv"),
    parameters = file.path(output_dir, "parameters.json"),
    provenance = file.path(output_dir, "provenance.json"),
    environment = file.path(output_dir, "environment.txt"),
    command = file.path(output_dir, "command_line.txt"),
    input_hashes = file.path(output_dir, "input_hashes.tsv"),
    bundle = file.path(output_dir, "population_genomics_reproducibility.tar.gz")
  )

  saveRDS(plan, paths$plan, version = 3)
  data.table::fwrite(plan$sections, paths$sections, sep = "\t", quote = FALSE)
  data.table::fwrite(summary, paths$summary_tsv, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(quality$components, paths$quality_tsv, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(validations, paths$validation, sep = "\t", quote = FALSE)
  data.table::fwrite(artifacts_table, paths$artifact_manifest, sep = "\t", quote = FALSE, na = "NA")
  jsonlite::write_json(dashboard_json_ready(summary), paths$summary_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
  jsonlite::write_json(list(score = quality$score, components = dashboard_json_ready(quality$components)), paths$quality_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
  jsonlite::write_json(dashboard_json_ready(parameters), paths$parameters, pretty = TRUE, auto_unbox = TRUE, null = "null")
  jsonlite::write_json(dashboard_json_ready(provenance), paths$provenance, pretty = TRUE, auto_unbox = TRUE, null = "null")
  writeLines(capture.output(utils::sessionInfo()), paths$environment, useBytes = TRUE)
  writeLines(paste(commandArgs(), collapse = " "), paths$command, useBytes = TRUE)

  hash_rows <- list()
  for (i in seq_along(plan$results)) {
    x <- plan$results[[i]]
    if (inherits(x, "PopgenVCFCoreResult") && !is.null(x$provenance$input_hashes)) {
      z <- data.table::as.data.table(x$provenance$input_hashes)
      z[, analysis := plan$sections$analysis[[i]]]
      hash_rows[[length(hash_rows) + 1L]] <- z
    }
  }
  hashes <- if (length(hash_rows)) data.table::rbindlist(hash_rows, fill = TRUE) else data.table::data.table()
  data.table::fwrite(hashes, paths$input_hashes, sep = "\t", quote = FALSE)

  write_dashboard_qmd(plan, summary, quality, paths$qmd)
  bundle_files <- basename(unlist(paths[c("plan", "sections", "summary_tsv", "summary_json", "quality_tsv", "quality_json", "validation", "artifact_manifest", "parameters", "provenance", "environment", "command", "input_hashes")]))
  old <- setwd(output_dir); on.exit(setwd(old), add = TRUE)
  utils::tar(basename(paths$bundle), files = bundle_files, compression = "gzip", tar = "internal")

  html <- character()
  if (isTRUE(render)) {
    quarto <- Sys.which("quarto")
    if (!nzchar(quarto)) stop("Quarto is required to render the dashboard; use render = FALSE to write sources only", call. = FALSE)
    status <- system2(quarto, c("render", basename(paths$qmd), "--to", "html", "--output-dir", "."), stdout = TRUE, stderr = TRUE)
    code <- attr(status, "status") %||% 0L
    if (code != 0L) stop("Quarto dashboard rendering failed: ", paste(status, collapse = "\n"), call. = FALSE)
    html <- file.path(output_dir, "population_genomics_dashboard.html")
  }

  declared <- list(
    new_analysis_artifact("dashboard", "source", "report", paths$qmd, "qmd", "Interactive dashboard source"),
    new_analysis_artifact("dashboard", "plan", "provenance", paths$plan, "rds", "Deterministic dashboard plan"),
    new_analysis_artifact("dashboard", "summary", "table", paths$summary_tsv, "tsv", "Dashboard summary metrics"),
    new_analysis_artifact("dashboard", "quality", "validation", paths$quality_tsv, "tsv", "Transparent scientific quality components"),
    new_analysis_artifact("dashboard", "bundle", "provenance", paths$bundle, "tar.gz", "Reproducibility bundle")
  )
  if (length(html)) declared[[length(declared) + 1L]] <- new_analysis_artifact("dashboard", "html", "report", html, "html", "Rendered interactive dashboard")
  manifest <- new_artifact_manifest(declared)
  validate_artifact_manifest(manifest, must_exist = TRUE)
  list(plan = plan, summary = summary, quality = quality, artifacts = manifest, paths = c(unlist(paths), html = html))
}
