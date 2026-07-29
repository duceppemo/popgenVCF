report_figure_caption <- function(stem) {
  caption <- sub("^[0-9]+[[:alpha:]]?_", "", stem)
  caption <- gsub("_", " ", caption, fixed = TRUE)
  caption <- gsub("([[:alpha:]])([0-9]+)", "\\1 \\2", caption)
  caption <- trimws(gsub("[[:space:]]+", " ", caption))
  if (!nzchar(caption)) caption <- stem
  paste0(toupper(substr(caption, 1L, 1L)), substring(caption, 2L))
}

report_figure_inventory <- function(results_rds) {
  results_rds <- normalizePath(results_rds, mustWork = TRUE)
  figure_dir <- file.path(dirname(results_rds), "figures")
  if (!dir.exists(figure_dir)) {
    return(data.frame(
      stem = character(), caption = character(), path = character(),
      format = character(), stringsAsFactors = FALSE
    ))
  }
  files <- list.files(
    figure_dir,
    pattern = "\\.(svg|png|jpe?g|webp|pdf)$",
    full.names = TRUE, ignore.case = TRUE
  )
  if (!length(files)) {
    return(data.frame(
      stem = character(), caption = character(), path = character(),
      format = character(), stringsAsFactors = FALSE
    ))
  }
  format <- tolower(tools::file_ext(files))
  stem <- tools::file_path_sans_ext(basename(files))
  preference <- match(format, c("svg", "png", "webp", "jpg", "jpeg", "pdf"))
  inventory <- data.frame(
    stem = stem,
    caption = vapply(stem, report_figure_caption, character(1L)),
    path = normalizePath(files, mustWork = TRUE),
    format = format,
    preference = preference,
    stringsAsFactors = FALSE
  )
  inventory <- inventory[order(
    inventory$stem, inventory$preference, inventory$path
  ), , drop = FALSE]
  inventory <- inventory[!duplicated(inventory$stem), , drop = FALSE]
  inventory$preference <- NULL
  rownames(inventory) <- NULL
  inventory
}

#' Render a population-genomics report
#'
#' @param results_rds Serialized analysis results path.
#' @param output_dir Report output directory.
#' @param title Report title.
#' @param author Report author.
#' @return The rendered report path, invisibly.
#' @export
render_report <- function(results_rds, output_dir, title = "Population genomics analysis", author = "") {
  template <- system.file("rmarkdown", "templates", "popgenvcf_report", "skeleton", "skeleton.Rmd", package = "popgenVCF")
  if (!nzchar(template)) stop("Installed report template not found", call. = FALSE)
  if (!rmarkdown::pandoc_available()) stop("Pandoc is required to render the optional manuscript report", call. = FALSE)
  results_rds <- normalizePath(results_rds, mustWork = TRUE)
  figures <- report_figure_inventory(results_rds)
  ensure_dir(output_dir)
  out <- rmarkdown::render(
    template,
    output_file = "population_genomics_report.html",
    output_dir = output_dir,
    params = list(
      results_rds = results_rds, title = title, author = author,
      figures = figures
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  invisible(out)
}

write_manifest <- function(cfg, dirs, analysis, timings = NULL) {
  validate_analysis(analysis)
  metadata <- analysis$samples$metadata
  qc_snps <- analysis$variants$qc_ids
  final_snps <- analysis$variants$ld_ids
  timings <- timings %||% analysis$timings
  manifest <- data.table::data.table(
    field = c(
      "pipeline_version", "analysis_schema", "analysis_date", "vcf",
      "vcf_sha256", "metadata", "metadata_sha256", "samples",
      "populations", "qc_snps", "ld_snps", "maf",
      "variant_missing", "ld_r2", "ld_threads"
    ),
    value = c(
      popgenvcf_version(), analysis$schema_version,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      normalizePath(cfg$input$vcf), hash_file(cfg$input$vcf),
      normalizePath(cfg$input$metadata), hash_file(cfg$input$metadata),
      nrow(metadata), data.table::uniqueN(metadata$population),
      length(qc_snps), length(final_snps), cfg$qc$maf, 0.2, 0.2,
      max(1L, min(as.integer(cfg$compute$threads), 4L))
    )
  )
  write_tsv(manifest, file.path(dirs$root, "run_manifest.tsv"))
  if (length(timings)) {
    write_tsv(
      data.table::data.table(
        stage = names(timings),
        elapsed_seconds = as.numeric(unlist(timings))
      ),
      file.path(dirs$root, "stage_timings.tsv")
    )
  }
}
