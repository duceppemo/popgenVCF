report_figure_caption <- function(stem) {
  caption <- sub("^[0-9]+[[:alpha:]]?_", "", stem)
  caption <- gsub("_", " ", caption, fixed = TRUE)
  caption <- gsub("([[:alpha:]])([0-9]+)", "\\1 \\2", caption)
  caption <- trimws(gsub("[[:space:]]+", " ", caption))
  if (!nzchar(caption)) caption <- stem
  paste0(toupper(substr(caption, 1L, 1L)), substring(caption, 2L))
}

report_figure_inventory <- function(results_rds, target = c("html", "pdf")) {
  target <- match.arg(target)
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
  preferred_formats <- if (identical(target, "pdf")) {
    c("pdf", "png", "jpg", "jpeg", "webp", "svg")
  } else {
    c("svg", "png", "webp", "jpg", "jpeg", "pdf")
  }
  preference <- match(format, preferred_formats)
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

report_latex_engine <- function() {
  engines <- c("xelatex", "lualatex", "pdflatex")
  paths <- Sys.which(engines)
  available <- engines[nzchar(paths)]
  if (length(available)) available[[1L]] else NULL
}

# Two real LaTeX layout defects, both found only by rendering a report from a
# realistic real-data analysis (many figures, many-column tables) rather than
# the small fixtures used elsewhere:
#
# 1. Every PDF-branch figure is already placed one-per-page via an explicit
#    \newpage in skeleton.Rmd's figure gallery loop, but pandoc still wraps
#    each markdown image in a *floating* LaTeX figure environment by default
#    -- floats left unplaced (e.g. because a caption pushes an image past a
#    page break) stay queued rather than being discarded at \newpage, and
#    LaTeX's default float queue (18) is smaller than this report's real
#    figure count once every per-K DAPC/PCA loading figure is included,
#    producing a hard "Too many unprocessed floats" compile failure. Forcing
#    figure placement with the float package's [H] specifier makes every
#    figure place immediately in document order -- matching the
#    one-figure-per-page behavior the template already intends -- so nothing
#    is ever queued, regardless of figure count.
# 2. Wide result tables (e.g. the population diversity summary or PCA scores,
#    each 10+ columns on real data) overflow \textwidth in the PDF, rendering
#    as illegible overlapping text -- knitr::kable() alone does not know the
#    page width. report_table_section() (skeleton.Rmd) wraps LaTeX-output
#    tables in a \sbox measured against \textwidth, shrinking with
#    \resizebox only when the table is actually too wide (never stretching a
#    naturally narrow table to fill the page). \pgvcftablebox is declared
#    once here so it can be reused (via \sbox, not \newsavebox) across every
#    report_table_section() call in the document. This requires
#    knitr::kable(..., format = "latex") to actually emit LaTeX tabular
#    source instead of its default pandoc-markdown table syntax (report_kable()
#    in skeleton.Rmd) -- raw \sbox{}{...} content is never re-processed by
#    pandoc's own markdown-to-LaTeX pass, unlike a normal chunk's output --
#    and booktabs, which pandoc would otherwise auto-load only when it detects
#    its own native table syntax in the source, which this raw LaTeX bypasses.
report_pdf_preamble <- function() {
  path <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\usepackage{float}",
    "\\usepackage{booktabs}",
    "\\let\\oldfigure\\figure",
    "\\let\\endoldfigure\\endfigure",
    "\\renewenvironment{figure}[1][2]{",
    "  \\expandafter\\oldfigure\\expandafter[H]",
    "}{",
    "  \\endoldfigure",
    "}",
    "\\newsavebox{\\pgvcftablebox}"
  ), path)
  path
}

render_standard_report_format <- function(template, results_rds, output_dir,
                                          title, author, format,
                                          latex_engine = NULL) {
  figures <- report_figure_inventory(results_rds, target = format)
  output_file <- paste0("population_genomics_report.", format)
  output_format <- if (identical(format, "html")) {
    rmarkdown::html_document(
      toc = TRUE, toc_float = TRUE, number_sections = TRUE,
      self_contained = TRUE
    )
  } else {
    rmarkdown::pdf_document(
      toc = TRUE, number_sections = TRUE,
      latex_engine = latex_engine,
      includes = rmarkdown::includes(in_header = report_pdf_preamble())
    )
  }
  rmarkdown::render(
    template,
    output_format = output_format,
    output_file = output_file,
    output_dir = output_dir,
    params = list(
      results_rds = results_rds, title = title, author = author,
      figures = figures, report_format = format
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
}

#' Render a population-genomics report
#'
#' @param results_rds Serialized analysis results path.
#' @param output_dir Report output directory.
#' @param title Report title.
#' @param author Report author.
#' @param formats One or more of `html` and `pdf`. Both are produced by
#'   default. PDF rendering requires XeLaTeX, LuaLaTeX, or pdfLaTeX. When both
#'   formats are requested and LaTeX is unavailable, HTML is still produced
#'   and PDF is skipped with a warning.
#' @return Named rendered report paths, invisibly.
#' @export
render_report <- function(results_rds, output_dir,
                          title = "Population genomics analysis", author = "",
                          formats = c("html", "pdf")) {
  template <- system.file("rmarkdown", "templates", "popgenvcf_report", "skeleton", "skeleton.Rmd", package = "popgenVCF")
  if (!nzchar(template)) stop("Installed report template not found", call. = FALSE)
  if (!rmarkdown::pandoc_available()) stop("Pandoc is required to render the optional manuscript report", call. = FALSE)
  formats <- unique(tolower(as.character(formats)))
  if (!length(formats) || any(!formats %in% c("html", "pdf"))) {
    stop("Report formats must contain html, pdf, or both", call. = FALSE)
  }
  results_rds <- normalizePath(results_rds, mustWork = TRUE)
  ensure_dir(output_dir)
  latex_engine <- if ("pdf" %in% formats) report_latex_engine() else NULL
  if ("pdf" %in% formats && is.null(latex_engine)) {
    message <- paste(
      "PDF report requires xelatex, lualatex, or pdflatex;",
      "no LaTeX engine was found"
    )
    if (identical(formats, "pdf")) stop(message, call. = FALSE)
    warning(paste0(message, "; generating HTML only"), call. = FALSE)
    formats <- setdiff(formats, "pdf")
  }
  paths <- vapply(formats, function(format) {
    render_standard_report_format(
      template, results_rds, output_dir, title, author, format,
      latex_engine = if (identical(format, "pdf")) latex_engine else NULL
    )
  }, character(1L))
  invisible(paths)
}

write_manifest <- function(cfg, dirs, analysis, timings = NULL) {
  validate_analysis(analysis)
  metadata <- analysis$samples$metadata
  qc_snps <- analysis$variants$qc_ids
  final_snps <- analysis$variants$ld_ids
  timings <- timings %||% analysis$timings
  metadata_supplied <- !is.null(cfg$input$metadata)
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
      if (metadata_supplied) normalizePath(cfg$input$metadata) else NA_character_,
      if (metadata_supplied) hash_file(cfg$input$metadata) else NA_character_,
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
