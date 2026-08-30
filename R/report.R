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
    natural_sort_key(inventory$stem), inventory$preference, inventory$path
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
  intermediates_dir <- tempfile(paste0("popgenvcf-report-", format, "-"))
  dir.create(intermediates_dir, recursive = TRUE)
  on.exit(unlink(intermediates_dir, recursive = TRUE), add = TRUE)
  # A per-format *render* output directory too, not just intermediates_dir
  # above: HTML and PDF both render from the same source template basename
  # into the same shared `output_dir`, so rmarkdown's own companion
  # "<basename>_files/" supporting-image directory lands at the identical
  # path for both concurrently forked renders (render_report_formats()
  # below). A real, confirmed race, not a hypothetical one: HTML's
  # self_contained = TRUE finalization step embeds every image and then
  # deletes that shared directory once done, which can happen while the PDF
  # format's xelatex/xdvipdfmx compiler is still reading images out of it --
  # "Image inclusion failed: Could not find file" for an image that
  # genuinely no longer exists, not a flaky/nondeterministic LaTeX error.
  # Confirmed directly: PDF alone and HTML alone each render cleanly every
  # time; only the concurrent combination fails, nondeterministically,
  # depending on fork scheduling. Rendering into its own scratch directory
  # and copying out only the final document afterward removes all shared
  # state between the two forked renders.
  render_output_dir <- tempfile(paste0("popgenvcf-report-out-", format, "-"))
  dir.create(render_output_dir, recursive = TRUE)
  on.exit(unlink(render_output_dir, recursive = TRUE), add = TRUE)
  rmarkdown::render(
    template,
    output_format = output_format,
    output_file = output_file,
    output_dir = render_output_dir,
    intermediates_dir = intermediates_dir,
    params = list(
      results_rds = results_rds, title = title, author = author,
      figures = figures, report_format = format
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  final_path <- file.path(output_dir, output_file)
  file.copy(file.path(render_output_dir, output_file), final_path, overwrite = TRUE)
  final_path
}

# Renders each requested format via `render_one(format)`. HTML and PDF share
# the same input template (system.file() -- installed, possibly read-only
# outside this directory), so per-format intermediates_dir isolation above is
# what makes concurrent rendering safe: without it, both formats would race
# on the same knitr intermediate file names. Uses fork-based parallelism
# (mclapply): each worker is a live copy of the calling process, so no
# package-namespace resolution or object-export step is needed, unlike a
# PSOCK cluster. Windows has no fork, so it always falls back to the
# original sequential path; `parallel = FALSE` forces that same sequential
# path on any platform (render_report() uses this for a report large enough
# that concurrent rendering's roughly-doubled peak memory is a real risk).
# mclapply() does not propagate worker errors as R conditions -- it returns
# a "try-error" object in the failed slot instead -- so failures are
# detected and re-signalled explicitly to preserve the same error-throwing
# behaviour the sequential path already has.
render_report_formats <- function(formats, render_one, parallel = TRUE) {
  if (length(formats) <= 1L || identical(.Platform$OS.type, "windows") || !isTRUE(parallel)) {
    return(stats::setNames(vapply(formats, render_one, character(1L)), formats))
  }
  results <- parallel::mclapply(
    formats, render_one,
    mc.cores = length(formats), mc.preschedule = FALSE
  )
  check_mclapply_results(results, formats, "report rendering")
  stats::setNames(unlist(results), formats)
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
#'   and PDF is skipped with a warning. When both formats are requested on a
#'   non-Windows platform, they render concurrently in forked worker
#'   processes -- unless the report has more than `max_concurrent_figures`
#'   embedded figures, in which case they render sequentially instead, to
#'   keep peak memory bounded on a very large report (a real production
#'   report with 300+ figures was killed, likely by the OS, while rendering
#'   both formats concurrently). Windows (no fork support) always renders
#'   sequentially regardless.
#' @param max_concurrent_figures Figure-count threshold above which
#'   concurrent HTML+PDF rendering is disabled in favor of sequential
#'   rendering (slower, but with roughly half the peak memory).
#' @return Named rendered report paths, invisibly.
#' @export
render_report <- function(results_rds, output_dir,
                          title = "Population genomics analysis", author = "",
                          formats = c("html", "pdf"), max_concurrent_figures = 100L) {
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
  n_figures <- nrow(report_figure_inventory(results_rds, "pdf"))
  render_parallel <- length(formats) > 1L && n_figures <= as.integer(max_concurrent_figures)
  if (length(formats) > 1L && !render_parallel) {
    log_msg(sprintf(
      "Rendering %d embedded figures sequentially, not concurrently, across %s formats (exceeds max_concurrent_figures = %d)",
      n_figures, paste(formats, collapse = "/"), as.integer(max_concurrent_figures)
    ))
  }
  paths <- render_report_formats(formats, function(format) {
    render_standard_report_format(
      template, results_rds, output_dir, title, author, format,
      latex_engine = if (identical(format, "pdf")) latex_engine else NULL
    )
  }, parallel = render_parallel)
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
      length(qc_snps), length(final_snps), cfg$qc$maf,
      cfg$qc$max_variant_missing, cfg$qc$ld_r2,
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
