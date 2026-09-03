report_figure_caption <- function(stem) {
  caption <- sub("^[0-9]+[[:alpha:]]?_", "", stem)
  caption <- gsub("_", " ", caption, fixed = TRUE)
  caption <- gsub("([[:alpha:]])([0-9]+)", "\\1 \\2", caption)
  caption <- trimws(gsub("[[:space:]]+", " ", caption))
  if (!nzchar(caption)) caption <- stem
  paste0(toupper(substr(caption, 1L, 1L)), substring(caption, 2L))
}

report_figure_inventory <- function(results_rds, target = c("html", "pdf"),
                                     max_pdf_figure_bytes = 2 * 1024^2) {
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
  # Every figure this package's own save_plot() call sites write uses a
  # numbered stem (e.g. "07_PCA_PC1_PC2", "24b_ROH_FROH_by_length_class") --
  # this is what determines both report inclusion and report ordering.
  # write_pca_publication_artifacts() (and its IBS/ancestry counterparts)
  # write a separate, un-numbered, publication-ready copy of the same
  # figure (e.g. "PCA_PC1_PC2.png") into this same directory, for a user to
  # pull directly into a manuscript -- confirmed directly against a real
  # production report: that duplicate PCA scatter plot, effectively
  # identical to the numbered one already in the report, was getting swept
  # into the report a second time, sorted to the very end (no numeric
  # prefix to place it by). Excluding un-numbered stems here keeps the
  # publication copies on disk (still reachable directly, or via the
  # written artifact manifest) without embedding them again.
  files <- files[grepl("^[0-9]{2}[[:alpha:]]?_", basename(files))]
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
  # A vector PDF draws one path/point object per plotted point -- fine for a
  # typical figure, but a dense per-locus/per-K scatter (Manhattan-style
  # loadings plots, allelic richness across a genome-wide marker set) can
  # carry tens of thousands of points, and pdflatex/xelatex just concatenates
  # each \includegraphics'd PDF's content stream as-is (no recompression).
  # Confirmed directly against a real production report: nine such figures
  # alone (11MB-126MB each) accounted for ~915MB of a 954MB report PDF, while
  # their PNG counterparts (identical visual content, rasterized) were
  # 0.5-30MB. Any on-disk PDF above this threshold is deprioritized below its
  # PNG sibling here so the report embeds the raster version instead --
  # legitimate vector figures (axes, bars, lines, sparse points) stay well
  # under it, so only the pathological cases are affected.
  if (identical(target, "pdf")) {
    oversized_pdf <- format == "pdf" & file.size(files) > max_pdf_figure_bytes
    preference[oversized_pdf] <- length(preferred_formats) + 1L
  }
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

# The PDF-target raster fallback in report_figure_inventory() above keeps any
# single figure from ballooning the report, but the sum of dozens of
# raster-embedded figures at their source 600 DPI still adds up (real
# production report: ~148MB of embedded figures even after that fallback) --
# too large to email. Ghostscript's pdfwrite device recompresses/downsamples
# a finished PDF's embedded raster images in place and leaves genuine vector
# content (text, paths, the smaller figures report_figure_inventory() left as
# vector PDF) untouched, so it's applied here as a final pass over the
# assembled report rather than reworking every figure-generating function.
# Preset chosen by direct comparison on the two largest real production
# figures (Manhattan-style DAPC/PCA loadings plots): /screen (72 DPI) and
# /ebook (150 DPI) both left dense multi-panel axis labels illegible;
# /printer (300 DPI) kept them legible while still compressing a 44MB
# 2-figure worst-case prototype to 1.1MB.
compress_report_pdf <- function(path) {
  gs <- Sys.which("gs")
  if (!nzchar(gs)) {
    log_msg(
      "ghostscript (gs) not found; report PDF was not compressed and may be too large to email",
      level = "WARNING"
    )
    return(invisible(path))
  }
  compressed <- tempfile(fileext = ".pdf")
  on.exit(unlink(compressed), add = TRUE)
  status <- system2(
    gs,
    c(
      "-sDEVICE=pdfwrite", "-dCompatibilityLevel=1.4", "-dPDFSETTINGS=/printer",
      "-dNOPAUSE", "-dQUIET", "-dBATCH", "-dSAFER",
      paste0("-sOutputFile=", compressed), path
    ),
    stdout = FALSE, stderr = FALSE
  )
  if (!identical(status, 0L) || !file.exists(compressed) || file.size(compressed) <= 0) {
    log_msg("Report PDF compression failed; keeping the uncompressed PDF", level = "WARNING")
    return(invisible(path))
  }
  before <- file.size(path)
  after <- file.size(compressed)
  if (after >= before) {
    return(invisible(path))
  }
  file.copy(compressed, path, overwrite = TRUE)
  log_msg(sprintf(
    "Compressed report PDF from %.1f MB to %.1f MB (%.0f%% reduction)",
    before / 1024^2, after / 1024^2, 100 * (1 - after / before)
  ), level = "SUCCESS")
  invisible(path)
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
#    page width. A tall table (e.g. LD decay's per-bin table, 100+ rows) is a
#    separate, worse problem: a plain (non-longtable) tabular block cannot
#    paginate at all, so its rows past the physical page bottom simply never
#    render anywhere -- confirmed directly on a real production report,
#    silently missing rows, not an error. report_kable() (skeleton.Rmd) always
#    builds the table via kable(..., longtable = TRUE) (with \endhead
#    inserted by hand so the header repeats on every page -- base knitr's
#    longtable support doesn't add that on its own), but only *displays* it
#    that way when the table's own \sbox-measured height exceeds ~82% of
#    \textheight -- \resizebox can't wrap a longtable at all (each page would
#    need independent measurement), so a genuinely tall table falls back to a
#    coarser discrete font size (\normalsize down to \scriptsize) picked
#    against \textwidth instead of \resizebox's continuous scaling. Every
#    other table (the common case) is displayed as a plain tabular, wrapped
#    in \resizebox exactly as before longtable existed here -- an
#    always-longtable-only earlier version of this function regressed that:
#    discrete sizing alone still let a real 10-32-numeric-column table
#    overflow \textwidth even at \scriptsize, where \resizebox had scaled it
#    to fit exactly. The height check itself must compare against
#    \ht\pgvcftablebox + \dp\pgvcftablebox, not \ht alone -- confirmed
#    directly: \ht alone came in just under the cutoff for a real 80-row
#    table (531pt vs. a 533pt threshold) while the combined height (1058pt)
#    was unambiguously over it, sending that table down the non-paginating
#    \resizebox branch and silently dropping its rows past the page bottom,
#    the exact defect this whole mechanism exists to prevent (pandoc's own
#    image-scaling macro also uses \ht+\dp, for the same reason).
#    \pgvcftablebox is declared once here so it can be reused (via \sbox, not
#    \newsavebox) across every report_table_section() call in the document.
#    This requires knitr::kable(..., format = "latex") to actually emit
#    LaTeX tabular/longtable source instead of its default pandoc-markdown
#    table syntax (report_kable() in skeleton.Rmd) -- raw \sbox{}{...}/
#    longtable content is never re-processed by pandoc's own
#    markdown-to-LaTeX pass, unlike a normal chunk's output -- and
#    booktabs/longtable, which pandoc would otherwise auto-load only when it
#    detects its own native table syntax in the source, which this raw LaTeX
#    bypasses. Also note: a raw-LaTeX line whose first character is a bare
#    "{" or "}" is not recognized by pandoc's raw-tex passthrough either --
#    confirmed directly, it rendered as a literal visible brace in the
#    compiled PDF -- so report_kable_render() (skeleton.Rmd) scopes with
#    \begingroup/\endgroup instead of a bare "{...}" group throughout.
report_pdf_preamble <- function() {
  path <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\usepackage{float}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
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
  if (identical(format, "pdf")) compress_report_pdf(final_path)
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
