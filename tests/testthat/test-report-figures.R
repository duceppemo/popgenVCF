minimal_standard_report_result <- function() {
  list(
    metadata = data.frame(
      sample = c("sample_1", "sample_2"),
      population = c("north", "south")
    ),
    qc_snps = 1:4,
    final_snps = 1:3,
    qc = list(sequential = data.frame(stage = "retained", variants = 4L)),
    diversity = list(population = data.frame(
      population = c("north", "south"),
      observed_heterozygosity = c(0.2, 0.3)
    )),
    pca = list(scores = data.frame(
      sample = c("sample_1", "sample_2"), PC1 = c(-1, 1), PC2 = c(0, 0)
    )),
    fst = list(
      global = 0.1,
      long = data.frame(population_1 = "north", population_2 = "south", fst = 0.1)
    ),
    dapc = NULL,
    amova = NULL,
    ibd = NULL,
    admixture_cv = NULL,
    chromosome_summary = NULL,
    faststructure = NULL,
    snmf = NULL
  )
}

test_that("standard report inventory selects one browser-friendly figure per stem", {
  root <- tempfile("standard-report-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  png_path <- file.path(root, "figures", "07_PCA_PC1_PC2.png")
  grDevices::png(png_path, width = 320, height = 240)
  graphics::plot(1:2, 1:2)
  grDevices::dev.off()
  grDevices::pdf(file.path(root, "figures", "07_PCA_PC1_PC2.pdf"))
  graphics::plot(1:2, 1:2)
  grDevices::dev.off()

  inventory <- popgenVCF:::report_figure_inventory(results)
  expect_equal(nrow(inventory), 1L)
  expect_identical(inventory$format, "png")
  expect_identical(inventory$stem, "07_PCA_PC1_PC2")
  expect_match(inventory$caption, "PCA PC 1 PC 2", fixed = TRUE)

  pdf_inventory <- popgenVCF:::report_figure_inventory(results, target = "pdf")
  expect_equal(nrow(pdf_inventory), 1L)
  expect_identical(pdf_inventory$format, "pdf")
})

test_that("PDF report inventory falls back to PNG for a pathologically large vector PDF", {
  root <- tempfile("oversized-pdf-report-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  small_stem <- "07_PCA_PC1_PC2"
  big_stem <- "15_DAPC_loadings_manhattan_K10"
  for (stem in c(small_stem, big_stem)) {
    grDevices::png(file.path(root, "figures", paste0(stem, ".png")), width = 320, height = 240)
    graphics::plot(1:2, 1:2)
    grDevices::dev.off()
    grDevices::pdf(file.path(root, "figures", paste0(stem, ".pdf")))
    graphics::plot(1:2, 1:2)
    grDevices::dev.off()
  }
  # Pad the big stem's on-disk PDF past the default 2MB threshold, standing
  # in for a real dense Manhattan-style vector PDF (tens of thousands of
  # points) without needing to actually plot that many. Content doesn't need
  # to be a valid PDF -- report_figure_inventory() only inspects extension
  # and file size, never parses it.
  big_pdf_path <- file.path(root, "figures", paste0(big_stem, ".pdf"))
  con <- file(big_pdf_path, "ab")
  writeBin(as.raw(rep(0L, 3 * 1024^2)), con)
  close(con)
  expect_gt(file.size(big_pdf_path), 2 * 1024^2)

  pdf_inventory <- popgenVCF:::report_figure_inventory(results, target = "pdf")
  pdf_inventory <- pdf_inventory[order(pdf_inventory$stem), ]
  expect_identical(pdf_inventory$stem, c(small_stem, big_stem))
  expect_identical(pdf_inventory$format, c("pdf", "png"))
})

test_that("compress_report_pdf skips gracefully when ghostscript is unavailable", {
  local_mocked_bindings(Sys.which = function(...) c(gs = ""), .package = "base")
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path); graphics::plot(1:2, 1:2); grDevices::dev.off()
  before <- file.size(path)

  expect_output(
    result <- popgenVCF:::compress_report_pdf(path),
    "ghostscript"
  )
  expect_identical(result, path)
  expect_identical(file.size(path), before)
})

test_that("compress_report_pdf shrinks a real PDF's embedded raster image without corrupting it", {
  skip_if_not(nzchar(Sys.which("gs")), "ghostscript is not available")

  root <- tempfile("compress-pdf-")
  dir.create(root)
  path <- file.path(root, "report.pdf")
  # A smooth gradient, oversampled well past the /printer preset's ~450 DPI
  # downsample threshold on this small a page (1200px over 2in = 600 DPI),
  # gives ghostscript real, predictable recompression work to do -- unlike a
  # bare vector plot (what a real small report figure already is, and what
  # compress_report_pdf() is expected to leave untouched).
  x <- seq(0, 1, length.out = 1200)
  gradient <- outer(x, x, function(a, b) (a + b) / 2)
  raster <- array(0, dim = c(1200, 1200, 3))
  raster[, , 1] <- gradient
  raster[, , 2] <- 1 - gradient
  raster[, , 3] <- 0.5
  grDevices::pdf(path, width = 2, height = 2)
  grid::grid.raster(raster)
  grDevices::dev.off()
  before <- file.size(path)

  result <- popgenVCF:::compress_report_pdf(path)

  expect_identical(result, path)
  expect_true(file.exists(path))
  expect_lt(file.size(path), before)
  header <- readBin(path, "raw", 5L)
  expect_identical(rawToChar(header), "%PDF-")
})

test_that("report figure inventory orders embedded K numbers naturally, not lexicographically", {
  root <- tempfile("k-order-report-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  for (k in c(2L, 9L, 10L)) {
    png_path <- file.path(root, "figures", sprintf("11_DAPC_K%d.png", k))
    grDevices::png(png_path, width = 320, height = 240)
    graphics::plot(1:2, 1:2)
    grDevices::dev.off()
  }

  inventory <- popgenVCF:::report_figure_inventory(results)
  expect_identical(inventory$stem, c("11_DAPC_K2", "11_DAPC_K9", "11_DAPC_K10"))
})

test_that("report displays the popgenVCF version when present, and omits it silently when absent", {
  skip_if_not(rmarkdown::pandoc_available())

  root_with <- tempfile("report-version-present-")
  dir.create(root_with)
  results_with <- file.path(root_with, "analysis_results.rds")
  with_version <- minimal_standard_report_result()
  with_version$package_version <- "9.9.9"
  saveRDS(with_version, results_with)
  rendered_with <- render_report(
    results_with, file.path(root_with, "report"), title = "Versioned report",
    formats = "html"
  )
  html_with <- paste(readLines(rendered_with[["html"]], warn = FALSE), collapse = "\n")
  expect_match(html_with, "popgenVCF v9.9.9", fixed = TRUE)

  root_without <- tempfile("report-version-absent-")
  dir.create(root_without)
  results_without <- file.path(root_without, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results_without)
  rendered_without <- render_report(
    results_without, file.path(root_without, "report"), title = "Unversioned report",
    formats = "html"
  )
  html_without <- paste(readLines(rendered_without[["html"]], warn = FALSE), collapse = "\n")
  expect_no_match(html_without, "popgenVCF v", fixed = TRUE)
})

test_that("PDF report tables paginate a tall table without losing rows and wrap a long message instead of shrinking the whole table illegibly", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if_not(nzchar(popgenVCF:::report_latex_engine() %||% ""), "no LaTeX engine available")
  skip_if(Sys.which("pdftotext") == "", "pdftotext is not available")

  root <- tempfile("pdf-table-layout-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results_path <- file.path(root, "analysis_results.rds")
  result <- minimal_standard_report_result()
  # Tall enough that it cannot possibly fit on one page at any font size --
  # a real production report silently lost this exact shape of table's tail
  # rows past the physical page bottom before longtable pagination was
  # added to report_kable().
  result$qc$sequential <- data.frame(
    step = sprintf("step_%03d", 1:80),
    variants = 1:80
  )
  result$messages <- data.frame(
    stage = "kinship", level = "WARNING",
    message = paste(
      "This is a deliberately long pipeline-notice message meant to force",
      "wrapping instead of shrinking the whole table illegibly to fit one line."
    )
  )
  saveRDS(result, results_path)

  rendered <- render_report(
    results_path, file.path(root, "report"), title = "Layout test",
    formats = "pdf"
  )
  text <- system2("pdftotext", c("-layout", rendered[["pdf"]], "-"), stdout = TRUE)
  text <- paste(text, collapse = "\n")

  expect_match(text, "step_001", fixed = TRUE)
  expect_match(text, "step_080", fixed = TRUE)
  expect_match(text, "deliberately long pipeline-notice message", fixed = TRUE)

  if (nzchar(Sys.which("pdfinfo"))) {
    info <- system2("pdfinfo", rendered[["pdf"]], stdout = TRUE)
    pages <- as.integer(sub("^Pages:\\s*", "", grep("^Pages:", info, value = TRUE)))
    expect_gt(pages, 1L)
  }
})

test_that("standard HTML report embeds figures without requiring an RDS viewer", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("standard-report-render-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  png_path <- file.path(root, "figures", "03_sample_missingness.png")
  grDevices::png(png_path, width = 320, height = 240)
  graphics::barplot(c(0.1, 0.2), names.arg = c("sample_1", "sample_2"))
  grDevices::dev.off()

  report_dir <- file.path(root, "report")
  rendered <- render_report(
    results, report_dir, title = "Portable report", formats = "html"
  )
  expect_true(file.exists(rendered))
  expect_identical(names(rendered), "html")
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")
  expect_match(html, "Figure gallery", fixed = TRUE)
  expect_match(html, "data:image/png;base64", fixed = TRUE)
  expect_match(html, "03_sample_missingness.png", fixed = TRUE)
  expect_match(html, "reproducibility archive", fixed = TRUE)
})

test_that("reduced HTML reports omit unavailable analysis sections", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("reduced-standard-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  reduced <- minimal_standard_report_result()
  reduced[c(
    "diversity", "fst", "dapc", "amova", "ibd", "admixture_cv",
    "chromosome_summary", "faststructure", "snmf"
  )] <- NULL
  saveRDS(reduced, results)

  rendered <- render_report(
    results, file.path(root, "report"), title = "Reduced report",
    formats = "html"
  )
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_match(html, 'id="quality-control"', fixed = TRUE)
  expect_match(html, 'id="principal-component-analysis"', fixed = TRUE)
  expect_no_match(html, "The global Weir-Cockerham", fixed = TRUE)
  expect_no_match(html, 'id="genetic-diversity"', fixed = TRUE)
  expect_no_match(html, 'id="population-differentiation"', fixed = TRUE)
  expect_no_match(
    html, 'id="discriminant-analysis-of-principal-components"', fixed = TRUE
  )
  expect_no_match(html, 'id="analysis-of-molecular-variance"', fixed = TRUE)
  expect_no_match(html, 'id="admixture-model-cross-validation"', fixed = TRUE)
  expect_no_match(
    html, 'id="population-structure-reproducibility"', fixed = TRUE
  )
})

test_that("populated reproducibility sections render mixed-type tables", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("populated-reproducibility-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  populated <- minimal_standard_report_result()
  populated$dapc <- list(diagnostics = data.table::data.table(
    K = 2L,
    assignment_accuracy = 0.8,
    replicate_max_rmse = 0.01
  ))
  populated$faststructure <- list(runs = data.table::data.table(
    K = 2L,
    exit_status = 0L,
    executable = "structure.py",
    log_file = "faststructure_K2.log",
    q_file = "faststructure_K2.meanQ"
  ))
  populated$snmf <- list(diagnostics = data.table::data.table(
    K = 2L, run = 1L, cross_entropy = 0.5
  ))
  saveRDS(populated, results)

  rendered <- expect_no_error(render_report(
    results, file.path(root, "report"),
    title = "Populated reproducibility report", formats = "html"
  ))
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_match(
    html, 'id="population-structure-reproducibility"', fixed = TRUE
  )
  expect_match(html, 'id="faststructure"', fixed = TRUE)
  expect_match(
    html, 'id="sparse-non-negative-matrix-factorization"', fixed = TRUE
  )
  expect_match(html, "structure.py", fixed = TRUE)
})

test_that("standard PDF report uses compact vector figure sources", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if(is.null(popgenVCF:::report_latex_engine()), "No LaTeX engine")
  root <- tempfile("standard-report-pdf-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  pdf_figure <- file.path(root, "figures", "07_PCA_PC1_PC2.pdf")
  grDevices::pdf(pdf_figure, width = 7, height = 5)
  graphics::plot(1:2, 1:2, main = "Principal component analysis")
  grDevices::dev.off()
  png_figure <- file.path(root, "figures", "07_PCA_PC1_PC2.png")
  grDevices::png(png_figure, width = 1200, height = 900)
  graphics::plot(1:2, 1:2, main = "Principal component analysis")
  grDevices::dev.off()

  rendered <- render_report(
    results, file.path(root, "report"), title = "Compact report",
    formats = "pdf"
  )
  expect_identical(names(rendered), "pdf")
  expect_true(file.exists(rendered[["pdf"]]))
  header <- readBin(rendered[["pdf"]], what = "raw", n = 4L)
  expect_identical(rawToChar(header), "%PDF")
  expect_lt(file.info(rendered[["pdf"]])$size, 1e6)
})

test_that("requesting both formats together renders each correctly, whether concurrently or sequentially", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if(is.null(popgenVCF:::report_latex_engine()), "No LaTeX engine")
  root <- tempfile("both-formats-report-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  rendered <- render_report(
    results, file.path(root, "report"), title = "Both formats report",
    formats = c("html", "pdf")
  )

  expect_identical(names(rendered), c("html", "pdf"))
  expect_true(file.exists(rendered[["html"]]))
  expect_true(file.exists(rendered[["pdf"]]))
  header <- readBin(rendered[["pdf"]], what = "raw", n = 4L)
  expect_identical(rawToChar(header), "%PDF")
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")
  expect_match(html, "Both formats report", fixed = TRUE)
})

test_that("each report format renders into its own isolated output directory, never the shared destination or each other's", {
  # Real regression, found on a real full pipeline report (not this file's
  # own minimal fixtures, which apparently never hit it): HTML and PDF
  # render from the same source template basename into the same shared
  # destination output_dir, concurrently, via render_report_formats()'s
  # mclapply(). rmarkdown creates a companion "<basename>_files/" directory
  # for supporting images at that identical shared path for both forked
  # renders; HTML's self_contained = TRUE finalization step deletes it once
  # its own images are embedded, which can race ahead of the PDF format's
  # xelatex/xdvipdfmx still reading images from that same path -- "Image
  # inclusion failed: Could not find file" for a real, existing report.
  # Confirmed directly against the real failure: PDF alone and HTML alone
  # each rendered cleanly on every repeated attempt; only the concurrent
  # combination failed, nondeterministically, depending on fork scheduling.
  # Testing the race itself would make this test just as nondeterministic
  # in the other direction, so this instead pins the fix's actual
  # invariant: rmarkdown::render() must never be called with the shared
  # destination as its own `output_dir`, and the two formats must never
  # share a render output_dir with each other either -- regardless of
  # timing, and without needing a real LaTeX installation to check it.
  root <- tempfile("report-isolation-")
  dir.create(root, recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)
  dest_dir <- file.path(root, "report")
  dir.create(dest_dir, recursive = TRUE)
  template <- system.file(
    "rmarkdown", "templates", "popgenvcf_report", "skeleton", "skeleton.Rmd",
    package = "popgenVCF"
  )

  captured_dirs <- list()
  local_mocked_bindings(
    render = function(input, output_format, output_file, output_dir,
                      intermediates_dir, params, envir, quiet) {
      captured_dirs[[params$report_format]] <<- output_dir
      writeLines("stub", file.path(output_dir, output_file))
      invisible(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  path_html <- popgenVCF:::render_standard_report_format(
    template, results, dest_dir, "Test", "Test", "html"
  )
  path_pdf <- popgenVCF:::render_standard_report_format(
    template, results, dest_dir, "Test", "Test", "pdf"
  )

  expect_false(identical(captured_dirs[["html"]], dest_dir))
  expect_false(identical(captured_dirs[["pdf"]], dest_dir))
  expect_false(identical(captured_dirs[["html"]], captured_dirs[["pdf"]]))
  expect_identical(path_html, file.path(dest_dir, "population_genomics_report.html"))
  expect_identical(path_pdf, file.path(dest_dir, "population_genomics_report.pdf"))
  expect_true(file.exists(path_html))
  expect_true(file.exists(path_pdf))
})

test_that("render_report_formats renders a single format sequentially", {
  calls <- character()
  result <- popgenVCF:::render_report_formats("html", function(format) {
    calls <<- c(calls, format)
    paste0("path-", format)
  })
  expect_identical(result, c(html = "path-html"))
  expect_identical(calls, "html")
})

test_that("render_report_formats renders multiple formats and preserves naming/order", {
  skip_on_os("windows")
  result <- popgenVCF:::render_report_formats(c("html", "pdf"), function(format) {
    Sys.sleep(0.05)
    paste0("path-", format)
  })
  expect_identical(result, c(html = "path-html", pdf = "path-pdf"))
})

test_that("render_report_formats re-signals a worker error as a real R condition", {
  skip_on_os("windows")
  expect_error(
    suppressWarnings(popgenVCF:::render_report_formats(c("html", "pdf"), function(format) {
      if (identical(format, "pdf")) stop("simulated render failure")
      "path-html"
    })),
    "simulated render failure"
  )
})

test_that("render_report_formats reports a clear error when a worker is killed rather than erroring normally", {
  # Real regression: a forked mclapply() worker killed by a signal (OOM,
  # segfault -- confirmed on a real 300+-figure report) returns NULL, not a
  # "try-error", for that slot. The old code only checked for "try-error"
  # and would let unlist() silently drop the NULL, misreporting a hard
  # crash as a clean (if quietly incomplete) success.
  skip_on_os("windows")
  expect_error(
    popgenVCF:::render_report_formats(c("html", "pdf"), function(format) {
      if (identical(format, "pdf")) return(NULL)
      "path-html"
    }),
    "terminated abnormally"
  )
})

test_that("render_report_formats's parallel = FALSE forces the sequential path even for multiple formats on a non-Windows platform", {
  skip_on_os("windows")
  pids <- character()
  result <- popgenVCF:::render_report_formats(c("html", "pdf"), function(format) {
    pids <<- c(pids, Sys.getpid())
    paste0("path-", format)
  }, parallel = FALSE)
  expect_identical(result, c(html = "path-html", pdf = "path-pdf"))
  # Sequential (non-forked) rendering runs both closures in this same process.
  expect_length(unique(pids), 1L)
})

test_that("render_report switches to sequential rendering above max_concurrent_figures, and stays concurrent at or below it", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if(is.null(popgenVCF:::report_latex_engine()), "No LaTeX engine")
  skip_on_os("windows")

  make_run <- function(n_figures) {
    root <- tempfile("report-size-guard-")
    dir.create(file.path(root, "figures"), recursive = TRUE)
    for (i in seq_len(n_figures)) {
      writeLines("", file.path(root, "figures", sprintf("%02d_figure.png", i)))
    }
    results <- file.path(root, "analysis_results.rds")
    saveRDS(minimal_standard_report_result(), results)
    results
  }

  captured <- new.env()
  testthat::local_mocked_bindings(
    render_report_formats = function(formats, render_one, parallel = TRUE) {
      captured$parallel <- parallel
      stats::setNames(vapply(formats, function(f) tempfile(fileext = paste0(".", f)), character(1L)), formats)
    },
    .package = "popgenVCF"
  )

  small <- make_run(3L)
  popgenVCF::render_report(small, tempfile("report-out-"), formats = c("html", "pdf"), max_concurrent_figures = 5L)
  expect_true(captured$parallel)

  large <- make_run(8L)
  popgenVCF::render_report(large, tempfile("report-out-"), formats = c("html", "pdf"), max_concurrent_figures = 5L)
  expect_false(captured$parallel)
})

test_that("reduced HTML reports omit the loading/private-allele sections when the data is absent", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("reduced-loadings-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  rendered <- render_report(
    results, file.path(root, "report"), title = "No loadings report",
    formats = "html"
  )
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_no_match(html, "Private alleles", fixed = TRUE)
  expect_no_match(html, "PCA SNP loadings", fixed = TRUE)
  expect_no_match(html, "DAPC SNP loadings", fixed = TRUE)
})

test_that("HTML reports include the loading and private-allele sections when the data is present", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("populated-loadings-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  populated <- minimal_standard_report_result()
  populated$diversity$locus <- data.table::data.table(
    population = c("north", "south", "north"),
    snp_id = c("1", "1", "2"),
    chromosome = c("1", "1", "1"),
    position = c(100L, 100L, 200L),
    private_allele = c("alt", "none", "none"),
    alternate_allele_count = c(4L, 0L, 2L),
    reference_allele_count = c(0L, 8L, 6L)
  )
  populated$pca$loadings <- data.table::data.table(
    axis = c("PC1", "PC1"), snp_id = c("1", "2"),
    chromosome = c("1", "1"), position = c(100L, 200L),
    contribution = c(-0.8, 0.2), magnitude = c(0.8, 0.2)
  )
  populated$dapc <- list(
    diagnostics = data.table::data.table(K = 2L, assignment_accuracy = 0.9),
    models = list(`2` = list(loadings = data.table::data.table(
      axis = c("LD1", "LD1"), snp_id = c("1", "2"),
      chromosome = c("1", "1"), position = c(100L, 200L),
      contribution = c(0.7, 0.1)
    )))
  )
  saveRDS(populated, results)

  rendered <- render_report(
    results, file.path(root, "report"), title = "With loadings report",
    formats = "html"
  )
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_match(html, "Private alleles", fixed = TRUE)
  expect_match(html, "PCA SNP loadings", fixed = TRUE)
  expect_match(html, "DAPC SNP loadings", fixed = TRUE)
  expect_match(html, "32_private_alleles.tsv", fixed = TRUE)
  expect_match(html, "31_PCA_loadings.tsv", fixed = TRUE)
})

test_that("HTML reports show a Pipeline notices section with WARNING/INFO messages, filtering out routine SUCCESS entries", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("pipeline-notices-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  populated <- minimal_standard_report_result()
  populated$messages <- data.table::data.table(
    timestamp = Sys.time(),
    level = c("SUCCESS", "WARNING", "INFO"),
    stage = c("VCF preparation", "clonality", "VCF preparation"),
    message = c(
      "completed",
      "some real warning text",
      "5 of 7 VCF record(s) are biallelic SNPs (2 dropped)"
    )
  )
  saveRDS(populated, results)

  rendered <- render_report(results, file.path(root, "report"), formats = "html")
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_match(html, "Pipeline notices", fixed = TRUE)
  expect_match(html, "some real warning text", fixed = TRUE)
  expect_match(html, "biallelic SNPs", fixed = TRUE)
})

test_that("HTML reports omit the Pipeline notices section entirely when there are no WARNING/INFO messages", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("no-pipeline-notices-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  populated <- minimal_standard_report_result()
  populated$messages <- data.table::data.table(
    timestamp = Sys.time(), level = "SUCCESS",
    stage = "VCF preparation", message = "completed"
  )
  saveRDS(populated, results)

  rendered <- render_report(results, file.path(root, "report"), formats = "html")
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_no_match(html, "Pipeline notices", fixed = TRUE)
})

test_that("standard report rejects unsupported formats", {
  expect_error(
    render_report(tempfile(), tempfile(), formats = "docx"),
    "formats"
  )
})
