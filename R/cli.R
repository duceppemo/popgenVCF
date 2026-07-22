cli_usage <- function(status = 0L) {
  cat(
    "popgenVCF population genomics toolkit\n\n",
    "Usage:\n",
    "  Rscript popgenVCF.R --config analysis.yml\n",
    "  Rscript popgenVCF.R --write-config analysis.yml\n\n",
    "Optional overrides:\n",
    "  --vcf FILE\n",
    "  --metadata FILE\n",
    "  --outdir DIR\n",
    "  --threads N\n",
    "  --seed N\n",
    "  --maf FLOAT\n",
    "  --max-sample-missing FLOAT\n",
    "  --force-gds\n",
    "  --no-report\n",
    "  --version\n",
    "  --help\n",
    sep = ""
  )
  quit(save = "no", status = status)
}

parse_cli <- function(args) {
  out <- list(
    config = NULL,
    write_config = NULL,
    force_gds = FALSE,
    no_report = FALSE,
    version = FALSE
  )
  value_opts <- c(
    "--config", "--write-config", "--vcf", "--metadata", "--outdir",
    "--threads", "--seed", "--maf", "--max-sample-missing"
  )
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (identical(a, "--help")) cli_usage(0L)
    if (identical(a, "--version")) {
      out$version <- TRUE
      i <- i + 1L
      next
    }
    if (a %in% c("--force-gds", "--no-report")) {
      out[[gsub("-", "_", sub("^--", "", a))]] <- TRUE
      i <- i + 1L
      next
    }
    if (!a %in% value_opts || i == length(args)) {
      stopf("Unknown or incomplete argument: %s", a)
    }
    out[[gsub("-", "_", sub("^--", "", a))]] <- args[[i + 1L]]
    i <- i + 2L
  }
  out
}

write_default_config <- function(path) {
  if (file.exists(path)) stopf("Refusing to overwrite existing file: %s", path)
  ensure_dir(dirname(normalizePath(path, mustWork = FALSE)))
  yaml::write_yaml(default_config(), path)
  cat(sprintf("Wrote default configuration: %s\n", normalizePath(path)))
  invisible(path)
}

#' Run the popgenVCF command-line interface
#'
#' @param args Character vector of command-line arguments.
#' @return The pipeline result, or `NULL` invisibly for informational commands.
#' @export
cli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  x <- parse_cli(args)
  if (isTRUE(x$version)) {
    cat(sprintf("popgenVCF %s\n", as.character(utils::packageVersion("popgenVCF"))))
    return(invisible(NULL))
  }
  if (!is.null(x$write_config)) return(write_default_config(x$write_config))
  if (is.null(x$config)) cli_usage(1L)

  cfg <- read_config(x$config)
  if (!is.null(x$vcf)) cfg$input$vcf <- x$vcf
  if (!is.null(x$metadata)) cfg$input$metadata <- x$metadata
  if (!is.null(x$outdir)) cfg$output$directory <- x$outdir
  if (!is.null(x$threads)) cfg$compute$threads <- as.integer(x$threads)
  if (!is.null(x$seed)) cfg$compute$seed <- as.integer(x$seed)
  if (!is.null(x$maf)) cfg$qc$maf <- as.numeric(x$maf)
  if (!is.null(x$max_sample_missing)) {
    cfg$qc$max_sample_missing <- as.numeric(x$max_sample_missing)
  }
  if (x$force_gds) cfg$compute$force_gds <- TRUE
  if (x$no_report) cfg$report$enabled <- FALSE
  run_pipeline(cfg)
}
