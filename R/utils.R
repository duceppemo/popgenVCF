`%||%` <- function(x, y) if (is.null(x)) y else x

.pg_env <- new.env(parent = emptyenv())
.pg_env$log_file <- NULL

log_msg <- function(..., level = "INFO") {
  line <- sprintf("[%s] [%-7s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level,
                  paste(..., collapse = ""))
  cat(line, "\n")
  if (!is.null(.pg_env$log_file)) cat(line, "\n", file = .pg_env$log_file, append = TRUE)
  invisible(line)
}

stopf <- function(...) stop(sprintf(...), call. = FALSE)

# Call a function using only arguments supported by the installed version.
# This is used for selected Bioconductor APIs whose formal arguments differ
# across supported releases. Required arguments should still be validated by
# the called function; only optional unsupported arguments are discarded.
call_supported <- function(fun, args, function_name = deparse(substitute(fun))) {
  if (!is.function(fun)) stop("fun must be a function", call. = FALSE)
  formal_names <- names(formals(fun))
  if (is.null(formal_names) || "..." %in% formal_names) {
    return(do.call(fun, args))
  }
  supported <- names(args) %in% formal_names | names(args) == ""
  dropped <- names(args)[!supported]
  dropped <- dropped[nzchar(dropped)]
  if (length(dropped)) {
    log_msg(
      sprintf(
        "%s does not support optional argument(s) in this installed version: %s",
        function_name, paste(dropped, collapse = ", ")
      ),
      level = "DEBUG"
    )
  }
  do.call(fun, args[supported])
}

run_stage <- function(name, expr, timings = NULL) {
  log_msg("Starting ", name)
  t0 <- proc.time()[["elapsed"]]
  ans <- tryCatch(force(expr), error = function(e) stopf("%s failed: %s", name, conditionMessage(e)))
  elapsed <- proc.time()[["elapsed"]] - t0
  log_msg(sprintf("Completed %s in %.2f seconds", name, elapsed), level = "SUCCESS")
  if (!is.null(timings)) timings[[name]] <- elapsed
  ans
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stopf("Could not create directory: %s", path)
  normalizePath(path, mustWork = TRUE)
}

write_tsv <- function(x, path) {
  data.table::fwrite(x, path, sep = "\t", quote = FALSE, na = "NA")
  invisible(path)
}

write_matrix_tsv <- function(x, path, row_name = "id") {
  dt <- data.table::as.data.table(x, keep.rownames = row_name)
  write_tsv(dt, path)
}

hash_file <- function(path) digest::digest(file = path, algo = "sha256")

parse_int_range <- function(x) {
  if (is.null(x) || !length(x)) return(integer())
  if (is.numeric(x)) return(sort(unique(as.integer(x))))
  x <- as.character(x)
  if (length(x) > 1L) return(sort(unique(as.integer(x))))
  if (grepl(":", x, fixed = TRUE)) {
    z <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    if (length(z) != 2L || anyNA(z)) stopf("Invalid integer range: %s", x)
    return(seq.int(z[1], z[2]))
  }
  z <- as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
  if (anyNA(z)) stopf("Invalid integer list: %s", x)
  sort(unique(z))
}

population_palette <- function(populations) {
  lev <- sort(unique(as.character(populations)))
  if (length(lev) <= 8L) {
    cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
    cols <- cols[seq_along(lev)]
  } else cols <- viridisLite::turbo(length(lev), begin = 0.05, end = 0.95)
  stats::setNames(cols, lev)
}

theme_publication <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(colour = "black"),
      legend.title = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "grey40"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

save_plot <- function(p, stem, dirs, formats = c("pdf", "png"), width = 8, height = 6, dpi = 600) {
  for (fmt in formats) {
    path <- file.path(dirs$figures, paste0(stem, ".", fmt))
    if (fmt == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
      log_msg("Skipping SVG output because svglite is unavailable", level = "WARNING")
      next
    }
    args <- list(
      filename = path,
      plot = p,
      width = width,
      height = height,
      device = if (fmt == "svg") svglite::svglite else fmt
    )
    if (identical(fmt, "png")) args$dpi <- dpi
    suppressMessages(do.call(ggplot2::ggsave, args))
  }
  invisible(TRUE)
}

popgenvcf_version <- function() {
  installed <- tryCatch(as.character(utils::packageVersion("popgenVCF")), error = function(e) NA_character_)
  if (!is.na(installed)) return(installed)
  description <- file.path(getwd(), "DESCRIPTION")
  if (file.exists(description)) {
    dcf <- tryCatch(read.dcf(description), error = function(e) NULL)
    if (!is.null(dcf) && "Version" %in% colnames(dcf)) return(unname(dcf[1, "Version"]))
  }
  "development"
}
