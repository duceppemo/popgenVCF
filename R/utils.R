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

figure_style_name <- function(cfg = NULL) {
  style <- cfg$output$figure_style %||% "accessibility-first"
  style <- tolower(as.character(style)[1L])
  allowed <- c("accessibility-first", "grayscale-safe", "standard-color")
  if (is.na(style) || !style %in% allowed) {
    stopf(
      "output.figure_style must be one of: %s",
      paste(allowed, collapse = ", ")
    )
  }
  style
}

figure_base_size <- function(cfg = NULL) {
  size <- suppressWarnings(as.numeric(
    cfg$output$base_font_size %||% 11
  )[1L])
  if (!is.finite(size) || size < 8) {
    stop("output.base_font_size must be a finite number >= 8", call. = FALSE)
  }
  size
}

figure_style_profile <- function(style = "accessibility-first") {
  if (inherits(style, "PopgenVCFPublicationFigureStyleProfile")) {
    validate_publication_figure_style_profile(style)
    return(style)
  }
  publication_figure_style_profile(as.character(style)[1L])
}

expand_figure_palette <- function(profile, n, aesthetic = c("colours", "fills")) {
  profile <- figure_style_profile(profile)
  aesthetic <- match.arg(aesthetic)
  n <- as.integer(n)[1L]
  if (is.na(n) || n < 0L) stop("palette size must be non-negative", call. = FALSE)
  if (!n) return(character())
  palette <- profile[[aesthetic]]
  if (n <= length(palette)) return(palette[seq_len(n)])

  if (isTRUE(profile$grayscale_safe)) {
    if (n > 9L) {
      log_msg(
        "A grayscale palette cannot guarantee strong luminance separation for ",
        n, " groups; retain direct labels and verify the exported figure.",
        level = "WARNING"
      )
    }
    return(grDevices::gray.colors(n, start = 0.10, end = 0.80))
  }
  grDevices::hcl.colors(n, palette = "Dark 3")
}

population_palette <- function(populations, style = "accessibility-first") {
  lev <- sort(unique(as.character(populations)))
  cols <- expand_figure_palette(
    figure_style_profile(style), length(lev), "colours"
  )
  stats::setNames(cols, lev)
}

population_shapes <- function(populations, style = "accessibility-first") {
  lev <- sort(unique(as.character(populations)))
  profile <- figure_style_profile(style)
  shapes <- rep(profile$shapes, length.out = length(lev))
  stats::setNames(shapes, lev)
}

cluster_palette <- function(clusters, style = "accessibility-first") {
  labels <- if (length(clusters) == 1L && is.numeric(clusters)) {
    paste0("cluster_", seq_len(as.integer(clusters)))
  } else {
    unique(as.character(clusters))
  }
  stats::setNames(
    expand_figure_palette(figure_style_profile(style), length(labels), "fills"),
    labels
  )
}

diversity_metric_palette <- function(style = "accessibility-first") {
  stats::setNames(
    expand_figure_palette(figure_style_profile(style), 2L, "fills"),
    c("observed_heterozygosity", "expected_heterozygosity")
  )
}

theme_publication <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = base_family, colour = "#1A1A1A", lineheight = 0.95
      ),
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 2.5, hjust = 0,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "#404040", size = base_size,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(
        colour = "#595959", size = base_size - 1, hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.margin = ggplot2::margin(10, 12, 10, 10),
      axis.title = ggplot2::element_text(
        face = "bold", size = base_size,
        margin = ggplot2::margin(4, 4, 4, 4)
      ),
      axis.text = ggplot2::element_text(
        colour = "#1A1A1A", size = base_size - 1
      ),
      axis.line = ggplot2::element_line(colour = "#333333", linewidth = 0.45),
      axis.ticks = ggplot2::element_line(colour = "#333333", linewidth = 0.4),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(colour = "#1A1A1A"),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2", colour = "#B3B3B3", linewidth = 0.45
      ),
      strip.text = ggplot2::element_text(face = "bold", colour = "#1A1A1A"),
      panel.spacing = grid::unit(9, "pt")
    )
}

save_plot <- function(p, stem, dirs, formats = c("pdf", "png"), width = 8, height = 6, dpi = 600) {
  for (fmt in formats) {
    path <- file.path(dirs$figures, paste0(stem, ".", fmt))
    if (fmt == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
      log_msg("Skipping SVG output because svglite is unavailable", level = "WARNING")
      next
    }
    device <- switch(
      fmt,
      pdf = if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf",
      png = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png",
      svg = svglite::svglite,
      fmt
    )
    args <- list(
      filename = path,
      plot = p,
      width = width,
      height = height,
      units = "in",
      device = device,
      bg = "white",
      limitsize = FALSE
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
