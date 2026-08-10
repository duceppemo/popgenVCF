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

# Splits a string into alternating digit/non-digit runs and zero-pads the
# digit runs so a plain lexicographic sort orders embedded numbers
# numerically (K2 before K10, chromosome 2 before chromosome 10) instead of
# as plain strings (order()'s default on character vectors would otherwise
# sort "K10"/"10" before "K2"/"2", since "1" < "2" at the first differing
# character). Non-numeric strings (e.g. "X", "Y", "MT") sort after all
# digit-leading strings, matching standard genomic chromosome ordering.
natural_sort_key <- function(x) {
  vapply(x, function(value) {
    parts <- regmatches(value, gregexpr("[0-9]+|[^0-9]+", value))[[1L]]
    is_digits <- grepl("^[0-9]+$", parts)
    parts[is_digits] <- formatC(as.numeric(parts[is_digits]), width = 12L, format = "d", flag = "0")
    paste(parts, collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

# Natural-order unique values of x, for use as explicit factor levels (e.g.
# before ggplot2::facet_wrap(), which otherwise silently alphabetizes a
# character/default-factor column).
natural_sort_levels <- function(x) {
  u <- unique(x)
  u[order(natural_sort_key(u))]
}

manhattan_layout <- function(chromosome, position) {
  chromosome <- as.character(chromosome)
  position <- as.numeric(position)
  order_chr <- natural_sort_levels(chromosome)
  offset <- stats::setNames(numeric(length(order_chr)), order_chr)
  cum <- 0
  for (chr in order_chr) {
    offset[[chr]] <- cum
    cum <- cum + max(position[chromosome == chr]) + 1
  }
  x <- unname(position + offset[chromosome])
  ticks <- data.frame(
    chromosome = order_chr,
    center = vapply(order_chr, function(chr) {
      mean(range(position[chromosome == chr])) + offset[[chr]]
    }, numeric(1L)),
    stringsAsFactors = FALSE
  )
  list(x = x, ticks = ticks, offset = offset)
}

# Basepair-position axis breaks for a manhattan_layout()'d x-axis. Compact
# Mb-formatted labels (not raw bp digits) keep the axis readable; the total
# tick count is capped and divided across chromosomes so a genome-wide,
# many-chromosome plot doesn't get one tick's worth of density per
# chromosome. The first break of each chromosome is prefixed with its name
# (e.g. "chr22: 20 Mb") so chromosome identity is preserved even when only
# one tick per chromosome fits; later breaks in the same chromosome are
# unprefixed ("20.5 Mb") to avoid repeating it.
manhattan_bp_breaks <- function(chromosome, position, offset, target_total = 14L) {
  chromosome <- as.character(chromosome)
  position <- as.numeric(position)
  chrs <- names(offset)
  n_per_chr <- max(1L, min(6L, round(target_total / max(1L, length(chrs)))))
  rows <- lapply(chrs, function(chr) {
    p <- position[chromosome == chr]
    if (!length(p)) return(NULL)
    span <- diff(range(p))
    breaks <- if (span <= 0) mean(p) else pretty(range(p), n = n_per_chr)
    breaks <- breaks[breaks >= min(p) & breaks <= max(p)]
    if (!length(breaks)) breaks <- mean(range(p))
    accuracy <- if (span < 2e6) 0.1 else 1
    label <- scales::label_number(scale = 1e-6, suffix = " Mb", accuracy = accuracy)(breaks)
    label[[1L]] <- paste0("chr", chr, ": ", label[[1L]])
    data.frame(x = breaks + offset[[chr]], label = label, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
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

# hierfstat's own genotype convention: each allele occupies one digit, so a
# 0/1/2 dosage becomes 11 (homozygous reference), 12 (heterozygous), or 22
# (homozygous alternate); missing dosage stays NA. Shared by the FST
# scientific-validation reference implementation and allelic richness.
hierfstat_encode_genotype <- function(genotype) {
  encoded <- as.data.frame(genotype, check.names = FALSE)
  encoded[] <- lapply(encoded, function(x) {
    out <- rep(NA_integer_, length(x))
    out[x == 0] <- 11L
    out[x == 1] <- 12L
    out[x == 2] <- 22L
    out
  })
  encoded
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
