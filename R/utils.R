`%||%` <- function(x, y) if (is.null(x)) y else x

.pg_env <- new.env(parent = emptyenv())
.pg_env$log_file <- NULL

log_msg <- function(..., level = "INFO") {
  line <- sprintf("[%s] [%-7s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level,
                  paste(..., collapse = ""))
  cat(line, "\n")
  # A log line is diagnostic, not load-bearing: an unwritable log destination
  # (a deleted output directory, a full disk, a stale path left over from an
  # unrelated earlier run in the same session) must never abort real
  # analysis work just because it couldn't also be recorded to file.
  if (!is.null(.pg_env$log_file)) {
    tryCatch(
      cat(line, "\n", file = .pg_env$log_file, append = TRUE),
      error = function(e) NULL
    )
  }
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

# Bounds a fork-based worker count by the number of independent tasks, the
# configured thread budget, and platform fork support (parallel::mcparallel()/
# mclapply() are Unix-only). Shared by every module that forks independent
# per-task work -- originally DAPC-specific (per-K model fitting), generalized
# here once diversity's per-population statistics needed the identical
# bounding logic, to avoid a second copy drifting from the first.
fork_worker_count <- function(n_tasks, threads, fork_available = .Platform$OS.type != "windows") {
  threads <- suppressWarnings(as.integer(threads)[1L])
  n_tasks <- suppressWarnings(as.integer(n_tasks)[1L])
  if (is.na(threads) || threads < 1L || is.na(n_tasks) || n_tasks < 1L || !isTRUE(fork_available)) {
    return(1L)
  }
  max(1L, min(threads, n_tasks))
}

# Validates parallel::mclapply() output against the task labels it was
# computed for (same length/order as `results`), distinguishing the two ways
# a forked task can fail to return a real result: an uncaught R-level
# condition, which mclapply() reports as a classed "try-error" result; and a
# worker process killed outright by a signal (OOM, segfault -- a real,
# confirmed cause on large real cohorts), which is NOT a "try-error" -- that
# slot is simply NULL. A caller that only checks for "try-error" and then
# unlist()s/rbindlist()s the result silently drops the killed task's slot
# instead of erroring, which either understates the output (a population's
# rows just vanish) or, worse, misaligns a positionally-zipped result against
# its labels (e.g. Fst values shifted onto the wrong population pairs) --
# this exact bug was found in run_fst() during a pre-release audit and this
# helper exists so it cannot recur silently at any other fork site.
check_mclapply_results <- function(results, labels, task_description) {
  failed <- vapply(results, inherits, logical(1L), what = "try-error")
  if (any(failed)) {
    condition <- attr(results[[which(failed)[1L]]], "condition")
    cond_message <- if (!is.null(condition)) conditionMessage(condition) else NULL
    stopf(
      "Parallel %s failed for %s%s", task_description,
      paste(labels[failed], collapse = ", "),
      if (is.null(cond_message)) "" else paste0(": ", cond_message)
    )
  }
  missing <- vapply(results, is.null, logical(1L))
  if (any(missing)) {
    stopf(
      "Parallel %s terminated abnormally (worker process killed, likely out-of-memory or a crash) for %s rather than returning a result or a normal error",
      task_description, paste(labels[missing], collapse = ", ")
    )
  }
  invisible(results)
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

# ggplot2 never wraps a plot.subtitle -- a long one (e.g. the DAPC
# reproducibility annotation, which can carry both an RMSE and a minimum
# cluster correlation clause) just overflows the plot width and gets
# silently clipped by the device canvas, confirmed directly on a real
# production report ("...minimum cluster correlation = 1 (t" cut off
# mid-word). strwrap() is applied per pre-existing line (not to the whole
# string at once) so an already-deliberate break -- the unstable-annotation
# text's own "\n" before "Avoid interpreting these assignments." -- is
# preserved rather than being merged back into one paragraph.
wrap_plot_subtitle <- function(text, width = 90L) {
  if (is.null(text) || !length(text) || is.na(text) || !nzchar(text)) return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  paste(unlist(lapply(lines, strwrap, width = width)), collapse = "\n")
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
  # method = "radix" keeps this locale-independent even for the non-digit
  # runs natural_sort_key() leaves untouched (e.g. "X"/"Y"/"MT" or any
  # letter-only chromosome name) -- the digit-padding alone only protects
  # purely-numeric comparisons, not the underlying character ordering.
  u[order(natural_sort_key(u), method = "radix")]
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
    # Each chromosome/contig's own laid-out span, in the same x-units as
    # `x` -- manhattan_chromosome_row() needs this to know how much physical
    # page width is actually available for that chromosome's own name label,
    # which for a many-small-contig assembly can differ hugely between
    # entries and isn't recoverable from chromosome count alone.
    width = vapply(order_chr, function(chr) {
      diff(range(position[chromosome == chr]))
    }, numeric(1L)),
    stringsAsFactors = FALSE
  )
  list(x = x, ticks = ticks, offset = offset)
}

# Real, font-metric-based label width (not a guessed characters-per-inch
# constant): renders into a discarded null device so graphics::strwidth()
# reflects the actual font used, at the actual point size, including bold
# weight -- exact enough to compare against the physical space a label
# will actually have, rather than a rule of thumb that could be wrong in
# either direction depending on the label alphabet (accession-style contig
# names skew towards wide digits/uppercase, unlike ordinary prose).
manhattan_label_width_in <- function(labels, font_pt, family = "sans") {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::strwidth(labels, units = "inches", cex = font_pt / 12, font = 2, family = family)
}

# Basepair-position axis breaks for a manhattan_layout()'d x-axis. Compact
# Mb-formatted labels (not raw bp digits) keep the axis readable; the total
# tick count is capped and divided across chromosomes so a genome-wide,
# many-chromosome plot doesn't get one tick's worth of density per
# chromosome. Unlike an earlier version of this helper, labels never carry a
# "chrNN: " prefix -- chromosome identity is now shown on its own row below
# the Mb ticks (manhattan_chromosome_row()), so the two would otherwise be
# redundant. Ticks are placed *inset* from each chromosome's own start/end
# (evenly within the open interval, never touching either boundary) so that
# adjacent chromosomes' edge ticks cannot land on top of each other when
# many chromosomes are packed tightly. When the shared per-chromosome tick
# budget rounds down to <= 1, or a chromosome has only a single distinct
# position, no Mb ticks are emitted for it at all: a single tick would only
# duplicate the chromosome-name row already centered at the same spot, and
# real genome-wide (many-chromosome) scans are exactly the case where that
# budget collapses -- matching this helper's original, pre-Mb-tick
# behavior of showing chromosome identity alone for that case, found to
# look cleanest empirically rather than assumed.
manhattan_bp_breaks <- function(chromosome, position, offset, target_total = 14L, max_per_chr = 6L) {
  chromosome <- as.character(chromosome)
  position <- as.numeric(position)
  chrs <- names(offset)
  n_per_chr <- max(1L, min(max_per_chr, round(target_total / max(1L, length(chrs)))))
  empty <- data.frame(x = numeric(), label = character(), stringsAsFactors = FALSE)
  if (n_per_chr <= 1L) return(empty)
  rows <- lapply(chrs, function(chr) {
    p <- position[chromosome == chr]
    if (!length(p)) return(NULL)
    span <- diff(range(p))
    if (span <= 0) return(NULL)
    breaks <- min(p) + span * seq_len(n_per_chr) / (n_per_chr + 1)
    accuracy <- if (span < 2e6) 0.1 else 1
    label <- scales::label_number(scale = 1e-6, suffix = " Mb", accuracy = accuracy)(breaks)
    data.frame(x = breaks + offset[[chr]], label = label, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  if (is.null(result)) empty else result
}

# Adds the chromosome-name row below a Manhattan-style figure's Mb-tick
# x-axis, centered on each chromosome's own span. Implemented as a text
# layer positioned below the plot's own data range plus
# coord_cartesian(clip = "off") to draw it in the margin, rather than a
# second, separately-built panel: this keeps everything in one coordinate
# system, so horizontal alignment with the Mb ticks above is exact by
# construction (a stacked second panel would need its own independently
# matched x-scale/expansion, a real source of subtle misalignment).
#
# For faceted plots (one panel per PC/discriminant axis, `scales =
# "free_y"`, e.g. the PCA/DAPC SNP-loading figures), pass `facet_var`/
# `facet_last_level` so the row is drawn only once, in the last (bottom)
# facet, positioned at that facet's own y-range -- an ordinary annotation
# layer would otherwise repeat the same y-position label in every facet,
# since annotate()/geom_text() with no facet-matching data replicates
# across all panels by default.
#
# `facet_levels` matters, not just `facet_last_level`: adding a layer whose
# facet column is a plain character (even a single value) alongside the
# main layer's properly-ordered factor makes ggplot2 fall back to
# alphabetical panel order when it combines both layers' data to resolve
# the facet variable -- confirmed directly (PC10 rendered second, right
# after PC1, instead of last) before this was fixed by constructing the
# label layer's facet column as a real factor sharing the exact same
# levels as the main data, not a bare string.
#
# A model genome with a handful of chromosomes leaves plenty of horizontal
# room per label at angle = 0 (this function's original, and still default,
# behavior). A non-model reference assembled into dozens or hundreds of
# short contigs with long accession-style names (e.g. "JAEVLN010000001.1")
# does not: horizontal labels centered on narrow contigs collide into an
# unreadable smear regardless of font size, since the fix has to be
# geometric (each contig's own physical width vs. its own label's real
# width), not a fixed threshold on chromosome count alone. When
# `plot_width_in` (the figure's actual saved width) is supplied and any
# label would not fit horizontally in its own contig's physical share of
# that width, every label switches to vertical (angle = 90) text instead --
# whose footprint is one line's height, not the full label string length,
# which comfortably fits far more labels side by side. If even that is not
# enough room for every contig (real assemblies can have hundreds of tiny
# scaffolds), labels are thinned by minimum physical spacing, keeping the
# first of any two candidates that would still collide -- the same
# "simplify once the per-item budget collapses" idiom already used by
# manhattan_bp_breaks() for the Mb ticks above this row, rather than
# rendering an unreadable jumble.
#
# Vertical mode also switches to a materially smaller, fixed font
# (independent of the figure's own base_size): once forced vertical, exact
# per-character legibility is secondary -- real contig/chromosome
# identities belong in the accompanying TSVs, not this margin row -- and a
# smaller font keeps the row's own physical footprint small regardless of
# how many labels are packed in. An earlier version of this function instead
# grew the *data-space* anchor offset (`pad`) to guarantee clearance on
# narrow-y-range panels; that avoided the labels overlapping the panel, but
# at a real cost found on a real regenerated production figure: `pad` is a
# value in the plotted data's own units, so growing it to fit a whole
# rotated label's length pulled that much of the y-scale's own range down
# with it, visibly crushing the actual plotted data into a sliver at the
# top of the panel. The correct place to reserve room for content outside
# the panel is `plot.margin` (a physical-device quantity, entirely outside
# the data coordinate system) combined with `coord_cartesian(clip = "off")`
# to let the label extend down into that margin -- confirmed directly
# against the exact real ticks/y-range that exposed both the original
# overlap bug and this squeeze regression: a small, fixed pad (the same
# 14%-of-range used in horizontal mode) plus a margin sized from the
# smaller font's real label width keeps the labels fully clear of the
# panel with the plotted data using its full, correct vertical extent.
manhattan_chromosome_row <- function(p, ticks, y_range, base_size = 11,
                                     facet_var = NULL, facet_last_level = NULL,
                                     facet_levels = NULL, plot_width_in = NULL) {
  pad <- diff(y_range) * 0.14
  if (!is.finite(pad) || pad <= 0) pad <- max(abs(y_range), 1, na.rm = TRUE) * 0.14

  label_pt <- base_size * 0.32 * 2.845276 # geom_text `size` (mm) -> points
  vertical <- FALSE
  keep <- rep(TRUE, nrow(ticks))
  margin_bottom_pt <- 5.5 + base_size * 2
  extra_height_in <- 0

  # The panel that actually plots `total_width` worth of data is narrower
  # than the figure's full saved width: the y-axis number/title column eats
  # into it, and ggplot2's default continuous-scale expansion (5% padding
  # on each side) stretches the data range across a still-smaller fraction
  # of even that. Confirmed against a real production figure where a pair
  # of contigs measured as fitting by ~20% margin under a naive
  # plot_width_in-based estimate still rendered fully overlapping -- this
  # constant is a deliberately conservative stand-in for that real,
  # version/theme-dependent geometry rather than an attempt to model it
  # exactly, so it always errs toward more rotation/thinning, never less.
  usable_width_in <- (plot_width_in %||% NA_real_) * 0.7

  total_width <- sum(ticks$width)
  if (!is.null(plot_width_in) && nrow(ticks) > 0L && is.finite(total_width) && total_width > 0) {
    available_in <- (ticks$width / total_width) * usable_width_in
    label_width_in <- manhattan_label_width_in(ticks$chromosome, label_pt)
    if (any(label_width_in > available_in)) {
      vertical <- TRUE
      label_pt <- 7 # fixed, independent of base_size -- see comment above
      label_width_in <- manhattan_label_width_in(ticks$chromosome, label_pt)
      # A rotated label anchored just below a *faceted* panel and left to
      # bleed into plot.margin via coord_cartesian(clip = "off") needs
      # meaningfully more margin than its own measured length to render in
      # full -- confirmed empirically (not just theoretically) on the exact
      # real faceted PCA-loadings figure this row is drawn on: a margin
      # sized to exactly max(label_width_in) still truncated the label
      # partway through, and bisecting real rendered output showed the
      # true requirement is well over 1x, closer to 1.5x, that naive
      # figure -- a real, separate contributor turned out to be
      # geom_hline()'s own silent y-scale expansion at affected call sites
      # (fixed at the call sites themselves, see ordination.R's
      # plot_pca_loading_manhattan()), but confirmed on the exact real
      # faceted PCA-loadings figure to not fully account for the shortfall
      # on its own, so this factor stays deliberately conservative rather
      # than exact, the same "always err toward more clearance" philosophy
      # already used for the width-fit and thinning checks above.
      # This clearance must come out of the canvas, not the panel: a fixed
      # point margin was found (on the real pcadapt figure, a compact
      # single-panel 10x4.5in save) to consume over half of a short
      # canvas's total height, squeezing the panel down to a sliver and
      # visually crushing the real plotted data -- exactly the squeeze
      # regression this function exists to avoid. save_plot() reads this
      # attribute and grows the saved height by it, so the panel keeps its
      # caller-intended size and only the canvas grows to fit the labels.
      extra_height_in <- max(label_width_in) * 2
      margin_bottom_pt <- margin_bottom_pt + max(label_width_in) * 72 * 2

      line_height_in <- (label_pt * 1.2) / 72
      min_gap_x <- (line_height_in / usable_width_in) * total_width
      last_kept <- -Inf
      for (i in seq_len(nrow(ticks))) {
        if (ticks$center[i] - last_kept < min_gap_x) {
          keep[i] <- FALSE
        } else {
          last_kept <- ticks$center[i]
        }
      }
    }
  }

  label_df <- data.frame(
    x = ticks$center[keep], y = y_range[1] - pad, label = ticks$chromosome[keep]
  )
  if (!is.null(facet_var)) {
    label_df[[facet_var]] <- factor(facet_last_level, levels = facet_levels %||% facet_last_level)
  }

  p <- p +
    ggplot2::geom_text(
      data = label_df, ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE, size = label_pt / 2.845276, fontface = "bold",
      angle = if (vertical) 90 else 0,
      hjust = if (vertical) 1 else 0.5,
      vjust = if (vertical) 0.5 else 1
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      plot.margin = ggplot2::unit(c(5.5, 5.5, margin_bottom_pt, 5.5), "pt")
    )
  if (vertical) {
    # The plot's own "Chromosome position" x-axis title sits directly below
    # the Mb-tick text at a fixed offset unrelated to this function's own
    # data-space annotation -- fine when the chromosome names are one short
    # horizontal line, but a column of much taller vertical names now
    # reaches down into that same space. The contig names are
    # self-explanatory as an x-axis identity on their own, so the
    # now-redundant title is dropped rather than fighting to recompute its
    # position to clear a per-plot-varying label height.
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    # A plot.caption (e.g. pcadapt's genomic-inflation-factor note) sits in
    # its own gtable row placed close to the panel, unrelated to how much
    # this function's own data-space annotation extends into the margin
    # below it -- confirmed directly: it rendered on top of the rotated
    # contig names rather than below them. Pushed down by the same real
    # clearance the labels themselves needed, so it lands below them
    # instead of colliding, without touching plots that have no caption.
    if (!is.null(p$labels$caption)) {
      p <- p + ggplot2::theme(
        plot.caption = ggplot2::element_text(
          # Same conservative x2 factor as margin_bottom_pt above -- the
          # caption sits below the label row, so it needs to clear the
          # label's real rendered footprint, not just its nominal length.
          margin = ggplot2::margin(t = max(label_width_in) * 72 * 2 + 5.5)
        )
      )
    }
  }
  attr(p, "manhattan_extra_height_in") <- extra_height_in
  p
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
  # method = "radix" is required, not cosmetic: character sort() otherwise
  # uses the ambient LC_COLLATE locale, which is not guaranteed consistent
  # across machines -- a real, empirically confirmed bug found in a
  # pre-release audit, where the same mixed-case population names sorted
  # differently under "C" vs "en_US.UTF-8", silently assigning different
  # colours to the same populations depending on which machine ran the
  # analysis. "radix" is documented as platform-independent for character
  # vectors (equivalent to a fixed C-locale byte-order sort), giving this
  # package's figures the same reproducibility guarantee its checkpoint/
  # config fingerprints already have.
  lev <- sort(unique(as.character(populations)), method = "radix")
  cols <- expand_figure_palette(
    figure_style_profile(style), length(lev), "colours"
  )
  stats::setNames(cols, lev)
}

population_shapes <- function(populations, style = "accessibility-first") {
  lev <- sort(unique(as.character(populations)), method = "radix")
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
  # manhattan_chromosome_row() reserves clearance for rotated chromosome-name
  # labels via plot.margin (a fixed point quantity that does not scale with
  # the caller's chosen height) rather than by inflating the data-space pad
  # that would otherwise squeeze the real plotted panel -- see that
  # function's own comments. Growing the canvas here by exactly what it
  # added keeps the panel at the caller's intended size on every call site,
  # including short, single-panel figures (e.g. pcadapt's 10x4.5in save)
  # where that margin would otherwise consume most of the canvas.
  extra_height_in <- attr(p, "manhattan_extra_height_in", exact = TRUE)
  if (!is.null(extra_height_in) && is.finite(extra_height_in) && extra_height_in > 0) {
    height <- height + extra_height_in
  }
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

# Base-graphics counterpart to save_plot(), for figures built with a plotting
# package (ape's tree plotting, here) that draws directly to a graphics
# device rather than returning a ggplot object ggsave() can render. `draw` is
# a zero-argument function that issues the plotting calls; called once per
# requested format inside that format's own device. Mirrors save_plot()'s
# format/dpi handling and optional-dependency device preferences (ragg for
# PNG, Cairo PDF when available) so both figure families look the same.
save_base_plot <- function(draw, stem, dirs, formats = c("pdf", "png"), width = 8, height = 6, dpi = 600) {
  for (fmt in formats) {
    path <- file.path(dirs$figures, paste0(stem, ".", fmt))
    if (fmt == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
      log_msg("Skipping SVG output because svglite is unavailable", level = "WARNING")
      next
    }
    open_device <- switch(
      fmt,
      pdf = function() if (isTRUE(capabilities("cairo"))) {
        grDevices::cairo_pdf(path, width = width, height = height, bg = "white")
      } else {
        grDevices::pdf(path, width = width, height = height, bg = "white")
      },
      png = function() if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_png(path, width = width, height = height, units = "in", res = dpi, background = "white")
      } else {
        grDevices::png(path, width = width, height = height, units = "in", res = dpi, bg = "white")
      },
      svg = function() svglite::svglite(path, width = width, height = height, bg = "white"),
      stop("Unsupported base-graphics figure format: ", fmt, call. = FALSE)
    )
    open_device()
    tryCatch(draw(), finally = grDevices::dev.off())
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
