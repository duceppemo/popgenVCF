# Shared phylogenetic bootstrap support-value machinery for both NJ trees
# this package builds (build_nj_tree()'s individual-level IBS tree in
# R/ordination.R, and build_population_nj_tree()'s population-level Nei's-D
# tree in R/population_tree.R). Both resample loci (the standard Felsenstein
# bootstrap unit for population-genetic distance trees), rebuild a tree per
# replicate, and need the same bipartition-support tally against the
# reference tree -- implemented once here via ape::prop.part()/prop.clades(),
# ape's own standard, already-tested bipartition-matching algorithm (the same
# one ape::boot.phylo() itself calls internally), rather than each tree type
# reimplementing it.

# `replicate_trees` must be a list of `ape::phylo` objects sharing the exact
# same tip labels as `reference_tree` (order need not match; prop.clades()
# reconciles it). Returns an integer percent-support vector, one entry per
# internal node, aligned to `reference_tree$edge`/`node.label` order, or NULL
# if fewer than two replicates succeeded.
bootstrap_tree_support <- function(reference_tree, replicate_trees) {
  replicate_trees <- Filter(function(x) inherits(x, "phylo"), replicate_trees)
  if (length(replicate_trees) < 2L) return(NULL)
  class(replicate_trees) <- "multiPhylo"
  bipartitions <- ape::prop.part(replicate_trees)
  counts <- ape::prop.clades(reference_tree, part = bipartitions, rooted = FALSE)
  percent <- round(100 * counts / length(replicate_trees))
  percent[is.na(percent)] <- 0L
  as.integer(percent)
}

# Deterministic, reproducible per-replicate seeds derived from one pipeline
# seed -- avoids relying on parallel::mclapply()'s own worker-level RNG
# state (order/scheduling-dependent, and explicitly disabled via
# mc.set.seed = FALSE below to keep it that way), matching this codebase's
# existing DAPC parallel-task convention (execute_dapc_k_tasks(), R/dapc.R).
tree_bootstrap_replicate_seeds <- function(seed, replicates) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = .GlobalEnv) else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  sample.int(.Machine$integer.max, replicates)
}

run_tree_bootstrap_replicates <- function(replicates, workers, build_one) {
  if (workers <= 1L) return(lapply(seq_len(replicates), build_one))
  results <- parallel::mclapply(
    seq_len(replicates), build_one,
    mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE
  )
  Filter(function(x) !inherits(x, "try-error"), results)
}

# Renders an NJ tree (individual IBS or population Nei's-D) as a phylogram,
# with bootstrap support percentages at internal nodes when the tree carries
# node.label (set by build_nj_tree()/build_population_tree() when
# analyses.tree_bootstrap.enabled). Uses ape's own base-graphics tree
# plotting (plot.phylo()/nodelabels()) rather than this codebase's usual
# ggplot2 figures: tree topology layout is exactly what ape already solves,
# and there is no ggplot2-native equivalent among this package's existing
# dependencies. `metadata` colours tips by population when supplied (the
# individual IBS tree); pass NULL for the population tree, whose tips already
# are population names.
plot_nj_tree <- function(tree, metadata, cfg, dirs, stem, title) {
  if (is.null(tree) || !inherits(tree, "phylo") || length(tree$tip.label) < 3L) {
    return(invisible(NULL))
  }
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  n_tips <- length(tree$tip.label)
  n_replicates <- attr(tree, "bootstrap_replicates") %||% 0L
  has_support <- n_replicates > 0L && !is.null(tree$node.label) && length(tree$node.label) == tree$Nnode

  tip_colour <- "#1A1A1A"
  if (!is.null(metadata) && "population" %in% names(metadata) && "sample" %in% names(metadata)) {
    tip_population <- metadata$population[match(tree$tip.label, public_sample_ids(metadata, metadata$sample))]
    if (!anyNA(tip_population)) {
      palette <- population_palette(tip_population, style)
      tip_colour <- unname(palette[tip_population])
    }
  }

  draw <- function() {
    op <- graphics::par(mar = c(2, 0.5, 2.5, 7), xpd = NA)
    on.exit(graphics::par(op), add = TRUE)
    ape::plot.phylo(
      tree, type = "phylogram", tip.color = tip_colour,
      cex = max(0.35, min(0.8, 9 / n_tips + 0.3)),
      label.offset = max(c(tree$edge.length, 0), na.rm = TRUE) * 0.01,
      no.margin = FALSE
    )
    if (has_support) {
      ape::nodelabels(tree$node.label, frame = "none", adj = c(1.15, -0.3),
                      cex = 0.65, col = "#555555", font = 1)
    }
    graphics::title(main = title, cex.main = 1.05, font.main = 1)
    caption <- if (has_support) {
      sprintf("Node labels: bootstrap support (%% of %d locus-resampled replicates)", n_replicates)
    } else {
      "Bootstrap support was not computed for this run"
    }
    graphics::mtext(caption, side = 1, line = 0.8, cex = 0.68, col = "#666666", adj = 0)
  }
  height <- max(4, min(24, n_tips * 0.16 + 1.5))
  save_base_plot(draw, stem, dirs, fmts, 8, height, dpi)
}
