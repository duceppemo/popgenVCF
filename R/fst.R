fst_pair <- function(gds, snp_ids, metadata, p1, p2) {
  s1 <- metadata[population == p1, sample]; s2 <- metadata[population == p2, sample]
  if (length(s1) < 2L || length(s2) < 2L) return(NA_real_)
  z <- SNPRelate::snpgdsFst(gds, sample.id = c(s1, s2), snp.id = snp_ids,
                            population = factor(c(rep(p1, length(s1)), rep(p2, length(s2)))),
                            method = "W&C84", autosome.only = FALSE, verbose = FALSE)
  as.numeric(z$Fst)
}

run_fst <- function(gds, snp_ids, metadata) {
  valid <- metadata[, .N, by = population][N >= 2, population]
  m <- metadata[population %in% valid]
  global <- if (data.table::uniqueN(m$population) >= 2L) {
    as.numeric(SNPRelate::snpgdsFst(gds, sample.id = m$sample, snp.id = snp_ids,
                                    population = factor(m$population), method = "W&C84",
                                    autosome.only = FALSE, verbose = FALSE)$Fst)
  } else NA_real_
  pops <- sort(unique(metadata$population)); pairs <- if (length(pops) >= 2L) utils::combn(pops, 2, simplify = FALSE) else list()
  long <- data.table::rbindlist(lapply(pairs, function(pp) data.table::data.table(
    population_1 = pp[1], population_2 = pp[2],
    n_1 = metadata[population == pp[1], .N], n_2 = metadata[population == pp[2], .N],
    fst = fst_pair(gds, snp_ids, metadata, pp[1], pp[2]))), fill = TRUE)
  mat <- matrix(0, length(pops), length(pops), dimnames = list(pops, pops))
  if (nrow(long)) for (i in seq_len(nrow(long))) mat[long$population_1[i], long$population_2[i]] <- mat[long$population_2[i], long$population_1[i]] <- long$fst[i]
  list(global = global, long = long, matrix = mat)
}

bootstrap_fst <- function(gds, snp_ids, ids, metadata, replicates, seed) {
  if (replicates <= 0L) return(data.table::data.table())
  chr <- ids$chromosome[match(snp_ids, ids$snp)]
  blocks <- split(snp_ids, chr)
  blocks <- blocks[lengths(blocks) >= 2L]
  if (length(blocks) < 2L) return(data.table::data.table())
  set.seed(seed)
  pairs <- utils::combn(sort(unique(metadata$population)), 2, simplify = FALSE)
  out <- lapply(pairs, function(pp) {
    by_chr <- data.table::rbindlist(lapply(names(blocks), function(ch) {
      data.table::data.table(chromosome = ch, n_snps = length(blocks[[ch]]),
                             fst = fst_pair(gds, blocks[[ch]], metadata, pp[1], pp[2]))
    }))
    by_chr <- by_chr[is.finite(fst)]
    if (nrow(by_chr) < 2L) return(data.table::data.table(population_1 = pp[1], population_2 = pp[2],
      estimate = NA_real_, lower = NA_real_, upper = NA_real_, replicates = replicates))
    estimate <- stats::weighted.mean(by_chr$fst, by_chr$n_snps)
    vals <- replicate(replicates, {
      idx <- sample(seq_len(nrow(by_chr)), nrow(by_chr), replace = TRUE)
      stats::weighted.mean(by_chr$fst[idx], by_chr$n_snps[idx])
    })
    data.table::data.table(population_1 = pp[1], population_2 = pp[2], estimate = estimate,
                           lower = stats::quantile(vals, .025, na.rm = TRUE),
                           upper = stats::quantile(vals, .975, na.rm = TRUE), replicates = replicates)
  })
  data.table::rbindlist(out, fill = TRUE)
}

plot_fst <- function(fst, cfg, dirs) {
  if (!nrow(fst$long)) return(invisible(NULL))
  lower <- fst$matrix; lower[upper.tri(lower, diag = TRUE)] <- NA_real_
  long <- data.table::as.data.table(as.table(lower)); data.table::setnames(long, c("p1", "p2", "fst")); long <- long[is.finite(fst)]
  lim <- max(abs(long$fst), na.rm = TRUE); if (!is.finite(lim) || lim == 0) lim <- 0.01
  p <- ggplot2::ggplot(long, ggplot2::aes(p2, p1, fill = fst)) + ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", fst)), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#3B4CC0", mid = "white", high = "#B40426", midpoint = 0, limits = c(-lim, lim)) +
    ggplot2::coord_equal() + ggplot2::labs(title = "Pairwise population differentiation", subtitle = "Weir-Cockerham FST", x = NULL, y = NULL) +
    theme_publication() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  save_plot(p, "10_pairwise_FST", dirs, cfg$output$figure_formats, 8, 7, cfg$output$dpi)
}
