# Frequency-based self-assignment test (Paetkau et al. 1995; Paetkau et al.
# 2004's minimum-frequency refinement; Rannala and Mountain 1997's leave-one-
# out bias correction): for each sample, scores the leave-one-out
# log-likelihood of its own genotype under every population's allele
# frequencies (assuming Hardy-Weinberg equilibrium within each population),
# then assigns it to whichever population maximizes that likelihood. A
# sample whose assigned population differs from its recorded metadata label
# is a candidate migrant or a metadata error -- the same "does the data
# match the metadata" question kinship/sex_check already ask, applied here
# to population identity.
#
# Uses the LD-pruned marker set (like kinship/PCA/IBS), not the full
# QC-passing set diversity/HWE use: per-locus likelihoods are multiplied
# together assuming independence across loci, which linked markers would
# violate, inflating apparent assignment confidence.
#
# Leave-one-out (Rannala and Mountain 1997): a sample's own genotype is
# excluded from its own recorded population's allele-frequency estimate
# before scoring against that population -- otherwise a sample trivially
# "confirms" its own label by partly defining the frequencies it is being
# compared to. No exclusion is applied for other candidate populations,
# since the sample was never part of their frequency estimate.
#
# A population fixed for the other allele at a locus (frequency exactly 0 or
# 1, e.g. after leave-one-out exclusion) is corrected to the standard
# 1 / (2n) minimum-observable-frequency (Paetkau et al. 2004's convention,
# also GenAlEx's default) rather than a hard zero, which would let one locus
# veto that population outright with a log-likelihood of -Inf. A locus with
# zero calls in a candidate population contributes no information there and
# is excluded from that population's score entirely -- the same locus-
# exclusion convention population_tree.R already uses for Nei's distance.

genotype_log_likelihood <- function(dosage, freq) {
  ifelse(is.na(dosage) | is.na(freq), NA_real_,
        ifelse(dosage == 0, 2 * log1p(-freq),
        ifelse(dosage == 1, log(2) + log(freq) + log1p(-freq),
                             2 * log(freq))))
}

empty_population_assignment <- function(populations = character()) {
  list(
    assignment = data.table::data.table(
      sample = character(), recorded_population = character(),
      assigned_population = character(), mismatch = logical(),
      log_likelihood = numeric(), likelihood_ratio = numeric(),
      posterior_probability = numeric(), n_loci_used = integer()
    ),
    log_likelihood = matrix(numeric(0), 0L, 0L),
    populations = populations
  )
}

run_population_assignment <- function(genotype, sample_table, locus_table, snp_ids) {
  populations <- sort(unique(locus_table$population))
  if (length(populations) < 2L || !length(snp_ids) || !nrow(sample_table)) {
    return(empty_population_assignment(populations))
  }

  n_pop <- length(populations); n_snp <- length(snp_ids)
  n_called_mat <- matrix(NA_real_, n_pop, n_snp, dimnames = list(populations, NULL))
  alt_count_mat <- matrix(NA_real_, n_pop, n_snp, dimnames = list(populations, NULL))
  for (i in seq_along(populations)) {
    sub <- locus_table[population == populations[i]]
    idx <- match(snp_ids, sub$snp_id)
    n_called_mat[i, ] <- sub$n_called[idx]
    alt_count_mat[i, ] <- sub$alternate_allele_count[idx]
  }

  own_pop_idx <- match(sample_table$population, populations)
  n_sample <- nrow(genotype)
  log_lik <- matrix(NA_real_, n_sample, n_pop, dimnames = list(sample_table$sample, populations))
  n_used <- matrix(0L, n_sample, n_pop, dimnames = list(sample_table$sample, populations))

  for (p in seq_len(n_pop)) {
    called_base <- n_called_mat[p, ]; alt_base <- alt_count_mat[p, ]
    for (i in seq_len(n_sample)) {
      g <- genotype[i, ]
      called <- called_base; alt <- alt_base
      if (!is.na(own_pop_idx[i]) && own_pop_idx[i] == p) {
        has_call <- !is.na(g)
        called <- called - has_call
        alt <- alt - ifelse(has_call, g, 0)
      }
      gene_copies <- 2 * called
      freq <- ifelse(gene_copies > 0, alt / gene_copies, NA_real_)
      floor_freq <- 1 / pmax(gene_copies, 1)
      freq <- ifelse(is.finite(freq) & freq <= 0, floor_freq, freq)
      freq <- ifelse(is.finite(freq) & freq >= 1, 1 - floor_freq, freq)
      contrib <- genotype_log_likelihood(g, freq)
      used <- sum(!is.na(contrib))
      n_used[i, p] <- used
      log_lik[i, p] <- if (used > 0L) sum(contrib, na.rm = TRUE) else NA_real_
    }
  }

  best_idx <- vapply(seq_len(n_sample), function(i) {
    x <- log_lik[i, ]
    if (all(is.na(x))) NA_integer_ else which.max(x)
  }, integer(1L))
  assigned_population <- ifelse(is.na(best_idx), NA_character_, populations[best_idx])
  recorded_population <- sample_table$population

  best_ll <- vapply(seq_len(n_sample), function(i) {
    x <- log_lik[i, ]
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }, numeric(1L))
  # NA (not Inf) when no second candidate population has any information at
  # all -- honestly reflects missing data, rather than fabricating infinite
  # assignment confidence.
  second_best_ll <- vapply(seq_len(n_sample), function(i) {
    ranked <- sort(log_lik[i, ][!is.na(log_lik[i, ])], decreasing = TRUE)
    if (length(ranked) >= 2L) ranked[2] else NA_real_
  }, numeric(1L))
  best_posterior <- vapply(seq_len(n_sample), function(i) {
    x <- log_lik[i, ]
    if (all(is.na(x))) return(NA_real_)
    m <- max(x, na.rm = TRUE)
    w <- exp(x - m); w[is.na(w)] <- 0
    w[best_idx[i]] / sum(w)
  }, numeric(1L))
  n_loci_used_best <- vapply(seq_len(n_sample), function(i) {
    if (is.na(best_idx[i])) NA_integer_ else n_used[i, best_idx[i]]
  }, integer(1L))

  assignment <- data.table::data.table(
    sample = sample_table$sample,
    recorded_population = recorded_population,
    assigned_population = assigned_population,
    mismatch = !is.na(assigned_population) & !is.na(recorded_population) &
      assigned_population != recorded_population,
    log_likelihood = best_ll,
    likelihood_ratio = exp(best_ll - second_best_ll),
    posterior_probability = best_posterior,
    n_loci_used = n_loci_used_best
  )
  list(assignment = assignment, log_likelihood = log_lik, populations = populations)
}

plot_population_assignment <- function(result, cfg, dirs) {
  a <- result$assignment
  if (!nrow(a) || !any(!is.na(a$assigned_population))) return(invisible(NULL))
  populations <- result$populations
  counts <- a[!is.na(assigned_population), .N, by = .(recorded_population, assigned_population)]
  grid <- data.table::CJ(recorded_population = populations, assigned_population = populations)
  grid[counts, N := i.N, on = c("recorded_population", "assigned_population")]
  grid[is.na(N), N := 0L]

  grayscale <- identical(figure_style_name(cfg), "grayscale-safe")
  fill_scale <- if (grayscale) {
    ggplot2::scale_fill_gradient(low = "#F7F7F7", high = "#252525", name = "Samples")
  } else {
    ggplot2::scale_fill_gradient(low = "#F7F7F7", high = "#B40426", name = "Samples")
  }
  lim <- max(grid$N, 1L)
  grid[, label_colour := ifelse(N >= 0.58 * lim, "white", "#1A1A1A")]
  n_scored <- sum(!is.na(a$mismatch))
  n_match <- sum(!a$mismatch, na.rm = TRUE)

  p <- ggplot2::ggplot(grid, ggplot2::aes(assigned_population, recorded_population, fill = N)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.65) +
    ggplot2::geom_text(
      ggplot2::aes(label = N, colour = label_colour),
      size = 3.2, fontface = "bold", show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    fill_scale +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Population assignment test",
      subtitle = if (n_scored > 0L) {
        sprintf(
          "%s / %s samples (%.1f%%) assign to their recorded population",
          n_match, n_scored, 100 * n_match / n_scored
        )
      } else {
        "No samples could be scored"
      },
      x = "Assigned population", y = "Recorded population"
    ) +
    theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    )
  save_plot(p, "47_population_assignment", dirs, cfg$output$figure_formats, 7.5, 6.5, cfg$output$dpi)
}
