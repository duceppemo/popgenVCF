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
# zero calls in ANY candidate population (after leave-one-out) is excluded
# from EVERY population's score for that sample -- the same locus-exclusion
# convention population_tree.R already uses for Nei's distance, applied
# symmetrically here so every population's log-likelihood is always summed
# over the identical set of loci. An earlier version excluded a locus only
# from the one population that individually lacked calls there, so
# different populations were scored over different numbers of loci for the
# same sample -- since every per-locus term is negative, a population
# simply missing more data at a locus could accumulate fewer penalty terms
# and win by omission rather than by a genuinely better-fitting frequency.

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

  for (i in seq_len(n_sample)) {
    g <- genotype[i, ]
    called_i <- n_called_mat
    alt_i <- alt_count_mat
    if (!is.na(own_pop_idx[i])) {
      p0 <- own_pop_idx[i]
      has_call <- !is.na(g)
      called_i[p0, ] <- called_i[p0, ] - has_call
      alt_i[p0, ] <- alt_i[p0, ] - ifelse(has_call, g, 0)
    }
    gene_copies_i <- 2 * called_i
    # A locus is usable for this sample only if EVERY candidate population
    # (after the leave-one-out adjustment above) still has at least one
    # call there -- the same symmetric locus-exclusion convention
    # population_tree.R uses for Nei's distance, applied per sample rather
    # than once globally because leave-one-out can zero out only the
    # sample's own population's count at a locus every other population
    # still has calls at.
    #
    # This must be checked across every population BEFORE scoring any of
    # them: an earlier version excluded a locus only from the one
    # population that individually had zero calls there, so each
    # population's log-likelihood was summed over a different number of
    # loci. Since every per-locus term is negative, summing more of them
    # can only push a population's total lower -- a population simply
    # missing more data at a locus could out-score a genuinely
    # better-fitting population by omission alone, not by a fair
    # comparison of the same evidence. Restricting every population to the
    # identical usable-locus set keeps their log-likelihoods directly
    # comparable, exactly as a leave-one-out self-assignment test requires.
    # A population left with no gene copies at any locus once this sample is
    # set aside -- the sample's own population when it is that population's
    # only member -- cannot be a candidate. Left in the all-populations test
    # it failed every locus, so the sample got no likelihood under ANY
    # population and was reported unassigned instead of being assigned among
    # the remaining populations.
    candidate <- rowSums(gene_copies_i > 0, na.rm = TRUE) > 0
    usable <- if (any(candidate)) {
      apply(gene_copies_i[candidate, , drop = FALSE] > 0, 2L, all)
    } else {
      rep(FALSE, n_snp)
    }
    for (p in seq_len(n_pop)) {
      if (!candidate[p] || !any(usable)) { log_lik[i, p] <- NA_real_; n_used[i, p] <- 0L; next }
      called <- called_i[p, usable]; alt <- alt_i[p, usable]
      gene_copies <- 2 * called
      freq <- alt / gene_copies
      floor_freq <- 1 / gene_copies
      freq <- ifelse(freq <= 0, floor_freq, freq)
      freq <- ifelse(freq >= 1, 1 - floor_freq, freq)
      contrib <- genotype_log_likelihood(g[usable], freq)
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
    # NA, not TRUE, when the recorded population was never a candidate (this
    # sample is its only member): assigning elsewhere is then forced, not
    # evidence of a migrant or a labelling error.
    mismatch = data.table::fifelse(
      is.na(log_lik[cbind(seq_len(n_sample), match(recorded_population, populations))]),
      NA,
      !is.na(assigned_population) & !is.na(recorded_population) &
        assigned_population != recorded_population
    ),
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
      subtitle = wrap_plot_text(if (n_scored > 0L) {
        sprintf(
          "%s / %s samples (%.1f%%) assign to their recorded population",
          n_match, n_scored, 100 * n_match / n_scored
        )
      } else {
        "No samples could be scored"
      }),
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
