make_k_selection_replicates <- function(backend, metric, values) {
  ids <- paste0("s", 1:4)
  unlist(lapply(seq_along(values), function(i) {
    k <- i + 1L
    lapply(seq_len(2L), function(rep) {
      q <- matrix(1 / k, nrow = length(ids), ncol = k)
      jitter <- if (rep == 1L) -0.0005 else 0.0005
      new_ancestry_replicate(
        sample_ids = ids,
        q = q,
        backend = backend,
        k = k,
        replicate = rep,
        metrics = stats::setNames(values[[i]] + jitter, metric)
      )
    })
  }), recursive = FALSE)
}

test_that("K selection summarizes backend metrics and recommends a plateau", {
  reps <- c(
    make_k_selection_replicates("admixture", "cv_error", c(0.50, 0.40, 0.399, 0.3985)),
    make_k_selection_replicates("faststructure", "marginal_likelihood", c(10, 15, 15.05, 15.06)),
    make_k_selection_replicates("snmf", "cross_entropy", c(0.60, 0.45, 0.449, 0.4485))
  )
  x <- new_ancestry_result(reps)
  out <- select_ancestry_k(x, plateau_fraction = 0.02)

  expect_s3_class(out, "PopgenVCFKSelection")
  expect_equal(nrow(out$recommendations), 3L)
  expect_true(all(out$recommendations$recommended_k >= 2L))
  expect_true(out$overall_k %in% out$recommendations$recommended_k)
  expect_true(out$agreement >= 1 / 3)
  expect_true(all(out$summary$lower <= out$summary$upper))
  expect_true(all(out$summary$n_replicates == 2L))
  expect_match(out$reason, "backend recommendations")
})

test_that("K selection recommends the K just before the plateau starts, not the K it plateaus at", {
  # CV error 0.50/0.40/0.399/0.3985 at K=2..5: the K=3->K=4 step already
  # falls under 1% of the metric's total span (well under the 2% plateau
  # threshold here), so K=3 -- the *last K that still improved
  # meaningfully* -- is the parsimonious recommendation. An earlier
  # version recommended K=4 (one K too many: the first K the negligible
  # step arrives AT, not the K it departs FROM), contradicting this
  # function's own stated goal of preferring "the first stable model with
  # negligible fit loss ... over unnecessarily complex models".
  reps <- make_k_selection_replicates("admixture", "cv_error", c(0.50, 0.40, 0.399, 0.3985))
  out <- select_ancestry_k(reps, plateau_fraction = 0.02)
  expect_identical(out$recommendations$best_k, 5L)
  expect_identical(out$recommendations$plateau_k, 3L)
  expect_identical(out$recommendations$recommended_k, 3L)
})

test_that("K selection's plateau search ignores a negative (regressing) step, not just a small one", {
  # A dip before the true rise-to-plateau (replicate noise, not
  # diminishing returns) must not trigger the plateau search: a negative
  # relative_improvement is always "< plateau_fraction" too, so without an
  # explicit non-negative guard the search would stop at the dip instead
  # of continuing on to the real plateau. cv_error 0.50/0.52/0.40/0.399 at
  # K=2..5: K=3 is genuinely *worse* than K=2 (a regression), K=4 is the
  # real, large improvement, and K=4->K=5 is the negligible step -- the
  # correct plateau is K=4, not K=2 (which the dip alone would suggest).
  reps <- make_k_selection_replicates("admixture", "cv_error", c(0.50, 0.52, 0.40, 0.399))
  out <- select_ancestry_k(reps, plateau_fraction = 0.02)
  expect_identical(out$recommendations$plateau_k, 4L)
})

test_that("K selection respects explicit optimization direction", {
  reps <- make_k_selection_replicates("admixture", "score", c(1, 3, 2, 1))
  out <- select_ancestry_k(reps, metric = "score", direction = "maximize", plateau_fraction = 0)
  expect_identical(out$recommendations$best_k, 3L)
})

test_that("K selection produces manuscript-ready text and validates inputs", {
  reps <- make_k_selection_replicates("admixture", "cv_error", c(0.5, 0.4, 0.39, 0.389))
  out <- select_ancestry_k(reps)
  text <- ancestry_k_selection_text(out)
  expect_named(text, c("methods", "results"))
  expect_match(text[["results"]], "K=")

  no_metrics <- new_ancestry_replicate(
    paste0("s", 1:4), matrix(0.5, 4, 2), "admixture"
  )
  expect_error(select_ancestry_k(list(no_metrics)), "metric")
  expect_error(select_ancestry_k(reps, confidence = 1), "confidence")
})
