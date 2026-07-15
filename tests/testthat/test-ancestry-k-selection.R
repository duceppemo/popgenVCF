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
