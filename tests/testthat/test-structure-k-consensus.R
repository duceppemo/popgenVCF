test_that("cluster-number selection combines several zero-refit rules", {
  diagnostics <- data.frame(
    K = 2:6,
    cv_error = c(0.50, 0.40, 0.350, 0.349, 0.3485),
    cv_error_se = c(0.01, 0.01, 0.008, 0.008, 0.008),
    mean_success = c(0.60, 0.80, 0.90, 0.91, 0.905)
  )
  selection <- select_structure_k(diagnostics)

  expect_s3_class(selection, "PopgenVCFStructureKSelection")
  expect_setequal(
    selection$best_by_method$rule,
    c("optimum", "elbow", "one standard error", "near-optimum plateau")
  )
  expect_equal(nrow(selection$best_by_method), 6L)
  expect_true(selection$consensus_k %in% diagnostics$K)
  expect_identical(selection$consensus$total_votes, 6L)
  expect_equal(sum(selection$vote_summary$votes), 6L)
  expect_true(all(selection$scores$desirability >= 0 &
                  selection$scores$desirability <= 1))
})

test_that("cluster-number consensus reports tables and a multipanel figure", {
  selection <- select_structure_k(data.frame(
    K = 2:5,
    BIC = c(120, 80, 70, 69),
    calinski_harabasz = c(10, 45, 70, 60)
  ))
  captured <- NULL
  stem <- NULL
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      captured <<- p
      stem <<- stem
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  plot_structure_k_selection(
    selection, default_config(), list(figures = tempdir()),
    "cluster_selection", "Cluster-number selection"
  )

  expect_s3_class(captured$facet, "FacetWrap")
  expect_identical(captured$labels$title, "Cluster-number selection")
  expect_match(captured$labels$subtitle, "Consensus number of clusters")
  expect_true(any(vapply(
    captured$layers, function(layer) inherits(layer$geom, "GeomVline"),
    logical(1L)
  )))

  out <- tempfile("k-selection-tables-")
  dir.create(out)
  write_structure_k_selection(selection, list(tables = out), "selection")
  expect_setequal(
    list.files(out),
    c("selection_methods.tsv", "selection_scores.tsv",
      "selection_votes.tsv", "selection_consensus.tsv")
  )
})

test_that("backend-native recommendations participate in consensus voting", {
  text <- c(
    "Model complexity that maximizes marginal likelihood = 4",
    "Model components used to explain structure in data = 3"
  )
  votes <- popgenVCF:::parse_faststructure_k_votes(text, evaluated_k = 2:6)
  expect_setequal(votes$K, c(3L, 4L))
  expect_match(paste(votes$method, collapse = " "), "marginal likelihood")
  expect_match(paste(votes$method, collapse = " "), "components")

  selection <- select_structure_k(
    data.frame(K = 2:6), additional_votes = votes
  )
  expect_equal(nrow(selection$best_by_method), 2L)
  expect_identical(selection$consensus_k, 3L)
  expect_match(selection$consensus$tie_break, "simpler model")
})

test_that("Calinski-Harabasz separation rewards compact distinct clusters", {
  coordinates <- rbind(
    c(0, 0), c(0.1, -0.1), c(-0.1, 0.1),
    c(10, 10), c(10.1, 9.9), c(9.9, 10.1)
  )
  separated <- popgenVCF:::calinski_harabasz_score(
    coordinates, rep(c("A", "B"), each = 3L)
  )
  mixed <- popgenVCF:::calinski_harabasz_score(
    coordinates, rep(c("A", "B"), 3L)
  )

  expect_true(is.finite(separated))
  expect_gt(separated, mixed)
  separated_db <- popgenVCF:::davies_bouldin_index(
    coordinates, rep(c("A", "B"), each = 3L)
  )
  mixed_db <- popgenVCF:::davies_bouldin_index(
    coordinates, rep(c("A", "B"), 3L)
  )
  expect_true(is.finite(separated_db))
  expect_lt(separated_db, mixed_db)
})

test_that("davies_bouldin_index reports NA, not a falsely-good score, when two distinct groups share a centroid", {
  # Real edge case found in a pre-release audit: a zero distance between two
  # DISTINCT group centroids is a genuinely degenerate, maximally-bad case
  # (not the meaningless i == i self-term, which is correctly excluded).
  # The old code filtered candidates via a blanket is.finite() across the
  # whole distance row, silently dropping this real Inf along with the
  # self-term -- understating the index instead of reporting the
  # undefined/degenerate result as NA.
  coordinates <- rbind(
    c(0, 0), c(0.1, -0.1), c(-0.1, 0.1),
    c(0, 0), c(0.05, -0.05), c(-0.05, 0.05),
    c(10, 10), c(10.1, 9.9), c(9.9, 10.1)
  )
  groups <- rep(c("A", "B", "C"), each = 3L)
  db <- popgenVCF:::davies_bouldin_index(coordinates, groups)
  expect_true(is.na(db))
})

test_that("optional cluster-number consensus handles uninformative diagnostics", {
  cases <- list(
    single = data.frame(K = 2L, cv_error = 0.2),
    flat = data.frame(K = 2:4, cv_error = rep(0.2, 3L)),
    empty = data.frame(K = integer(), cv_error = numeric())
  )

  for (diagnostics in cases) {
    expect_null(
      popgenVCF:::select_structure_k_if_informative(diagnostics)
    )
  }

  informative <- popgenVCF:::select_structure_k_if_informative(data.frame(
    K = 2:4,
    cv_error = c(0.3, 0.2, 0.25)
  ))
  expect_s3_class(informative, "PopgenVCFStructureKSelection")
})
