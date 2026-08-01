ancestry_three_backend_fixture_replicates <- function() {
  ids <- paste0("s", 1:6)
  make_backend <- function(backend, metric, k_values, jitter) {
    unlist(lapply(k_values, function(k) {
      lapply(seq_len(2L), function(rep) {
        base <- diag(k)[rep(seq_len(k), length.out = length(ids)), , drop = FALSE]
        noise <- matrix(stats::runif(length(ids) * k, 0, jitter), length(ids), k)
        q <- normalize_q_matrix(base + noise)
        new_ancestry_replicate(
          sample_ids = ids, q = q, backend = backend, k = k, replicate = rep,
          metrics = stats::setNames(k + rep * 0.001, metric)
        )
      })
    }), recursive = FALSE)
  }
  set.seed(1)
  c(
    make_backend("admixture", "cv_error", 2:3, 0.02),
    make_backend("faststructure", "marginal_likelihood", 2:3, 0.02),
    make_backend("snmf", "cross_entropy", 2:3, 0.02)
  )
}

ancestry_three_backend_fixture_evidence <- function(approval = "proposed", approved_by = NULL, approved_at = NULL) {
  reps <- ancestry_three_backend_fixture_replicates()
  backends <- c("admixture", "faststructure", "snmf")

  stability_by_k <- function(backend_name) {
    backend_reps <- Filter(function(r) identical(r$backend, backend_name), reps)
    ks <- vapply(backend_reps, `[[`, integer(1L), "k")
    metric_name <- names(backend_reps[[1L]]$metrics)
    rows <- lapply(sort(unique(ks)), function(k) {
      subset <- backend_reps[ks == k]
      consensus <- consensus_ancestry(subset)
      data.frame(
        k = k,
        metric_mean = mean(vapply(subset, function(r) unname(r$metrics[[metric_name]]), numeric(1L))),
        global_stability = consensus$global_stability,
        mean_alignment_score = mean(consensus$alignment_table$alignment_score)
      )
    })
    list(table = do.call(rbind, rows), metric_name = metric_name)
  }

  backend_evidence <- lapply(backends, function(b) {
    s <- stability_by_k(b)
    new_ancestry_backend_evidence(
      backend = b, tool_version = "1.0-test", command = sprintf("%s --k <k>", b),
      k_values = 2:3, replicates = 2L, seed = 42L,
      metric_name = s$metric_name, stability_by_k = s$table
    )
  })

  k_selection <- select_ancestry_k(reps, plateau_fraction = 0.5)
  selected_k <- as.integer(k_selection$overall_k)

  consensus_at_k <- stats::setNames(lapply(backends, function(b) {
    backend_reps <- Filter(function(r) identical(r$backend, b) && identical(r$k, selected_k), reps)
    consensus_ancestry(backend_reps)
  }), backends)

  pairs <- utils::combn(backends, 2, simplify = FALSE)
  cross_backend_comparisons <- lapply(pairs, function(pp) {
    alignment <- align_ancestry_replicate(consensus_at_k[[pp[1]]]$mean_q, consensus_at_k[[pp[2]]]$mean_q)
    new_ancestry_cross_backend_comparison(
      backend_a = pp[1], backend_b = pp[2], k = selected_k, alignment = alignment,
      minimum_alignment_score = 0.5, role = "diagnostic", interpretation = "test fixture"
    )
  })

  new_canonical_ancestry_three_backend_evidence(
    dataset_id = "test_dataset", dataset_version = "1.0",
    sample_ids = ids <- paste0("s", 1:6), region = "chrTest:1-1000",
    backend_evidence = backend_evidence, cross_backend_comparisons = cross_backend_comparisons,
    k_selection = k_selection, selected_k = selected_k,
    generated_by = "unit test fixture", generated_at = "2026-07-31T00:00:00Z",
    source_commit = paste(rep("a", 40L), collapse = ""),
    approval = approval, approved_by = approved_by, approved_at = approved_at
  )
}

test_that("canonical ancestry three-backend evidence is constructed from real replicate data", {
  evidence <- ancestry_three_backend_fixture_evidence()
  expect_s3_class(evidence, "PopgenVCFCanonicalAncestryThreeBackendEvidence")
  expect_identical(evidence$approval, "proposed")
  expect_length(evidence$backend_evidence, 3L)
  expect_identical(
    vapply(evidence$backend_evidence, `[[`, character(1L), "backend"),
    c("admixture", "faststructure", "snmf")
  )
  expect_length(evidence$cross_backend_comparisons, 3L)
  expect_true(evidence$selected_k %in% 2:3)
  expect_identical(evidence$selected_k, as.integer(evidence$k_selection$overall_k))
})

test_that("proposed evidence cannot carry approval metadata", {
  expect_error(
    ancestry_three_backend_fixture_evidence(approval = "proposed", approved_by = "someone"),
    "cannot contain approval metadata"
  )
})

test_that("approved evidence requires reviewer identity and an ISO-8601 date", {
  expect_error(
    ancestry_three_backend_fixture_evidence(approval = "approved"),
    "must be one non-empty string"
  )
  expect_error(
    ancestry_three_backend_fixture_evidence(approval = "approved", approved_by = "Reviewer", approved_at = "not-a-date"),
    "ISO-8601 date"
  )
})

test_that("approve_canonical_ancestry_three_backend_evidence transitions state and validates", {
  proposed <- ancestry_three_backend_fixture_evidence()
  expect_error(
    validate_canonical_ancestry_three_backend_evidence(proposed, require_approved = TRUE),
    "not approved"
  )
  approved <- approve_canonical_ancestry_three_backend_evidence(proposed, "Reviewer Name", "2026-07-31")
  expect_identical(approved$approval, "approved")
  expect_identical(approved$approved_by, "Reviewer Name")
  validate_canonical_ancestry_three_backend_evidence(approved, require_approved = TRUE)
  expect_error(
    approve_canonical_ancestry_three_backend_evidence(approved, "Reviewer Name", "2026-07-31"),
    "only proposed evidence"
  )
})

test_that("backend_evidence must cover exactly the three known backends", {
  evidence <- ancestry_three_backend_fixture_evidence()
  duplicated <- evidence$backend_evidence
  duplicated[[3L]] <- duplicated[[1L]]
  expect_error(
    new_canonical_ancestry_three_backend_evidence(
      dataset_id = "test_dataset", dataset_version = "1.0",
      sample_ids = paste0("s", 1:6), region = "chrTest:1-1000",
      backend_evidence = duplicated, cross_backend_comparisons = evidence$cross_backend_comparisons,
      k_selection = evidence$k_selection, selected_k = evidence$selected_k,
      generated_by = "test", generated_at = "2026-07-31T00:00:00Z",
      source_commit = paste(rep("a", 40L), collapse = "")
    ),
    "exactly one admixture, faststructure, and snmf"
  )
})

test_that("cross_backend_comparisons must cover each backend pair exactly once at selected_k", {
  evidence <- ancestry_three_backend_fixture_evidence()
  wrong_k <- evidence$cross_backend_comparisons
  wrong_k[[1L]]$k <- evidence$selected_k + 1L
  expect_error(
    new_canonical_ancestry_three_backend_evidence(
      dataset_id = "test_dataset", dataset_version = "1.0",
      sample_ids = paste0("s", 1:6), region = "chrTest:1-1000",
      backend_evidence = evidence$backend_evidence, cross_backend_comparisons = wrong_k,
      k_selection = evidence$k_selection, selected_k = evidence$selected_k,
      generated_by = "test", generated_at = "2026-07-31T00:00:00Z",
      source_commit = paste(rep("a", 40L), collapse = "")
    ),
    "must use selected_k"
  )
})

test_that("selected_k must equal the K-selection consensus overall_k", {
  evidence <- ancestry_three_backend_fixture_evidence()
  other_k <- setdiff(2:3, evidence$selected_k)[[1L]]

  # Rebuild cross_backend_comparisons at other_k so that check passes and the
  # selected_k-vs-k_selection$overall_k mismatch is the only remaining fault.
  reps <- ancestry_three_backend_fixture_replicates()
  backends <- c("admixture", "faststructure", "snmf")
  consensus_at_other_k <- stats::setNames(lapply(backends, function(b) {
    consensus_ancestry(Filter(function(r) identical(r$backend, b) && identical(r$k, other_k), reps))
  }), backends)
  pairs <- utils::combn(backends, 2, simplify = FALSE)
  comparisons_at_other_k <- lapply(pairs, function(pp) {
    alignment <- align_ancestry_replicate(
      consensus_at_other_k[[pp[1]]]$mean_q, consensus_at_other_k[[pp[2]]]$mean_q
    )
    new_ancestry_cross_backend_comparison(
      backend_a = pp[1], backend_b = pp[2], k = other_k, alignment = alignment,
      minimum_alignment_score = 0.5, role = "diagnostic", interpretation = "test fixture"
    )
  })

  expect_error(
    new_canonical_ancestry_three_backend_evidence(
      dataset_id = "test_dataset", dataset_version = "1.0",
      sample_ids = paste0("s", 1:6), region = "chrTest:1-1000",
      backend_evidence = evidence$backend_evidence, cross_backend_comparisons = comparisons_at_other_k,
      k_selection = evidence$k_selection, selected_k = other_k,
      generated_by = "test", generated_at = "2026-07-31T00:00:00Z",
      source_commit = paste(rep("a", 40L), collapse = "")
    ),
    "selected_k must equal k_selection"
  )
})

test_that("cross-backend comparison pass state is derived from the alignment threshold", {
  reps <- ancestry_three_backend_fixture_replicates()
  a <- Filter(function(r) identical(r$backend, "admixture") && identical(r$k, 2L), reps)
  b <- Filter(function(r) identical(r$backend, "snmf") && identical(r$k, 2L), reps)
  alignment <- align_ancestry_replicate(
    consensus_ancestry(a)$mean_q, consensus_ancestry(b)$mean_q
  )
  strict <- new_ancestry_cross_backend_comparison(
    "admixture", "snmf", 2L, alignment, minimum_alignment_score = 0.999999, role = "diagnostic"
  )
  lenient <- new_ancestry_cross_backend_comparison(
    "admixture", "snmf", 2L, alignment, minimum_alignment_score = 0, role = "equivalence"
  )
  expect_false(strict$passed)
  expect_true(lenient$passed)
  expect_identical(strict$backend_a, "admixture")
  expect_identical(strict$backend_b, "snmf")
})

test_that("cross-backend comparisons require distinct known backends", {
  reps <- ancestry_three_backend_fixture_replicates()
  a <- Filter(function(r) identical(r$backend, "admixture") && identical(r$k, 2L), reps)
  alignment <- align_ancestry_replicate(consensus_ancestry(a)$mean_q, consensus_ancestry(a)$mean_q)
  expect_error(
    new_ancestry_cross_backend_comparison("admixture", "admixture", 2L, alignment, 0.5, role = "diagnostic"),
    "must differ"
  )
  expect_error(
    new_ancestry_cross_backend_comparison("admixture", "not_a_backend", 2L, alignment, 0.5, role = "diagnostic"),
    "admixture, faststructure, or snmf"
  )
})

test_that("write and re-read canonical ancestry three-backend evidence round-trips", {
  evidence <- ancestry_three_backend_fixture_evidence()
  path <- tempfile(fileext = ".json")
  written <- write_canonical_ancestry_three_backend_evidence(evidence, path)
  expect_true(file.exists(written))
  payload <- jsonlite::read_json(written, simplifyVector = FALSE)
  expect_identical(payload$dataset_id, "test_dataset")
  expect_identical(payload$approval, "proposed")
  expect_length(payload$backend_evidence, 3L)
  expect_length(payload$cross_backend_comparisons, 3L)
  expect_identical(as.integer(payload$selected_k), evidence$selected_k)
})

test_that("read_canonical_ancestry_three_backend_evidence reconstructs a validated, equivalent object", {
  evidence <- ancestry_three_backend_fixture_evidence()
  path <- tempfile(fileext = ".json")
  write_canonical_ancestry_three_backend_evidence(evidence, path)
  reread <- read_canonical_ancestry_three_backend_evidence(path)
  expect_s3_class(reread, "PopgenVCFCanonicalAncestryThreeBackendEvidence")
  expect_identical(reread$dataset_id, evidence$dataset_id)
  expect_identical(reread$sample_order_sha256, evidence$sample_order_sha256)
  expect_identical(reread$selected_k, evidence$selected_k)
  expect_identical(reread$approval, "proposed")
  expect_identical(
    vapply(reread$backend_evidence, `[[`, character(1L), "backend"),
    vapply(evidence$backend_evidence, `[[`, character(1L), "backend")
  )
  expect_equal(
    reread$backend_evidence[[1L]]$stability_by_k,
    evidence$backend_evidence[[1L]]$stability_by_k
  )
  expect_equal(reread$k_selection$overall_k, evidence$k_selection$overall_k)
})

test_that("read_canonical_ancestry_three_backend_evidence enforces require_approved and round-trips approval", {
  evidence <- ancestry_three_backend_fixture_evidence()
  path <- tempfile(fileext = ".json")
  write_canonical_ancestry_three_backend_evidence(evidence, path)
  expect_error(
    read_canonical_ancestry_three_backend_evidence(path, require_approved = TRUE),
    "not approved"
  )
  reread <- read_canonical_ancestry_three_backend_evidence(path)
  approved <- approve_canonical_ancestry_three_backend_evidence(reread, "Reviewer Name", "2026-07-31")
  approved_path <- tempfile(fileext = ".json")
  write_canonical_ancestry_three_backend_evidence(approved, approved_path, require_approved = TRUE)
  reapproved <- read_canonical_ancestry_three_backend_evidence(approved_path, require_approved = TRUE)
  expect_identical(reapproved$approval, "approved")
  expect_identical(reapproved$approved_by, "Reviewer Name")
})

test_that("write_canonical_ancestry_three_backend_evidence enforces approval when required", {
  evidence <- ancestry_three_backend_fixture_evidence()
  expect_error(
    write_canonical_ancestry_three_backend_evidence(evidence, tempfile(fileext = ".json"), require_approved = TRUE),
    "not approved"
  )
  approved <- approve_canonical_ancestry_three_backend_evidence(evidence, "Reviewer Name", "2026-07-31")
  written <- write_canonical_ancestry_three_backend_evidence(
    approved, tempfile(fileext = ".json"), require_approved = TRUE
  )
  expect_true(file.exists(written))
})

test_that("stability_by_k must have one ordered row per k_values entry", {
  evidence <- ancestry_three_backend_fixture_evidence()
  broken <- evidence$backend_evidence[[1L]]
  broken$stability_by_k <- broken$stability_by_k[1L, , drop = FALSE]
  expect_error(validate_ancestry_backend_evidence(broken), "one ordered row per k_values")
})
