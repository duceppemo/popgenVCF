scientific_review_scalar <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  trimws(x)
}

scientific_review_sha256 <- function(path) {
  tolower(digest::digest(path, algo = "sha256", file = TRUE))
}

scientific_review_is_safe_relative <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    !startsWith(path, "/") && !grepl("^[A-Za-z]:[/\\\\]", path) &&
    !grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", path)
}

scientific_review_is_within <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

scientific_review_find_one <- function(root, basename_value) {
  matches <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  matches <- matches[basename(matches) == basename_value & !file.info(matches)$isdir]
  sort(matches)
}

scientific_review_md_table <- function(x) {
  if (!nrow(x)) return("_None._")
  values <- lapply(x, function(column) {
    column <- as.character(column)
    column[is.na(column)] <- ""
    gsub("|", "\\\\|", column, fixed = TRUE)
  })
  x <- as.data.frame(values, stringsAsFactors = FALSE)
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(x, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  )
}

scientific_review_checklist <- function() {
  data.frame(
    gate_id = c(
      rep("all", 5L), rep("production_baseline", 7L),
      rep("external_concordance", 5L), rep("ancestry_three_backend", 6L),
      rep("benchmark_history", 5L), rep("scientific_approval", 5L)
    ),
    item_id = c(
      "candidate_identity", "transport_integrity", "artifact_completeness",
      "environment_provenance", "conflict_and_scope",
      "source_identity", "sample_inventory", "analysis_contract", "qc_evidence",
      "pca_evidence", "metric_values", "tolerance_rationale",
      "tool_inventory", "command_and_version", "estimator_compatibility",
      "numerical_comparisons", "diagnostic_interpretation",
      "same_biological_input", "sample_order", "replicate_design", "k_selection",
      "label_alignment", "biological_limits",
      "benchmark_identity", "repetition_count", "environment_comparability",
      "budget_checks", "trend_interpretation",
      "all_gates_complete", "failures_resolved", "claims_supported",
      "decision_notes", "approval_record"
    ),
    required_review = c(
      "Commit, package version, candidate ID, and target release all identify the same candidate.",
      "Every checksum and evidence-index size/digest check passes.",
      "Required files, logs, tables, commands, versions, and citations are retained.",
      "OS, R, packages, containers, external binaries, threads, and seeds are recorded.",
      "Confirm reviewer role, competence, scope, and any conflict of interest.",
      "Confirm dataset ID/version and filename-bound source checksums against the approved registry.",
      "Confirm 2,504 unique samples, complete population/superpopulation/sex fields, and exact VCF-panel matching.",
      "Confirm region 22:20000000-21000000, biallelic SNP filter, missingness 0.20, MAF 0.05, LD r2 0.20, seed 42, ten PCs, and four threads.",
      "Inspect sample QC, metadata matching, independent/sequential QC counts, and retained marker identities.",
      "Inspect PCA scores/variance, eigenvalue behavior, outliers, population patterns, and absence of sample-order errors.",
      "Independently confirm all six reported values against the retained tables; do not rely only on snapshot self-consistency.",
      "Accept 0 tolerance only for discrete counts and relative 1e-6 only if justified for PCA proportions; never loosen a tolerance to pass.",
      "Confirm every required tool-analysis pair exists and equivalence versus diagnostic roles are appropriate.",
      "Confirm exact commands, versions, executable/container checksums, inputs, sample order, and citations.",
      "Confirm both implementations estimate the same quantity before treating a comparison as equivalence.",
      "Inspect every row, error, tolerance, status, and any failure/skip; equivalence records must pass.",
      "Confirm diagnostic disagreements are explained and are not represented as equivalence.",
      "Confirm ADMIXTURE, fastStructure, and LEA/sNMF use the same checksum-pinned biological dataset and immutable samples.",
      "Recompute sample-order checksums and verify each Q-matrix row maps to the correct sample.",
      "Confirm K range, seeds, repetitions, backend settings, convergence/fit statistics, and complete logs.",
      "Inspect backend-specific K criteria; do not infer biological truth from a minimum statistic alone.",
      "Inspect label permutation, aligned-Q diagnostics, replicate RMSE/correlations, and consensus construction.",
      "Confirm the report distinguishes computational agreement from biological correctness of K/components.",
      "Confirm benchmark ID, module, dataset tier, threads, release, commit, and baseline identity.",
      "Confirm both observations meet the declared minimum repetitions.",
      "Confirm hardware/software/environment fingerprints are comparable; otherwise classify as insufficient evidence.",
      "Inspect runtime, memory, throughput, and scaling checks against the predeclared budget.",
      "Inspect historical trends and explain changes; runner noise must not be presented as scientific regression.",
      "All assigned gates have complete, checksum-bound evidence for the exact candidate.",
      "No failed, skipped, errored, missing, unexplained, or insufficient equivalence evidence remains.",
      "Methods, results, limitations, captions, and release claims agree with retained evidence.",
      "Record an artifact-specific rationale, limitations, deviations, and unresolved concerns.",
      "Record approved or rejected, reviewer identity, actual ISO date, notes, and exact artifact digests; never sign a pending template blindly."
    ),
    reviewer_status = "pending",
    reviewer_notes = "",
    stringsAsFactors = FALSE
  )
}

build_scientific_review_packet <- function(
    evidence_dir, output_dir, assignment_path, policy_path, strict = FALSE) {
  for (package in c("data.table", "digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is required", call. = FALSE)
    }
  }
  evidence_dir <- normalizePath(
    scientific_review_scalar(evidence_dir, "evidence_dir"),
    winslash = "/", mustWork = TRUE
  )
  if (!dir.exists(evidence_dir)) stop("evidence_dir must be a directory", call. = FALSE)
  assignment_path <- normalizePath(assignment_path, winslash = "/", mustWork = TRUE)
  policy_path <- normalizePath(policy_path, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (scientific_review_is_within(output_dir, evidence_dir)) {
    stop("output_dir must be outside evidence_dir", call. = FALSE)
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("output_dir must be absent or empty", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  checks <- list()
  add_check <- function(category, check_id, status, evidence = "", detail = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      category = category, check_id = check_id, status = status,
      evidence = as.character(evidence), detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }

  assignment <- jsonlite::read_json(assignment_path, simplifyVector = TRUE)
  policy <- jsonlite::read_json(policy_path, simplifyVector = FALSE)
  reviewer <- assignment$reviewer
  assigned_gates <- as.character(assignment$gate_ids)
  policy_gates <- vapply(policy$gates, `[[`, character(1L), "id")
  add_check(
    "assignment", "assigned_gate_policy_match",
    if (all(assigned_gates %in% policy_gates)) "pass" else "fail",
    basename(assignment_path), paste(assigned_gates, collapse = ", ")
  )
  add_check(
    "assignment", "assignment_is_not_approval",
    if (identical(assignment$state, "assigned") && !isTRUE(assignment$approval_conferred)) "pass" else "fail",
    basename(assignment_path), "Assignment must not confer approval."
  )

  all_paths <- list.files(evidence_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  all_paths <- all_paths[basename(all_paths) %in% setdiff(basename(all_paths), c(".", ".."))]
  links <- all_paths[nzchar(Sys.readlink(all_paths))]
  add_check(
    "integrity", "no_symbolic_links", if (!length(links)) "pass" else "fail",
    paste(basename(links), collapse = ", "),
    if (!length(links)) "No symbolic links found." else "Evidence must contain regular files only."
  )

  checksum_manifests <- sort(all_paths[
    !file.info(all_paths)$isdir & grepl("SHA256SUMS\\.txt$", basename(all_paths))
  ])
  if (!length(checksum_manifests)) {
    add_check("integrity", "checksum_manifests_present", "not_available", "", "No checksum manifest found.")
  }
  for (manifest in checksum_manifests) {
    relative_manifest <- substring(manifest, nchar(evidence_dir) + 2L)
    lines <- readLines(manifest, warn = FALSE)
    valid <- length(lines) > 0L && all(grepl("^[A-Fa-f0-9]{64} [ *].+", lines))
    add_check(
      "integrity", paste0("manifest_format:", relative_manifest),
      if (valid) "pass" else "fail", relative_manifest,
      if (valid) paste(length(lines), "entries") else "Malformed or empty SHA-256 manifest."
    )
    if (!valid) next
    expected <- tolower(substr(lines, 1L, 64L))
    relative <- substring(lines, 67L)
    safe <- vapply(relative, scientific_review_is_safe_relative, logical(1L))
    duplicate <- duplicated(relative) | duplicated(relative, fromLast = TRUE)
    for (i in seq_along(relative)) {
      target <- file.path(dirname(manifest), relative[[i]])
      target_ok <- safe[[i]] && !duplicate[[i]] && file.exists(target) &&
        !dir.exists(target) && !nzchar(Sys.readlink(target))
      digest_ok <- target_ok && identical(scientific_review_sha256(target), expected[[i]])
      add_check(
        "integrity", paste0("checksum:", relative_manifest, ":", relative[[i]]),
        if (digest_ok) "pass" else "fail", relative[[i]],
        if (digest_ok) expected[[i]] else "Unsafe, missing, duplicated, non-regular, or checksum-mismatched file."
      )
    }
    if (basename(manifest) %in% c(
      "canonical-production-SHA256SUMS.txt", "autosomal-baseline-SHA256SUMS.txt"
    )) {
      present <- sort(list.files(dirname(manifest), recursive = TRUE, all.files = FALSE))
      present <- setdiff(present, basename(manifest))
      complete <- identical(sort(relative), present)
      add_check(
        "integrity", paste0("manifest_completeness:", relative_manifest),
        if (complete) "pass" else "fail", relative_manifest,
        if (complete) "Manifest covers every retained file." else "Manifest inventory and directory contents differ."
      )
    }
  }

  index_matches <- scientific_review_find_one(evidence_dir, "release-candidate-evidence-index.json")
  index <- NULL
  if (length(index_matches) == 1L) {
    index <- jsonlite::read_json(index_matches[[1L]], simplifyVector = FALSE)
    add_check("index", "unique_evidence_index", "pass", basename(index_matches), "One evidence index found.")
  } else {
    add_check(
      "index", "unique_evidence_index", if (length(index_matches) > 1L) "fail" else "not_available",
      paste(index_matches, collapse = ", "),
      if (length(index_matches)) "More than one evidence index found." else "Evidence can be reviewed by component, but final gate review requires the index."
    )
  }

  gate_summary <- data.frame(
    gate_id = assigned_gates, status = "not_available", approval_state = "not_available",
    reviewer = "", reviewed_at = "", artifact_count = 0L,
    readiness = "evidence_missing", stringsAsFactors = FALSE
  )
  if (!is.null(index)) {
    records <- index$records
    for (gate_id in assigned_gates) {
      matching <- records[vapply(records, function(record) identical(record$gate_id, gate_id), logical(1L))]
      row <- match(gate_id, gate_summary$gate_id)
      if (length(matching) != 1L) {
        add_check("index", paste0("gate_record:", gate_id), "fail", gate_id, "Gate record missing or duplicated.")
        next
      }
      record <- matching[[1L]]
      approval <- record$approval
      gate_summary$status[[row]] <- record$status
      gate_summary$approval_state[[row]] <- if (is.null(approval$state)) "missing" else approval$state
      gate_summary$reviewer[[row]] <- if (is.null(approval$reviewer)) "" else approval$reviewer
      gate_summary$reviewed_at[[row]] <- if (is.null(approval$reviewed_at)) "" else approval$reviewed_at
      gate_summary$artifact_count[[row]] <- length(record$artifacts)
      gate_summary$readiness[[row]] <- if (identical(record$status, "passed") && length(record$artifacts)) {
        if (identical(approval$state, "approved")) "recorded_approved" else "ready_for_review"
      } else {
        "evidence_incomplete"
      }
      add_check(
        "index", paste0("gate_record:", gate_id),
        if (identical(record$status, "passed") && length(record$artifacts)) "pass" else "not_available",
        gate_id, paste("status", record$status, "artifacts", length(record$artifacts))
      )
      for (artifact in record$artifacts) {
        rel <- artifact$path
        safe <- scientific_review_is_safe_relative(rel)
        path <- file.path(evidence_dir, rel)
        regular <- safe && file.exists(path) && !dir.exists(path) && !nzchar(Sys.readlink(path))
        size_ok <- regular && identical(as.numeric(file.info(path)$size), as.numeric(artifact$size_bytes))
        sha_ok <- size_ok && identical(scientific_review_sha256(path), tolower(artifact$sha256))
        add_check(
          "index", paste0("indexed_artifact:", gate_id, ":", rel),
          if (sha_ok) "pass" else "fail", rel,
          if (sha_ok) artifact$sha256 else "Unsafe, missing, size-mismatched, or checksum-mismatched indexed artifact."
        )
      }
    }
    add_check(
      "index", "candidate_identity", "manual", basename(index_matches),
      paste("candidate", index$candidate_id, "release", index$target_release,
            "version", index$package_version, "commit", index$git_commit)
    )
  }

  baseline_rows <- data.frame(
    metric_id = character(), observed = character(), snapshot_expected = character(),
    comparator = character(), tolerance = character(), internal_match = logical(),
    stringsAsFactors = FALSE
  )
  baseline_json <- scientific_review_find_one(evidence_dir, "autosomal-baseline-proposal.json")
  baseline_tsv <- scientific_review_find_one(evidence_dir, "autosomal-baseline-observations.tsv")
  if (length(baseline_json) == 1L && length(baseline_tsv) == 1L) {
    snapshot <- jsonlite::read_json(baseline_json[[1L]], simplifyVector = FALSE)
    observations <- data.table::fread(baseline_tsv[[1L]], data.table = FALSE)
    metrics <- snapshot$baseline_registry$metrics
    for (metric in metrics) {
      observed <- observations$value[match(metric$id, observations$metric_id)]
      expected <- unlist(metric$expected, recursive = TRUE, use.names = FALSE)[[1L]]
      observed_number <- suppressWarnings(as.numeric(observed))
      expected_number <- suppressWarnings(as.numeric(expected))
      tolerance <- as.numeric(metric$tolerance)
      internal_match <- if (identical(metric$comparator, "exact")) {
        isTRUE(all.equal(observed_number, expected_number, tolerance = 0))
      } else if (identical(metric$comparator, "absolute")) {
        is.finite(observed_number) && is.finite(expected_number) && abs(observed_number - expected_number) <= tolerance
      } else {
        is.finite(observed_number) && is.finite(expected_number) &&
          abs(observed_number - expected_number) <= tolerance * max(abs(expected_number), .Machine$double.eps)
      }
      baseline_rows <- rbind(baseline_rows, data.frame(
        metric_id = metric$id, observed = observed, snapshot_expected = format(expected, digits = 17),
        comparator = metric$comparator, tolerance = format(tolerance, scientific = TRUE),
        internal_match = internal_match, stringsAsFactors = FALSE
      ))
    }
    add_check(
      "baseline", "proposal_internal_consistency",
      if (nrow(baseline_rows) == 6L && all(baseline_rows$internal_match)) "pass" else "fail",
      basename(baseline_json),
      "This checks serialization consistency only; the reviewer must independently inspect source tables."
    )
    add_check(
      "baseline", "proposal_unapproved_boundary",
      if (identical(snapshot$approval, "proposed") && is.null(snapshot$approved_by) && is.null(snapshot$approved_at)) "pass" else "fail",
      basename(baseline_json), paste("approval", snapshot$approval)
    )
  } else {
    add_check("baseline", "proposal_available", "not_available", "", "Baseline proposal JSON and observations TSV were not both found.")
  }

  concordance_rows <- data.frame()
  concordance_json <- scientific_review_find_one(evidence_dir, "scientific_concordance.json")
  if (length(concordance_json) == 1L) {
    concordance <- jsonlite::read_json(concordance_json[[1L]], simplifyVector = FALSE)
    concordance_rows <- do.call(rbind, lapply(concordance$records, function(record) data.frame(
      dataset_id = record$dataset_id, analysis = record$analysis,
      reference_tool = record$reference_tool, role = record$role,
      status = record$status, passed = record$passed, approval = record$approval,
      stringsAsFactors = FALSE
    )))
    equivalence <- concordance_rows[concordance_rows$role == "equivalence", , drop = FALSE]
    ready <- isTRUE(concordance$inventory_complete) && nrow(equivalence) > 0L &&
      all(equivalence$status == "passed") && all(equivalence$approval == "approved")
    add_check(
      "concordance", "equivalence_inventory", if (ready) "pass" else "not_available",
      basename(concordance_json),
      paste("inventory_complete", concordance$inventory_complete, "release_ready", concordance$release_ready)
    )
  } else {
    add_check("concordance", "evidence_available", "not_available", "", "No unique scientific_concordance.json found.")
  }

  benchmark_rows <- data.frame()
  benchmark_json <- scientific_review_find_one(evidence_dir, "continuous_benchmarks.json")
  if (length(benchmark_json) == 1L) {
    benchmark <- jsonlite::read_json(benchmark_json[[1L]], simplifyVector = FALSE)
    benchmark_rows <- do.call(rbind, lapply(benchmark$comparisons, function(comparison) data.frame(
      observation_key = comparison$observation_key, current_release = comparison$current_release,
      baseline_release = comparison$baseline_release, status = comparison$status,
      evidence_complete = comparison$evidence_complete, release_ready = comparison$release_ready,
      stringsAsFactors = FALSE
    )))
    ready <- nrow(benchmark_rows) > 0L && all(benchmark_rows$status == "passed") &&
      all(benchmark_rows$evidence_complete) && all(benchmark_rows$release_ready)
    add_check("benchmark", "comparison_evidence", if (ready) "pass" else "not_available",
              basename(benchmark_json), paste("release_ready", benchmark$release_ready))
  } else {
    add_check("benchmark", "evidence_available", "not_available", "", "No unique continuous_benchmarks.json found.")
  }

  ancestry_files <- all_paths[grepl(
    "(admixture|faststructure|snmf|ancestry|sample.order|q.matrix|k.selection|alignment)",
    basename(all_paths), ignore.case = TRUE
  ) & !file.info(all_paths)$isdir]
  add_check(
    "ancestry", "candidate_files", if (length(ancestry_files)) "manual" else "not_available",
    paste(basename(ancestry_files), collapse = ", "),
    paste(length(ancestry_files), "candidate ancestry evidence files found; semantic review is always manual.")
  )

  check_table <- do.call(rbind, checks)
  checklist <- scientific_review_checklist()
  data.table::fwrite(check_table, file.path(output_dir, "automated-checks.tsv"), sep = "\t")
  data.table::fwrite(gate_summary, file.path(output_dir, "assigned-gates.tsv"), sep = "\t")
  data.table::fwrite(checklist, file.path(output_dir, "manual-review-checklist.tsv"), sep = "\t")
  data.table::fwrite(baseline_rows, file.path(output_dir, "baseline-summary.tsv"), sep = "\t")
  if (nrow(concordance_rows)) data.table::fwrite(concordance_rows, file.path(output_dir, "concordance-summary.tsv"), sep = "\t")
  if (nrow(benchmark_rows)) data.table::fwrite(benchmark_rows, file.path(output_dir, "benchmark-summary.tsv"), sep = "\t")

  decision <- list(
    schema_version = "1.0",
    record_type = "popgenvcf_scientific_review_decision_template",
    target_release = assignment$target_release,
    candidate_id = if (is.null(index)) NULL else index$candidate_id,
    git_commit = if (is.null(index)) NULL else index$git_commit,
    reviewer = reviewer,
    reviewed_at = NULL,
    decision = "pending",
    gate_decisions = lapply(assigned_gates, function(gate_id) list(
      gate_id = gate_id, decision = "pending", notes = NULL
    )),
    limitations = NULL,
    statement = "Complete only after manual scientific review. This template is not approval."
  )
  jsonlite::write_json(
    decision, file.path(output_dir, "scientific-review-decision-template.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  failures <- sum(check_table$status == "fail")
  unavailable <- sum(check_table$status == "not_available")
  gate_complete <- all(gate_summary$readiness %in% c("ready_for_review", "recorded_approved"))
  packet_status <- if (failures) {
    "INTEGRITY FAILED"
  } else if (!gate_complete) {
    "EVIDENCE INCOMPLETE"
  } else {
    "READY FOR MANUAL SCIENTIFIC REVIEW"
  }
  report <- c(
    "# popgenVCF scientific review packet", "",
    paste0("**Packet status: ", packet_status, "**"), "",
    paste0("- Reviewer: ", reviewer$name, " (ORCID: ", reviewer$orcid, ")"),
    paste0("- Evidence directory: `", evidence_dir, "`"),
    paste0("- Automated failures: ", failures),
    paste0("- Unavailable or incomplete automated checks: ", unavailable), "",
    "> Automated checks establish integrity and summarize declared comparisons. They do not establish biological validity and they never confer approval.", "",
    "## Assigned gates", "", scientific_review_md_table(gate_summary), "",
    "## Automated-check summary", "",
    scientific_review_md_table(as.data.frame(table(check_table$status), stringsAsFactors = FALSE)), "",
    "## Baseline values", "", scientific_review_md_table(baseline_rows), "",
    "Internal match only confirms that the proposal snapshot and observation table agree. Independently inspect the retained QC and PCA source tables.", "",
    "## Concordance records", "", scientific_review_md_table(concordance_rows), "",
    "## Benchmark comparisons", "", scientific_review_md_table(benchmark_rows), "",
    "## Required manual review", "",
    "Complete `manual-review-checklist.tsv`. Every required item needs a reviewer status and evidence-specific notes.", "",
    "## Decision and return path", "",
    "1. Resolve every failed or unavailable required item; regenerate evidence instead of editing measured outputs.",
    "2. Complete the checklist and inspect all retained tables, logs, commands, versions, citations, tolerances, and limitations.",
    "3. Copy the decision template, fill the actual review date, set each gate to `approved` or `rejected`, and provide artifact-specific notes.",
    "4. Submit the completed checklist, decision JSON, and this checksummed packet as a reviewed repository change or attach them to the release-evidence record.",
    "5. Rebuild the production release-candidate dossier. Only that evaluator compares the signed gate records with the exact indexed artifacts and determines readiness.", "",
    "No email or external submission is performed by this script."
  )
  writeLines(report, file.path(output_dir, "scientific-review-report.md"), useBytes = TRUE)

  packet_files <- sort(list.files(output_dir, full.names = TRUE))
  packet_files <- packet_files[basename(packet_files) != "scientific-review-packet-SHA256SUMS.txt"]
  checksum_lines <- paste(
    vapply(packet_files, scientific_review_sha256, character(1L)),
    basename(packet_files), sep = "  "
  )
  writeLines(checksum_lines, file.path(output_dir, "scientific-review-packet-SHA256SUMS.txt"), useBytes = TRUE)

  cat("Scientific review packet:", normalizePath(output_dir), "\n")
  cat("Status:", packet_status, "\n")
  cat("Automated failures:", failures, "\n")
  if (isTRUE(strict) && (failures > 0L || !gate_complete)) {
    stop("scientific review packet is not complete in strict mode", call. = FALSE)
  }
  invisible(list(
    output_dir = normalizePath(output_dir), status = packet_status,
    checks = check_table, gates = gate_summary
  ))
}

scientific_review_packet_main <- function(args, source_root) {
  strict <- "--strict" %in% args
  args <- setdiff(args, "--strict")
  if (length(args) != 2L) {
    stop(paste(
      "Usage: build_scientific_review_packet.R",
      "<evidence-dir> <output-dir> [--strict]"
    ), call. = FALSE)
  }
  build_scientific_review_packet(
    evidence_dir = args[[1L]], output_dir = args[[2L]],
    assignment_path = file.path(source_root, "inst", "metadata", "scientific-review-assignment.json"),
    policy_path = file.path(source_root, "inst", "metadata", "release-candidate-policy.json"),
    strict = strict
  )
}
