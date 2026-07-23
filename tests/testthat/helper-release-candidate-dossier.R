rc_module_dir <- function() {
  installed <- system.file("scripts", package = "popgenVCF")
  modules <- c(
    "release_candidate_utils.R",
    "release_candidate_policy.R",
    "release_candidate_evaluate.R",
    "release_candidate_write.R"
  )
  if (nzchar(installed) && all(file.exists(file.path(installed, modules)))) return(installed)
  testthat::test_path("..", "..", "inst", "scripts")
}

rc_policy_path <- function() {
  installed <- system.file("metadata", "release-candidate-policy.json", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  testthat::test_path("..", "..", "inst", "metadata", "release-candidate-policy.json")
}

rc_env <- function() {
  env <- new.env(parent = globalenv())
  module_dir <- rc_module_dir()
  for (module in c(
    "release_candidate_utils.R",
    "release_candidate_policy.R",
    "release_candidate_evaluate.R",
    "release_candidate_write.R"
  )) {
    sys.source(file.path(module_dir, module), envir = env)
  }
  env
}

rc_fixture <- function(env, mode = "production", blocked = NULL,
                       omit_gate = NULL, omit_approval = NULL) {
  root <- tempfile("popgenvcf-rc-")
  evidence <- file.path(root, "evidence")
  dir.create(evidence, recursive = TRUE)
  policy_path <- rc_policy_path()
  policy <- env$read_release_candidate_policy(policy_path)

  records <- lapply(seq_len(nrow(policy$gate_table)), function(i) {
    gate <- policy$gate_table[i, , drop = FALSE]
    rel <- paste0(sprintf("%02d", gate$order), "-", gate$gate_id, ".txt")
    abs <- file.path(evidence, rel)
    writeLines(paste("verified evidence for", gate$gate_id), abs, useBytes = TRUE)
    status <- if (identical(gate$gate_id, blocked)) "blocked" else "passed"
    artifacts <- if (status == "passed") list(list(
      path = rel,
      size_bytes = as.numeric(file.info(abs)$size),
      sha256 = digest::digest(abs, algo = "sha256", file = TRUE)
    )) else list()
    approval <- NULL
    if (isTRUE(gate$approval_required) && !identical(gate$gate_id, omit_approval)) {
      approval <- if (status == "passed") {
        list(state = "approved", reviewer = "Scientific Reviewer",
             reviewed_at = "2026-07-22", notes = "Reviewed retained evidence.")
      } else {
        list(state = "pending", notes = "Approval pending.")
      }
    }
    list(
      gate_id = gate$gate_id,
      status = status,
      summary = if (status == "passed") "Evidence is complete." else "Evidence is incomplete.",
      artifacts = artifacts,
      approval = approval
    )
  })
  if (!is.null(omit_gate)) {
    records <- records[vapply(records, function(x) !identical(x$gate_id, omit_gate), logical(1L))]
  }
  index <- list(
    schema_version = "1.0",
    mode = mode,
    candidate_id = "0.10.0-rc1",
    target_release = "v0.10.0",
    package_version = "0.10.0",
    git_commit = paste(rep("a", 40L), collapse = ""),
    evaluated_at = "2026-07-22T23:59:59Z",
    records = records
  )
  index_path <- file.path(root, "release-candidate-evidence-index.json")
  jsonlite::write_json(index, index_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  list(root = root, evidence = evidence, policy = policy,
       policy_path = policy_path, index_path = index_path)
}

