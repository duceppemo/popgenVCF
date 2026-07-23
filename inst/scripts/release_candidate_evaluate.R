evaluate_release_candidate_dossier <- function(policy_path, index_path, evidence_root) {
  rc_need(); policy <- read_release_candidate_policy(policy_path)
  if (!file.exists(index_path)) stop("Evidence index is missing", call. = FALSE)
  idx <- jsonlite::read_json(index_path, simplifyVector = FALSE)
  if (!identical(rc_scalar(idx$schema_version, "index schema"), "1.0"))
    stop("Unsupported release-candidate evidence schema", call. = FALSE)
  mode <- rc_scalar(idx$mode, "index mode")
  if (!mode %in% as.character(unlist(policy$allowed_modes)))
    stop("Evidence index mode is invalid", call. = FALSE)
  candidate_id <- rc_scalar(idx$candidate_id, "candidate id")
  target <- rc_scalar(idx$target_release, "target release")
  version <- rc_scalar(idx$package_version, "package version")
  commit <- tolower(rc_scalar(idx$git_commit, "git commit"))
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("git_commit must be a lowercase 40-character SHA", call. = FALSE)
  evaluated <- rc_datetime(idx$evaluated_at, "evaluated_at")
  if (!identical(target, rc_scalar(policy$target_release, "policy target release")))
    stop("Evidence target release does not match policy", call. = FALSE)
  if (!identical(version, rc_scalar(policy$package_version, "policy package version")))
    stop("Evidence package version does not match policy", call. = FALSE)

  recs <- idx$records
  if (!is.list(recs)) stop("Evidence index records must be a list", call. = FALSE)
  ids <- vapply(recs, function(x) rc_scalar(x$gate_id, "record gate_id"), character(1L))
  expected <- policy$gate_table$gate_id
  if (anyDuplicated(ids)) stop("Evidence index contains duplicate gate records", call. = FALSE)
  missing <- setdiff(expected, ids); extra <- setdiff(ids, expected)
  if (length(missing) || length(extra))
    stop("Evidence index gate inventory mismatch; missing: ", paste(missing, collapse = ", "),
         "; extra: ", paste(extra, collapse = ", "), call. = FALSE)

  gate_rows <- vector("list", length(expected)); artifact_rows <- list()
  for (i in seq_along(expected)) {
    id <- expected[[i]]; gate <- policy$gate_table[i, , drop = FALSE]
    rec <- recs[[match(id, ids)]]
    status <- rc_scalar(rec$status, paste0(id, " status"))
    if (!status %in% as.character(unlist(policy$allowed_statuses)))
      stop("Invalid status for gate: ", id, call. = FALSE)
    summary <- rc_scalar(rec$summary, paste0(id, " summary"))
    approval <- rc_approval(rec$approval, isTRUE(gate$approval_required), id, status)
    arts <- rec$artifacts; if (is.null(arts)) arts <- list()
    if (!is.list(arts)) stop("artifacts must be a list for gate: ", id, call. = FALSE)
    if (status == "passed" && !length(arts))
      stop("Passed gate must retain at least one artifact: ", id, call. = FALSE)
    if (length(arts)) artifact_rows <- c(artifact_rows, lapply(seq_along(arts), function(j)
      rc_artifact(arts[[j]], evidence_root, id, j)))
    passed <- status == "passed" &&
      (!isTRUE(gate$approval_required) || approval$state == "approved")
    reason <- if (passed) "" else if (status != "passed") paste0(status, ": ", summary) else
      paste0("approval: ", approval$state)
    gate_rows[[i]] <- data.frame(
      order = gate$order, category = gate$category, gate_id = id,
      required = gate$required, approval_required = gate$approval_required,
      issue = gate$issue, status = status, passed = passed,
      approval_state = approval$state, reviewer = approval$reviewer,
      reviewed_at = approval$reviewed_at, summary = summary,
      blocking_reason = reason, stringsAsFactors = FALSE
    )
  }
  gates <- do.call(rbind, gate_rows)
  artifacts <- if (length(artifact_rows)) do.call(rbind, artifact_rows) else
    data.frame(gate_id = character(), artifact_index = integer(), path = character(),
               size_bytes = numeric(), sha256 = character(), stringsAsFactors = FALSE)
  if (anyDuplicated(artifacts$path)) stop("Evidence artifact paths must be unique", call. = FALSE)
  artifacts <- artifacts[order(match(artifacts$gate_id, expected), artifacts$artifact_index), , drop = FALSE]
  rownames(artifacts) <- NULL
  blockers <- gates[gates$required & !gates$passed,
    c("order", "category", "gate_id", "issue", "status", "blocking_reason"), drop = FALSE]
  if (mode == "rehearsal" && !nrow(blockers))
    blockers <- data.frame(order = max(gates$order) + 1L, category = "release",
      gate_id = "evaluation_mode", issue = "#1", status = "blocked",
      blocking_reason = "rehearsal mode cannot authorize a production release",
      stringsAsFactors = FALSE)
  ready <- mode == "production" && !nrow(blockers)
  dossier <- list(
    schema_version = "1.0", record_type = "popgenvcf_release_candidate_dossier",
    policy_id = rc_scalar(policy$policy_id, "policy id"), candidate_id = candidate_id,
    target_release = target, package_version = version, git_commit = commit,
    evaluated_at = evaluated, mode = mode, release_ready = ready,
    summary = list(required_gates = sum(gates$required),
      passed_required_gates = sum(gates$required & gates$passed),
      blocking_gates = nrow(blockers), retained_artifacts = nrow(artifacts)),
    inputs = list(
      policy = list(path = basename(policy_path),
        sha256 = digest::digest(policy_path, "sha256", file = TRUE)),
      evidence_index = list(path = basename(index_path),
        sha256 = digest::digest(index_path, "sha256", file = TRUE))),
    gates = lapply(seq_len(nrow(gates)), function(i) as.list(gates[i, , drop = FALSE])),
    blockers = lapply(seq_len(nrow(blockers)), function(i) as.list(blockers[i, , drop = FALSE])),
    artifacts = lapply(seq_len(nrow(artifacts)), function(i) as.list(artifacts[i, , drop = FALSE]))
  )
  structure(list(policy = policy, index = idx, gates = gates, blockers = blockers,
    artifacts = artifacts, dossier = dossier, release_ready = ready),
    class = "PopgenVCFReleaseCandidateDossier")
}
