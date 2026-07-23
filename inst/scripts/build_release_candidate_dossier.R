rc_need <- function() {
  pkgs <- c("data.table", "digest", "jsonlite")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(miss)) stop("Missing required packages: ", paste(miss, collapse = ", "), call. = FALSE)
}
rc_scalar <- function(x, label) {
  x <- as.character(unlist(x, use.names = FALSE))
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    stop(label, " must be one non-empty string", call. = FALSE)
  trimws(x)
}
rc_bool <- function(x, label) {
  x <- as.logical(unlist(x, use.names = FALSE))
  if (length(x) != 1L || is.na(x)) stop(label, " must be logical", call. = FALSE)
  x
}
rc_sha <- function(x, label = "sha256") {
  x <- tolower(sub("^sha256:", "", rc_scalar(x, label)))
  if (!grepl("^[0-9a-f]{64}$", x)) stop(label, " must be a SHA-256 digest", call. = FALSE)
  x
}
rc_datetime <- function(x, label) {
  x <- rc_scalar(x, label)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", x))
    stop(label, " must use YYYY-MM-DDTHH:MM:SSZ", call. = FALSE)
  x
}
rc_date <- function(x, label) {
  x <- rc_scalar(x, label)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))
    stop(label, " must use YYYY-MM-DD", call. = FALSE)
  x
}

read_release_candidate_policy <- function(path) {
  rc_need()
  if (!file.exists(path)) stop("Release-candidate policy is missing", call. = FALSE)
  p <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(rc_scalar(p$schema_version, "policy schema"), "1.0"))
    stop("Unsupported release-candidate policy schema", call. = FALSE)
  rc_scalar(p$policy_id, "policy id"); rc_scalar(p$target_release, "target release")
  rc_scalar(p$package_version, "package version")
  if (!identical(as.character(unlist(p$allowed_modes)), c("rehearsal", "production")))
    stop("policy allowed_modes are invalid", call. = FALSE)
  if (!identical(as.character(unlist(p$allowed_statuses)),
                 c("passed", "failed", "blocked", "not_run")))
    stop("policy allowed_statuses are invalid", call. = FALSE)
  if (!is.list(p$gates) || !length(p$gates)) stop("policy must define gates", call. = FALSE)
  rows <- lapply(seq_along(p$gates), function(i) {
    g <- p$gates[[i]]
    data.frame(
      order = as.integer(unlist(g$order)),
      gate_id = rc_scalar(g$id, paste0("gate ", i, " id")),
      category = rc_scalar(g$category, paste0("gate ", i, " category")),
      required = rc_bool(g$required, paste0("gate ", i, " required")),
      approval_required = rc_bool(g$approval_required, paste0("gate ", i, " approval")),
      issue = rc_scalar(g$issue, paste0("gate ", i, " issue")),
      description = rc_scalar(g$description, paste0("gate ", i, " description")),
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  if (anyNA(tab$order) || any(tab$order < 1L) || anyDuplicated(tab$order) ||
      anyDuplicated(tab$gate_id))
    stop("policy gate identities and order must be unique", call. = FALSE)
  tab <- tab[order(tab$order), , drop = FALSE]; rownames(tab) <- NULL
  if (!identical(tab$order, seq_len(nrow(tab))))
    stop("policy gate order must be contiguous", call. = FALSE)
  p$gate_table <- tab
  p
}

rc_path <- function(path, root) {
  path <- gsub("\\\\", "/", rc_scalar(path, "artifact path"))
  if (grepl("^(/|[A-Za-z]:/)", path) || grepl("(^|/)\\.\\.(/|$)", path))
    stop("Evidence paths must be relative and cannot traverse directories: ", path, call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  abs <- normalizePath(file.path(root, path), winslash = "/", mustWork = TRUE)
  if (!startsWith(abs, paste0(root, "/")))
    stop("Evidence path is outside the evidence root: ", path, call. = FALSE)
  list(relative = path, absolute = abs)
}
rc_artifact <- function(x, root, gate_id, index) {
  if (!is.list(x) || !all(c("path", "size_bytes", "sha256") %in% names(x)))
    stop("Artifact records require path, size_bytes, and sha256", call. = FALSE)
  p <- rc_path(x$path, root)
  if (!file.exists(p$absolute) || isTRUE(file.info(p$absolute)$isdir))
    stop("Evidence artifact is not a regular file: ", p$relative, call. = FALSE)
  size <- suppressWarnings(as.numeric(unlist(x$size_bytes)))
  if (length(size) != 1L || is.na(size) || size < 0 || size != floor(size))
    stop("Artifact size_bytes must be a non-negative integer", call. = FALSE)
  sha <- rc_sha(x$sha256)
  if (!identical(as.numeric(file.info(p$absolute)$size), size))
    stop("Evidence artifact size mismatch: ", p$relative, call. = FALSE)
  if (!identical(digest::digest(p$absolute, "sha256", file = TRUE), sha))
    stop("Evidence artifact checksum mismatch: ", p$relative, call. = FALSE)
  data.frame(gate_id = gate_id, artifact_index = as.integer(index), path = p$relative,
             size_bytes = size, sha256 = sha, stringsAsFactors = FALSE)
}
rc_approval <- function(x, required, gate_id, status) {
  empty <- list(state = "", reviewer = "", reviewed_at = "", notes = "")
  if (is.null(x)) {
    if (required && identical(status, "passed"))
      stop("Passed gate requires approval metadata: ", gate_id, call. = FALSE)
    return(empty)
  }
  if (!is.list(x)) stop("approval must be an object for gate: ", gate_id, call. = FALSE)
  state <- rc_scalar(x$state, paste0(gate_id, " approval state"))
  if (!state %in% c("approved", "rejected", "pending"))
    stop("Invalid approval state for gate: ", gate_id, call. = FALSE)
  reviewer <- if (is.null(x$reviewer)) "" else rc_scalar(x$reviewer, "reviewer")
  reviewed <- if (is.null(x$reviewed_at)) "" else rc_date(x$reviewed_at, "reviewed_at")
  notes <- if (is.null(x$notes)) "" else rc_scalar(x$notes, "approval notes")
  if (state == "approved" && (!nzchar(reviewer) || !nzchar(reviewed)))
    stop("Approved gates require reviewer and reviewed_at: ", gate_id, call. = FALSE)
  if (required && status == "passed" && state != "approved")
    stop("Passed gate requires approved review state: ", gate_id, call. = FALSE)
  list(state = state, reviewer = reviewer, reviewed_at = reviewed, notes = notes)
}

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
  version <- rc_scalar(idx$package_version, "packae version")
  commit <- tolower(rc_scalar(idx$git_commit, "git commit"))
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("git_commit must be a lowercase 40-character SHA", call. = FALSE)
  evaluated <- rc_datetime(idx$evaluated_at, "evaluated_at")
  if (!identical(target, rc_scalar(policy$target_release, "policy target release")))
    stop("Evidence target release does not match policy", call. = FALSE)
  if (!identical(version, rc_scalar(policy$package_version, "policy package version")))
    stop("Evidence package version does not match policy", call. = FALSE)

  recs <- idx$records
  if (!is.list(recs)) stop("Evidence index records must be a list", call. = FALSE)
  ids <- vapply(recs,