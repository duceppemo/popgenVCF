write_release_candidate_gate_record <- function(
    gate_id, status, summary, artifact_paths, root, output_path, approval = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  artifact_paths <- normalizePath(artifact_paths, winslash = "/", mustWork = TRUE)
  if (!all(startsWith(artifact_paths, paste0(root, "/")))) {
    stop("artifact_paths must live under root", call. = FALSE)
  }
  relative <- substring(artifact_paths, nchar(root) + 2L)
  artifacts <- lapply(seq_along(artifact_paths), function(i) {
    list(
      path = relative[[i]],
      size_bytes = as.numeric(file.info(artifact_paths[[i]])$size),
      sha256 = tolower(digest::digest(artifact_paths[[i]], algo = "sha256", file = TRUE))
    )
  })
  record <- list(
    gate_id = gate_id, status = status, summary = summary,
    artifacts = artifacts, approval = approval
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    record, output_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
  )
  normalizePath(output_path, winslash = "/", mustWork = TRUE)
}
