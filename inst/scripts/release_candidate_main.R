main <- function(args = commandArgs(trailingOnly = TRUE)) {
  ready_required <- "--require-ready" %in% args
  args <- setdiff(args, "--require-ready")
  if (length(args) != 4L) {
    stop(
      "Usage: build_release_candidate_dossier.R <policy.json> <evidence-index.json> <evidence-root> <output-dir> [--require-ready]",
      call. = FALSE
    )
  }
  result <- evaluate_release_candidate_dossier(args[[1L]], args[[2L]], args[[3L]])
  print(write_release_candidate_dossier(result, args[[4L]]))
  cat(
    "Release-candidate dossier: ",
    if (result$release_ready) "READY" else "BLOCKED",
    "\n",
    sep = ""
  )
  if (ready_required && !result$release_ready) {
    stop("Release-candidate dossier is not release-ready", call. = FALSE)
  }
  invisible(result)
}
