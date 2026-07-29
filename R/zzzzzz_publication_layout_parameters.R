# Loaded after the core Phase 0.9.2 implementation so the complete renderer
# parameter vector, including profile metadata, is canonically ordered.
#' Return deterministic renderer parameters for a layout binding
#' @param binding A validated layout binding.
#' @return A normalized named list suitable for renderer execution.
#' @export
publication_layout_parameters <- function(binding) {
  if (!inherits(binding, "PopgenVCFPublicationLayoutBinding") ||
      !identical(binding$fingerprint, .publication_layout_fingerprint(binding))) {
    stop("Invalid publication layout binding.", call. = FALSE)
  }
  out <- c(
    list(
      layout_profile = binding$profile_id,
      layout_profile_version = binding$profile_version,
      layout_fingerprint = binding$profile_fingerprint
    ),
    as.list(unlist(binding$resolved, recursive = TRUE, use.names = TRUE))
  )
  out[order(names(out))]
}
