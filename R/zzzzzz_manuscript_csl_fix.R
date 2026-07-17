# Keep missing-file validation deterministic across R versions and platforms.
new_citation_profile <- function(style_id = "generic", csl = NULL) {
  style_id <- trimws(as.character(style_id)[1L])
  if (!nzchar(style_id)) stop("citation style identity must be non-empty", call. = FALSE)

  csl_path <- NULL
  csl_sha256 <- NA_character_
  csl_name <- NA_character_

  if (!is.null(csl)) {
    requested_path <- as.character(csl)[1L]
    if (!file.exists(requested_path)) {
      stop("citation style file does not exist", call. = FALSE)
    }
    csl_path <- normalizePath(requested_path, winslash = "/", mustWork = TRUE)
    if (!identical(tolower(tools::file_ext(csl_path)), "csl")) {
      stop("citation style file must use the .csl extension", call. = FALSE)
    }
    csl_sha256 <- digest::digest(csl_path, algo = "sha256", file = TRUE)
    csl_name <- basename(csl_path)
  }

  profile <- structure(list(
    schema_version = "1.0",
    style_id = style_id,
    csl_path = csl_path,
    csl_name = csl_name,
    csl_sha256 = csl_sha256,
    bundle_path = if (is.null(csl_path)) NA_character_ else "citation-style.csl"
  ), class = "PopgenVCFCitationProfile")

  validate_citation_profile(profile)
  profile
}
