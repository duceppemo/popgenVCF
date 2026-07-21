# Phase 0.9.3 - accessible and grayscale-safe figure modes

.publication_figure_style_ids <- c("accessibility-first", "grayscale-safe", "standard-color")

.publication_figure_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_figure_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(sprintf("%s must be one non-empty character value.", name), call. = FALSE)
  }
  trimws(x)
}

.publication_hex_rgb <- function(colours) {
  if (!is.character(colours) || !length(colours) || anyNA(colours)) {
    stop("colours must be a non-empty character vector.", call. = FALSE)
  }
  tryCatch(grDevices::col2rgb(colours) / 255,
           error = function(e) stop("colours contains an invalid colour.", call. = FALSE))
}

.publication_relative_luminance <- function(colours) {
  rgb <- .publication_hex_rgb(colours)
  linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(c(0.2126, 0.7152, 0.0722) %*% linear)
}

.publication_contrast_ratio <- function(a, b) {
  la <- .publication_relative_luminance(a)
  lb <- .publication_relative_luminance(b)
  (pmax(la, lb) + 0.05) / (pmin(la, lb) + 0.05)
}

#' Create a deterministic publication figure-style profile
#'
#' @param id Stable profile identifier.
#' @param colours Ordered colour palette.
#' @param linetypes Ordered line-type palette.
#' @param shapes Ordered point-shape palette.
#' @param fills Ordered fill palette.
#' @param background Figure background colour.
#' @param foreground Text and axis colour.
#' @param labels_required Whether direct or legend labels are mandatory.
#' @param colour_alone Whether colour may be the sole group encoding.
#' @param min_contrast Minimum foreground/background contrast ratio.
#' @param grayscale_safe Whether the profile guarantees distinct grayscale luminance.
#' @param version Profile version.
#' @return A fingerprinted `PopgenVCFPublicationFigureStyleProfile`.
#' @export
new_publication_figure_style_profile <- function(
    id, colours, linetypes, shapes, fills = colours,
    background = "#FFFFFF", foreground = "#000000",
    labels_required = TRUE, colour_alone = FALSE,
    min_contrast = 4.5, grayscale_safe = FALSE, version = "1.0.0") {
  id <- .publication_figure_scalar(id, "id")
  version <- .publication_figure_scalar(version, "version")
  colours <- as.character(colours)
  fills <- as.character(fills)
  linetypes <- as.character(linetypes)
  shapes <- as.integer(shapes)
  .publication_hex_rgb(c(colours, fills, background, foreground))
  if (!length(linetypes) || !length(shapes) || anyNA(shapes)) {
    stop("linetypes and shapes must be non-empty deterministic palettes.", call. = FALSE)
  }
  if (!is.numeric(min_contrast) || length(min_contrast) != 1L || is.na(min_contrast) || min_contrast < 1) {
    stop("min_contrast must be one number greater than or equal to 1.", call. = FALSE)
  }
  logicals <- c(labels_required, colour_alone, grayscale_safe)
  if (anyNA(logicals) || !all(vapply(logicals, is.logical, logical(1L)))) {
    stop("style flags must be non-missing logical values.", call. = FALSE)
  }
  profile <- list(
    record_type = "popgenvcf_publication_figure_style_profile",
    schema_version = "1.0.0", id = id, version = version,
    colours = colours, fills = fills, linetypes = linetypes, shapes = shapes,
    background = background, foreground = foreground,
    labels_required = labels_required, colour_alone = colour_alone,
    min_contrast = as.numeric(min_contrast), grayscale_safe = grayscale_safe
  )
  profile$fingerprint <- .publication_figure_fingerprint(profile)
  class(profile) <- c("PopgenVCFPublicationFigureStyleProfile", "list")
  validate_publication_figure_style_profile(profile)
  profile
}

#' Validate a publication figure-style profile
#' @param profile A figure-style profile.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_figure_style_profile <- function(profile) {
  if (!inherits(profile, "PopgenVCFPublicationFigureStyleProfile")) {
    stop("profile must be a publication figure-style profile.", call. = FALSE)
  }
  if (!identical(profile$schema_version, "1.0.0")) stop("Malformed figure-style profile.", call. = FALSE)
  .publication_figure_scalar(profile$id, "profile id")
  .publication_figure_scalar(profile$version, "profile version")
  .publication_hex_rgb(c(profile$colours, profile$fills, profile$background, profile$foreground))
  ratio <- .publication_contrast_ratio(profile$foreground, profile$background)
  if (ratio + .Machine$double.eps < profile$min_contrast) {
    stop("Figure-style profile fails its minimum contrast requirement.", call. = FALSE)
  }
  if (isTRUE(profile$colour_alone) && isTRUE(profile$labels_required)) {
    stop("Colour-alone encoding is incompatible with mandatory redundant labels.", call. = FALSE)
  }
  if (isTRUE(profile$grayscale_safe) && length(profile$colours) > 1L) {
    lum <- sort(.publication_relative_luminance(profile$colours))
    if (any(diff(lum) < 0.08)) stop("Grayscale-safe colours are not sufficiently distinguishable.", call. = FALSE)
  }
  if (!identical(profile$fingerprint, .publication_figure_fingerprint(profile))) {
    stop("Figure-style profile fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Return a built-in publication figure-style profile
#' @param name One of `accessibility-first`, `grayscale-safe`, or `standard-color`.
#' @return A deterministic figure-style profile.
#' @export
publication_figure_style_profile <- function(
    name = c("accessibility-first", "grayscale-safe", "standard-color")) {
  name <- match.arg(name)
  if (name == "grayscale-safe") {
    return(new_publication_figure_style_profile(
      name, c("#111111", "#666666", "#BBBBBB"),
      c("solid", "dashed", "dotted"), c(16L, 17L, 15L),
      grayscale_safe = TRUE
    ))
  }
  if (name == "accessibility-first") {
    return(new_publication_figure_style_profile(
      name, c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
      c("solid", "dashed", "dotdash", "dotted"), c(16L, 17L, 15L, 18L),
      labels_required = TRUE, colour_alone = FALSE
    ))
  }
  new_publication_figure_style_profile(
    name, c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"),
    c("solid", "dashed", "dotdash", "dotted", "longdash"),
    c(16L, 17L, 15L, 18L, 8L), labels_required = TRUE, colour_alone = FALSE
  )
}

#' Bind a figure style to publication layout and report contracts
#' @param spec A validated publication report specification.
#' @param layout A validated publication layout profile.
#' @param style A validated publication figure-style profile.
#' @param groups Number of scientific groups requiring distinct aesthetics.
#' @return A fingerprinted figure-style binding.
#' @export
bind_publication_figure_style <- function(spec, layout, style, groups = 1L) {
  validate_publication_report_spec(spec)
  validate_publication_layout_profile(layout)
  validate_publication_figure_style_profile(style)
  groups <- as.integer(groups)
  if (length(groups) != 1L || is.na(groups) || groups < 1L) stop("groups must be one positive integer.", call. = FALSE)
  capacities <- c(colours = length(style$colours), linetypes = length(style$linetypes), shapes = length(style$shapes))
  if (groups > max(capacities)) stop("Figure style cannot preserve all requested scientific groups.", call. = FALSE)
  if (!isTRUE(style$colour_alone) && groups > max(capacities[c("linetypes", "shapes")])) {
    stop("Figure style lacks sufficient redundant non-colour encodings.", call. = FALSE)
  }
  binding <- list(
    record_type = "popgenvcf_publication_figure_style_binding",
    schema_version = "1.0.0", specification_fingerprint = spec$fingerprint,
    layout_fingerprint = layout$fingerprint, style_id = style$id,
    style_version = style$version, style_fingerprint = style$fingerprint,
    groups = groups, formats = spec$formats
  )
  binding$fingerprint <- .publication_figure_fingerprint(binding)
  class(binding) <- c("PopgenVCFPublicationFigureStyleBinding", "list")
  binding
}

#' Validate a publication figure-style binding
#' @param binding A figure-style binding.
#' @param spec Originating report specification.
#' @param layout Originating layout profile.
#' @param style Originating figure-style profile.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_figure_style_binding <- function(binding, spec, layout, style) {
  if (!inherits(binding, "PopgenVCFPublicationFigureStyleBinding")) stop("binding must be a figure-style binding.", call. = FALSE)
  validate_publication_report_spec(spec)
  validate_publication_layout_profile(layout)
  validate_publication_figure_style_profile(style)
  if (!identical(binding$specification_fingerprint, spec$fingerprint) ||
      !identical(binding$layout_fingerprint, layout$fingerprint) ||
      !identical(binding$style_fingerprint, style$fingerprint) ||
      !identical(binding$formats, spec$formats)) {
    stop("Figure-style binding is not bound to its specification, layout, and style.", call. = FALSE)
  }
  if (!identical(binding$fingerprint, .publication_figure_fingerprint(binding))) {
    stop("Figure-style binding fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Return backend-independent plotting parameters
#' @param binding A validated figure-style binding.
#' @param style Originating figure-style profile.
#' @return A normalized named list of plotting parameters.
#' @export
publication_figure_parameters <- function(binding, style) {
  if (!inherits(binding, "PopgenVCFPublicationFigureStyleBinding") ||
      !identical(binding$style_fingerprint, style$fingerprint)) stop("Invalid figure-style binding.", call. = FALSE)
  validate_publication_figure_style_profile(style)
  list(
    background = style$background, colours = style$colours, fills = style$fills,
    foreground = style$foreground, labels_required = style$labels_required,
    linetypes = style$linetypes, min_contrast = style$min_contrast,
    shapes = style$shapes, style_fingerprint = style$fingerprint,
    style_profile = style$id, style_profile_version = style$version
  )
}

#' Audit a publication figure style
#' @param profile A validated figure-style profile.
#' @return A deterministic accessibility audit record.
#' @export
publication_figure_accessibility_audit <- function(profile) {
  validate_publication_figure_style_profile(profile)
  audit <- list(
    record_type = "popgenvcf_publication_figure_accessibility_audit",
    schema_version = "1.0.0", profile_id = profile$id,
    profile_fingerprint = profile$fingerprint,
    foreground_background_contrast = unname(.publication_contrast_ratio(profile$foreground, profile$background)),
    grayscale_luminance = unname(.publication_relative_luminance(profile$colours)),
    redundant_encoding = !isTRUE(profile$colour_alone),
    labels_required = profile$labels_required,
    passed = TRUE
  )
  audit$fingerprint <- .publication_figure_fingerprint(audit)
  class(audit) <- c("PopgenVCFPublicationFigureAccessibilityAudit", "list")
  audit
}
