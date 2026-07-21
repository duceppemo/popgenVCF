\name{publication-figure-styles}
\alias{new_publication_figure_style_profile}
\alias{validate_publication_figure_style_profile}
\alias{publication_figure_style_profile}
\alias{bind_publication_figure_style}
\alias{validate_publication_figure_style_binding}
\alias{publication_figure_parameters}
\alias{publication_figure_accessibility_audit}
\title{Deterministic Accessible Publication Figure Styles}
\usage{
new_publication_figure_style_profile(
  id, colours, linetypes, shapes, fills = colours,
  background = "#FFFFFF", foreground = "#000000",
  labels_required = TRUE, colour_alone = FALSE,
  min_contrast = 4.5, grayscale_safe = FALSE, version = "1.0.0"
)

validate_publication_figure_style_profile(profile)

publication_figure_style_profile(
  name = c("accessibility-first", "grayscale-safe", "standard-color")
)

bind_publication_figure_style(spec, layout, style, groups = 1L)

validate_publication_figure_style_binding(binding, spec, layout, style)

publication_figure_parameters(binding, style)

publication_figure_accessibility_audit(profile)
}
\arguments{
  \item{id}{Stable profile identifier.}
  \item{colours}{Ordered colour palette.}
  \item{linetypes}{Ordered line-type palette.}
  \item{shapes}{Ordered point-shape palette.}
  \item{fills}{Ordered fill palette.}
  \item{background}{Figure background colour.}
  \item{foreground}{Text and axis colour.}
  \item{labels_required}{Whether labels or legends are mandatory.}
  \item{colour_alone}{Whether colour may be the sole group encoding.}
  \item{min_contrast}{Minimum foreground/background contrast ratio.}
  \item{grayscale_safe}{Whether grayscale luminance separation is required.}
  \item{version}{Profile version.}
  \item{profile}{A publication figure-style profile.}
  \item{name}{A built-in profile name.}
  \item{spec}{A publication report specification.}
  \item{layout}{A publication layout profile.}
  \item{style}{A publication figure-style profile.}
  \item{groups}{Number of scientific groups requiring distinct aesthetics.}
  \item{binding}{A publication figure-style binding.}
}
\value{A deterministic profile, binding, parameter list, audit record, or invisible \code{TRUE}.}
\description{Creates, validates, binds, exports, and audits deterministic accessibility-aware figure presentation contracts.}
