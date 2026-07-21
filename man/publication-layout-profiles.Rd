\name{publication-layout-profiles}
\alias{new_publication_layout_profile}
\alias{validate_publication_layout_profile}
\alias{publication_layout_profile}
\alias{bind_publication_layout}
\alias{validate_publication_layout_binding}
\alias{publication_layout_parameters}
\alias{publication_layout_report}
\title{Deterministic journal-oriented publication layouts}
\usage{
new_publication_layout_profile(
  id,
  journal_profile = generic_journal_profile(),
  formats = .publication_layout_formats,
  geometry = list(paper = "letter", margin_mm = 25.4, columns = 1L),
  typography = list(font_family = "serif", font_size_pt = 11, line_spacing = 1.5),
  structure = list(heading_depth = 3L, numbered_sections = TRUE,
    numbered_figures = TRUE, captions = "below", table_captions = "above",
    figure_placement = "near-reference"),
  bibliography = list(style = "author-date", doi_as_url = TRUE),
  submission = list(line_numbering = FALSE, separate_title_page = FALSE,
    supplementary_index = TRUE),
  renderer_parameters = list(),
  version = "1.0.0"
)

validate_publication_layout_profile(profile, journal_profile = NULL)

publication_layout_profile(
  name = c("g3", "general", "molecular-ecology", "nature-style", "plos")
)

bind_publication_layout(spec, profile, overrides = list())

validate_publication_layout_binding(binding, spec, profile)

publication_layout_parameters(binding)

publication_layout_report(profile)
}
\arguments{
\item{id}{Stable layout-profile identifier.}
\item{journal_profile}{An existing validated journal submission profile.}
\item{formats}{Supported output formats.}
\item{geometry}{Named page and margin geometry settings.}
\item{typography}{Named typography settings.}
\item{structure}{Named heading, numbering, caption, and placement settings.}
\item{bibliography}{Named citation and bibliography settings.}
\item{submission}{Named submission-layout requirements.}
\item{renderer_parameters}{Named backend-independent renderer parameters.}
\item{version}{Layout-profile version.}
\item{profile}{A publication layout profile.}
\item{name}{Built-in profile identifier.}
\item{spec}{A publication report specification.}
\item{overrides}{Named deterministic layout overrides.}
\item{binding}{A publication layout binding.}
}
\value{
Constructors return deterministic fingerprinted records. Validators return \code{TRUE} invisibly. \code{publication_layout_parameters()} returns a canonically ordered named list and \code{publication_layout_report()} returns Markdown lines.
}
\description{
Defines deterministic journal-oriented rendering profiles and binds them to Phase 0.9 publication report specifications without replacing existing journal submission profiles or renderer contracts.
}
