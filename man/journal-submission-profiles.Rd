\name{journal-submission-profiles}
\alias{new_journal_profile}
\alias{validate_journal_profile}
\alias{generic_journal_profile}
\alias{journal_profile}
\alias{apply_journal_profile}
\alias{validate_journal_submission}
\alias{render_journal_profile}
\alias{write_journal_profile}
\title{Deterministic journal submission profiles}
\usage{
new_journal_profile(id = "generic", required_roles = c("manuscript_source"),
  optional_roles = character(), filenames = NULL,
  description = "Generic popgenVCF journal submission profile",
  version = "1.0", journal = description, publisher = NA_character_,
  status = c("generic", "verified", "deprecated"),
  source_url = NA_character_, source_date = NA_character_,
  required_sections = c("abstract", "introduction", "methods", "results", "discussion"),
  optional_sections = character(),
  required_declarations = c("data_availability", "software_availability", "reproducibility", "funding", "author_contributions", "competing_interests"),
  required_companions = character(), title_max_chars = NA_integer_,
  abstract_max_words = NA_integer_, keyword_min = 0L,
  keyword_max = NA_integer_, highlight_min = 0L,
  highlight_max = NA_integer_, highlight_max_chars = NA_integer_,
  graphical_abstract = FALSE, figure_max = NA_integer_,
  table_max = NA_integer_, supplementary_max = NA_integer_,
  allowed_figure_extensions = c("pdf", "png", "tif", "tiff", "svg"),
  filename_pattern = NA_character_, overrides = list())

validate_journal_profile(x)

generic_journal_profile()

journal_profile(name = c("research-article", "short-communication", "data-note"))

apply_journal_profile(plan, profile = generic_journal_profile())

validate_journal_submission(profile, manuscript, companions = NULL,
  graphical_abstract = NULL, strict = FALSE)

render_journal_profile(profile)

write_journal_profile(profile, directory, overwrite = FALSE)
}
\arguments{
  \item{id}{Stable profile identifier.}
  \item{required_roles, optional_roles}{Semantic submission roles.}
  \item{filenames}{Named role-to-filename character vector.}
  \item{description}{Human-readable profile description.}
  \item{version}{Version identifier for the encoded requirements.}
  \item{journal}{Journal or generic profile name.}
  \item{publisher}{Publisher name, when applicable.}
  \item{status}{Profile status: generic, verified, or deprecated.}
  \item{source_url, source_date}{Versioned source and verification date for named profiles.}
  \item{required_sections, optional_sections}{Manuscript section requirements.}
  \item{required_declarations}{Required declaration names.}
  \item{required_companions}{Required companion document names.}
  \item{title_max_chars, abstract_max_words, keyword_min, keyword_max}{Title, abstract, and keyword limits.}
  \item{highlight_min, highlight_max, highlight_max_chars}{Highlight constraints.}
  \item{graphical_abstract}{Whether a graphical abstract is required.}
  \item{figure_max, table_max, supplementary_max}{Maximum artifact counts.}
  \item{allowed_figure_extensions}{Allowed figure filename extensions.}
  \item{filename_pattern}{Optional regular expression for submission filenames.}
  \item{overrides}{Named author-supplied requirement overrides.}
  \item{x}{A journal profile or written profile directory.}
  \item{name}{Built-in generic profile name.}
  \item{plan}{A submission-plan data frame with role and destination columns.}
  \item{profile}{A validated journal profile.}
  \item{manuscript}{A validated canonical manuscript.}
  \item{companions}{Optional submission companion record.}
  \item{graphical_abstract}{Optional graphical abstract specification.}
  \item{strict}{Whether failed requirements raise an error.}
  \item{directory}{Parent output directory.}
  \item{overwrite}{Whether an existing profile directory may be replaced.}
}
\description{
Creates, validates, applies, reports, renders, and writes deterministic journal submission profiles. Profiles encode explicit structural and packaging requirements without rewriting scientific content.
}
\value{
A journal profile, a profile-aware submission plan, a deterministic completeness report, Markdown text, a normalized output directory, or \code{TRUE} invisibly.
}
