\name{manuscript_regeneration_plans}
\alias{new_manuscript_regeneration_plan}
\alias{manuscript_regeneration_table}
\alias{validate_manuscript_regeneration_plan}
\alias{render_manuscript_regeneration_plan}
\alias{write_manuscript_regeneration_plan}
\title{Manuscript regeneration plans}
\usage{
new_manuscript_regeneration_plan(manuscript_id, revision_id, dependencies,
  changes, generator_id = "popgenVCF-manuscript")
manuscript_regeneration_table(x)
validate_manuscript_regeneration_plan(x, strict = FALSE)
render_manuscript_regeneration_plan(x)
write_manuscript_regeneration_plan(x, path, overwrite = FALSE)
}
\arguments{
\item{manuscript_id}{Stable manuscript identifier.}
\item{revision_id}{Stable target revision identifier.}
\item{dependencies}{Section dependency mappings.}
\item{changes}{Changed dependency records.}
\item{generator_id}{Stable generator identifier.}
\item{x}{A regeneration plan or output directory.}
\item{strict}{Whether blocked sections raise an error.}
\item{path}{Output directory.}
\item{overwrite}{Whether existing output may be replaced.}
}
\value{A plan object, table, Markdown text, or output path.}
\description{Builds deterministic section impact plans from explicit dependency changes. The plan does not modify manuscript text.}
