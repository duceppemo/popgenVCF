\name{manuscript_regeneration_verifications}
\alias{new_manuscript_regeneration_verification}
\alias{manuscript_regeneration_verification_table}
\alias{validate_manuscript_regeneration_verification}
\alias{render_manuscript_regeneration_verification}
\alias{write_manuscript_regeneration_verification}
\title{Deterministic manuscript regeneration verification records}
\usage{
new_manuscript_regeneration_verification(execution, checks, verification_id, plan = NULL)

manuscript_regeneration_verification_table(x)

validate_manuscript_regeneration_verification(x, execution = NULL, plan = NULL,
  strict = FALSE)

render_manuscript_regeneration_verification(x, execution = NULL, plan = NULL)

write_manuscript_regeneration_verification(x, path, execution = NULL, plan = NULL,
  overwrite = FALSE)
}
\arguments{
\item{execution}{A validated \code{PopgenVCFRegenerationExecution}.}
\item{checks}{A data frame of section identities, verification status, verifier, and notes.}
\item{verification_id}{A stable verification identifier.}
\item{plan}{An optional linked \code{PopgenVCFRegenerationPlan}.}
\item{x}{A verification record or written verification directory.}
\item{strict}{Whether every section must have status \code{verified}.}
\item{path}{Output directory.}
\item{overwrite}{Whether an existing output directory may be replaced.}
}
\value{The constructor returns a validated \code{PopgenVCFRegenerationVerification}; the table function returns a data table; the renderer returns Markdown lines; the writer returns the normalized path invisibly; validation returns \code{TRUE} invisibly.}
\description{Creates, validates, renders, and writes deterministic, tamper-evident records that verify regenerated manuscript output identities against a linked execution record.}
