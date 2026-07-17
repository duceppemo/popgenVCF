\name{analysis-publication}
\alias{publication_analysis_citations}
\alias{publication_analysis_narratives}
\alias{write_publication_bibliography}
\title{Analysis-specific publication narratives}
\description{
Generate methods text, figure legends, citation keys, and a BibTeX library from
canonical popgenVCF analysis results embedded in a reproducible project.
}
\usage{
publication_analysis_citations(kind, result = NULL)

publication_analysis_narratives(project)

write_publication_bibliography(narratives, path)
}
\arguments{
  \item{kind}{Analysis kind such as \code{pca}, \code{ibs}, \code{fst}, or \code{ancestry}.}
  \item{result}{Optional canonical analysis result.}
  \item{project}{A validated \code{PopgenVCFProject}.}
  \item{narratives}{The table returned by \code{publication_analysis_narratives()}.}
  \item{path}{Destination BibTeX file.}
}
\value{
\code{publication_analysis_citations()} returns citation keys.
\code{publication_analysis_narratives()} returns a data table with one row per
available canonical analysis. \code{write_publication_bibliography()} returns
the normalized path invisibly.
}
\details{
Narratives are generated only from fields present in canonical results. Missing
optional fields are omitted rather than inferred. IBS-derived distance is kept
distinct from population-level FST, DAPC is not described as ancestry, and
model-based ancestry language is reserved for ancestry backend results.
}
