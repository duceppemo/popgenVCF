\name{render_report}
\alias{render_report}
\title{Render a Population Genomics Report}
\usage{
render_report(
  results_rds,
  output_dir,
  title = "Population genomics analysis",
  author = ""
)
}
\arguments{
  \item{results_rds}{Path to a serialized pipeline result object.}
  \item{output_dir}{Report output directory.}
  \item{title}{Report title.}
  \item{author}{Report author.}
}
\value{The path returned by \code{rmarkdown::render()}, invisibly.}
\description{Renders the bundled manuscript-oriented HTML report template.}
