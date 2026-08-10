\name{render_report}
\alias{render_report}
\title{Render a Population Genomics Report}
\usage{
render_report(
  results_rds,
  output_dir,
  title = "Population genomics analysis",
  author = "",
  formats = c("html", "pdf")
)
}
\arguments{
  \item{results_rds}{Path to a serialized pipeline result object.}
  \item{output_dir}{Report output directory.}
  \item{title}{Report title.}
  \item{author}{Report author.}
  \item{formats}{One or more of \code{html} and \code{pdf}. Both are produced by default. PDF rendering requires XeLaTeX, LuaLaTeX, or pdfLaTeX. When both formats are requested and LaTeX is unavailable, HTML is still produced and PDF is skipped with a warning. When both formats are requested on a non-Windows platform, they render concurrently in forked worker processes; Windows (no fork support) always renders sequentially.}
}
\value{Named rendered report paths, invisibly.}
\description{Renders the bundled manuscript-oriented report template as portable HTML, compact PDF, or both.}
