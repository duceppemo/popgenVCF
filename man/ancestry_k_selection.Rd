\name{ancestry_k_selection}
\alias{select_ancestry_k}
\alias{validate_ancestry_k_selection}
\alias{ancestry_k_selection_text}
\title{Select ancestry K from fit metrics and replicate stability}
\usage{
select_ancestry_k(x, metric = NULL, direction = NULL,
  consensus = NULL, confidence = 0.95, plateau_fraction = 0.01,
  stability_threshold = 0.95)

validate_ancestry_k_selection(x)

ancestry_k_selection_text(x)
}
\arguments{
\item{x}{A \code{PopgenVCFAncestryResult}, list of ancestry replicates, or K-selection object for validation and text generation.}
\item{metric}{Optional backend metric name. A backend-specific default is selected when omitted.}
\item{direction}{Optional optimization direction, either \code{minimize} or \code{maximize}.}
\item{consensus}{Optional consensus ancestry object or list of objects supplying replicate stability by backend and K.}
\item{confidence}{Confidence level for empirical replicate metric intervals.}
\item{plateau_fraction}{Relative fit-improvement threshold used for plateau detection.}
\item{stability_threshold}{Minimum replicate stability considered strong.}
}
\value{
\code{select_ancestry_k()} returns a validated \code{PopgenVCFKSelection} object. The validator returns its input invisibly. \code{ancestry_k_selection_text()} returns named methods and results text.
}
\description{
Summarizes replicate-level ancestry fit metrics, applies direction-aware optimization, detects performance plateaus, incorporates replicate stability, and returns transparent backend-specific and cross-backend K recommendations with confidence scores and machine-readable reasons.
}
