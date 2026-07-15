\name{consensus_ancestry}
\alias{consensus_ancestry}
\alias{validate_ancestry_consensus}
\alias{ancestry_consensus_table}
\title{Build and validate consensus ancestry estimates}
\usage{
consensus_ancestry(x, confidence = 0.95, reference_replicate = NULL,
  tolerance = 1e-06)

validate_ancestry_consensus(x, tolerance = 1e-06)

ancestry_consensus_table(x)
}
\arguments{
\item{x}{A \code{PopgenVCFAncestryResult}, a list of canonical ancestry replicates, or a \code{PopgenVCFAncestryConsensus} for validation and table conversion.}
\item{confidence}{Confidence level used for empirical per-cell ancestry intervals.}
\item{reference_replicate}{Optional replicate identifier used as the alignment reference. The smallest replicate identifier is used by default.}
\item{tolerance}{Numerical tolerance for ancestry validation, alignment, and simplex checks.}
}
\value{
\code{consensus_ancestry()} returns a validated \code{PopgenVCFAncestryConsensus}. The validator returns its input invisibly. \code{ancestry_consensus_table()} returns a long-form \code{data.table}.
}
\description{
Aligns ancestry replicates across label permutations and calculates mean and median Q matrices, variance, standard deviation, empirical confidence intervals, cluster stability, sample uncertainty, alignment diagnostics, and a global stability score. All replicates must use the same backend, K, sample identifiers, and sample order.
}
