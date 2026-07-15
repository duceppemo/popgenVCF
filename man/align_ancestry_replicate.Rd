\name{align_ancestry_replicate}
\alias{align_ancestry_replicate}
\title{Align ancestry Q matrices across label permutations}
\usage{
align_ancestry_replicate(target, reference, tolerance = 1e-06)
}
\arguments{
\item{target}{Numeric Q matrix or \code{PopgenVCFAncestryReplicate} to align.}
\item{reference}{Numeric Q matrix or replicate defining the desired cluster order.}
\item{tolerance}{Numerical tolerance for ancestry replicate validation.}
}
\value{
A \code{PopgenVCFAncestryAlignment} containing the aligned Q matrix, aligned replicate when applicable, permutation vector and matrix, correlation, cosine and combined similarity matrices, RMSD, and alignment scores.
}
\description{
Aligns ancestry clusters with a Hungarian assignment using a nonnegative combination of column-wise Pearson correlation and cosine similarity. Both inputs must have identical sample order and dimensions.
}
