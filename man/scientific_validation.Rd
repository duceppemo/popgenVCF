\name{scientific_validation}
\alias{validation_tolerances}
\alias{validation_statistics}
\alias{align_pca_signs}
\alias{compare_validation_values}
\alias{manual_ibs_matrix}
\alias{compare_pca_subspaces}
\alias{manual_population_diversity}
\alias{run_scientific_validation}
\title{Scientific validation utilities}
\description{
Deterministic fixtures, independent numerical calculations, PCA subspace
comparison, and the bundled scientific-validation suite.
}
\usage{
validation_tolerances()

validation_statistics(
  genotype,
  sample_ids = rownames(genotype),
  variant_ids = colnames(genotype)
)

align_pca_signs(target, reference)

compare_validation_values(
  observed,
  expected,
  tolerance,
  label = "comparison"
)

manual_ibs_matrix(genotype)

compare_pca_subspaces(observed, expected, components = 2L)

manual_population_diversity(genotype, populations)

run_scientific_validation(integration = FALSE, threads = 1L)
}
\arguments{
  \item{genotype}{Numeric sample-by-variant genotype dosage matrix containing
  values 0, 1, 2, or \code{NA}.}
  \item{sample_ids}{Optional character vector of sample identifiers.}
  \item{variant_ids}{Optional character vector of variant identifiers.}
  \item{target}{Numeric PCA score matrix whose component signs will be aligned.}
  \item{reference}{Numeric reference PCA matrix with matching dimensions.}
  \item{observed}{Observed numeric vector or matrix. For PCA subspace
  comparison, the observed score or eigenvector matrix.}
  \item{expected}{Expected numeric vector or matrix. For PCA subspace
  comparison, the independent reference matrix.}
  \item{tolerance}{Maximum permitted absolute numerical difference.}
  \item{label}{Character label attached to the comparison result.}
  \item{components}{Positive integer number of leading PCA components.}
  \item{populations}{Population label for every genotype-matrix row.}
  \item{integration}{Logical; run the complete SNPRelate and independent
  reference integration checks.}
  \item{threads}{Positive integer number of requested threads. LD validation
  is capped at four threads.}
}
\value{
The individual helpers return tolerance lists, validation tables, similarity
matrices, PCA-subspace metrics, or population-diversity tables.
\code{run_scientific_validation()} returns an overall pass flag and detailed
checks.
}
