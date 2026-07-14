\name{pca_eigen_residuals}
\alias{pca_eigen_residuals}
\title{Validate PCA Eigenvectors Against a Covariance Matrix}
\usage{
pca_eigen_residuals(covariance, eigenvectors, components = 2L)
}
\arguments{
  \item{covariance}{Numeric square covariance or genetic relationship matrix.}
  \item{eigenvectors}{Numeric matrix with samples in rows and components in columns.}
  \item{components}{Number of leading components to validate.}
}
\value{A list containing relative residuals, orthogonality error, and summary maxima.}
\description{Checks the eigen equation directly using Rayleigh quotients.}
