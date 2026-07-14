\name{run_admixture_cv}
\alias{run_admixture_cv}
\title{Run ADMIXTURE Cross-Validation}
\usage{
run_admixture_cv(executable, plink_prefix, k_values, threads = 1L,
  cv_folds = 5L, output_dir = ".", seed = 42L)
}
\arguments{
  \item{executable}{Path or command name for the ADMIXTURE executable.}
  \item{plink_prefix}{Prefix of a PLINK BED/BIM/FAM dataset.}
  \item{k_values}{Integer vector of K values.}
  \item{threads}{Number of ADMIXTURE threads.}
  \item{cv_folds}{Number of cross-validation folds.}
  \item{output_dir}{Directory for logs and Q/P files.}
  \item{seed}{Random seed exposed to ADMIXTURE through the environment.}
}
\value{A data table containing cross-validation error by K.}
\description{Runs external ADMIXTURE analyses and parses cross-validation errors.}
