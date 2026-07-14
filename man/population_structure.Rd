\name{population_structure}
\alias{align_cluster_labels}
\alias{compare_q_matrices}
\alias{structure_reproducibility}
\alias{select_structure_k}
\alias{run_faststructure}
\alias{run_snmf}
\alias{run_population_structure_validation}
\title{Population-structure comparison and validation tools}
\description{Tools for label-switching-aware membership comparison, replicate reproducibility, K selection, optional external population-structure engines, and deterministic validation.}
\usage{
align_cluster_labels(target, reference)

compare_q_matrices(target, reference)

structure_reproducibility(matrices, reference = 1L)

select_structure_k(diagnostics)

run_faststructure(
  structure_executable = "structure.py",
  choosek_executable = "chooseK.py",
  plink_prefix,
  k_values,
  output_dir = ".",
  seed = 42L
)

run_snmf(
  geno_file,
  k_values,
  repetitions = 5L,
  entropy = TRUE,
  seed = 42L,
  project_mode = "new"
)

run_population_structure_validation(integration = FALSE, seed = 42L)
}
\arguments{
  \item{target}{Numeric sample-by-K membership matrix to align or compare.}
  \item{reference}{Reference membership matrix, or the reference replicate index/name.}
  \item{matrices}{Named list of equal-dimensional membership matrices.}
  \item{diagnostics}{Data frame containing K and one or more supported diagnostic columns.}
  \item{structure_executable}{Path or command name for fastStructure \code{structure.py}.}
  \item{choosek_executable}{Path or command name for fastStructure \code{chooseK.py}.}
  \item{plink_prefix}{Prefix of PLINK BED/BIM/FAM files.}
  \item{k_values}{Integer values of K to evaluate.}
  \item{output_dir}{Directory for external-program output.}
  \item{seed}{Random seed.}
  \item{geno_file}{LEA-format \code{.geno} file.}
  \item{repetitions}{Number of sNMF repetitions per K.}
  \item{entropy}{Whether sNMF computes cross entropy.}
  \item{project_mode}{LEA project mode passed to \code{LEA::snmf}.}
  \item{integration}{Run the optional adegenet DAPC integration fixture.}
}
\value{Function-specific lists or data tables containing aligned membership matrices, diagnostics, reproducibility metrics, or validation checks.}
\references{
Alexander, Novembre and Lange (2009). Fast model-based estimation of ancestry in unrelated individuals. Genome Research.

Raj, Stephens and Pritchard (2014). fastSTRUCTURE: variational inference of population structure in large SNP data sets. Genetics.

Frichot et al. (2014). Fast and efficient estimation of individual ancestry coefficients. Genetics.

Jombart, Devillard and Balloux (2010). Discriminant analysis of principal components. BMC Genetics.
}
