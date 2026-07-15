\name{core_results}
\alias{new_core_result}
\alias{validate_core_result}
\alias{new_pca_result}
\alias{new_ibs_result}
\alias{new_tree_result}
\alias{new_diversity_result}
\alias{new_fst_result}
\alias{new_amova_result}
\alias{new_dapc_result}
\alias{new_ibd_result}
\alias{as_core_result}
\alias{core_result_table}
\alias{save_core_result}
\alias{read_core_result}
\alias{print.PopgenVCFCoreResult}
\title{Canonical core analysis result objects}
\usage{
new_core_result(analysis, payload, parameters = list(), provenance = list(),
  metadata = NULL, validation = NULL,
  artifacts = new_artifact_manifest(), class_name = NULL)

validate_core_result(x)

new_pca_result(coordinates, eigenvalues, ...)
new_ibs_result(similarity, distance = 1 - similarity, mds = NULL, ...)
new_tree_result(tree, distances = NULL, ...)
new_diversity_result(statistics, ...)
new_fst_result(global_fst, pairwise, confidence_intervals = NULL, ...)
new_amova_result(components, statistics = NULL, ...)
new_dapc_result(coordinates, assignments = NULL, discriminant = NULL, ...)
new_ibd_result(distances, mantel_statistic, p_value = NA_real_,
  permutations = NA_integer_, ...)

as_core_result(analysis, x, parameters = list(), provenance = list(),
  metadata = NULL, validation = NULL, artifacts = new_artifact_manifest())

core_result_table(x)
save_core_result(x, path)
read_core_result(path)
\method{print}{PopgenVCFCoreResult}(x, ...)
}
\arguments{
\item{analysis}{Stable analysis identifier.}
\item{payload}{Named list containing analysis-specific statistical results.}
\item{parameters}{Named list of analysis parameters.}
\item{provenance}{Named list describing software, inputs, commands, and runtime.}
\item{metadata}{Optional sample or population metadata data frame.}
\item{validation}{Optional data frame containing \code{check} and \code{passed}.}
\item{artifacts}{A \code{PopgenVCFArtifactManifest}.}
\item{class_name}{Optional analysis-specific S3 class name.}
\item{x}{A canonical result, or an existing analysis output for adaptation.}
\item{coordinates}{Sample coordinate data frame containing \code{sample_id}.}
\item{eigenvalues}{Numeric PCA eigenvalues.}
\item{similarity}{Symmetric IBS similarity matrix.}
\item{distance}{Symmetric IBS-derived distance matrix.}
\item{mds}{Optional MDS coordinates.}
\item{tree}{Neighbour-joining or other tree representation.}
\item{distances}{Optional tree distances, or an IBD pairwise-distance data frame.}
\item{statistics}{Analysis statistics or optional AMOVA statistics.}
\item{global_fst}{Finite global FST estimate.}
\item{pairwise}{Pairwise FST data frame.}
\item{confidence_intervals}{Optional FST confidence intervals.}
\item{components}{AMOVA variance-component data frame.}
\item{assignments}{Optional DAPC assignments.}
\item{discriminant}{Optional DAPC discriminant model.}
\item{mantel_statistic}{Finite Mantel statistic.}
\item{p_value}{Optional Mantel permutation p-value.}
\item{permutations}{Optional number of Mantel permutations.}
\item{path}{RDS file path.}
\item{...}{Additional arguments passed to \code{new_core_result()}.}
}
\description{
Creates, validates, adapts, tabulates, serializes, and restores stable canonical
result objects for PCA, IBS/MDS, neighbour-joining trees, diversity, FST,
AMOVA, DAPC, and isolation-by-distance analyses.
}
\value{
Constructors and adapters return a validated analysis-specific object inheriting
from \code{PopgenVCFCoreResult}. \code{core_result_table()} returns a data table,
\code{save_core_result()} returns the path invisibly, and
\code{read_core_result()} returns a validated canonical result.
}
