\name{external_reference_validation}
\alias{new_external_reference_spec}
\alias{validate_external_reference_spec}
\alias{compare_external_reference}
\alias{run_external_reference}
\alias{external_reference_table}
\alias{external_reference_benchmark_spec}
\title{Systematic external-reference scientific validation}
\usage{
new_external_reference_spec(
  id,
  analysis,
  reference_tool,
  mode = c("numeric", "exact", "matrix", "subspace", "q_matrix"),
  observed,
  reference,
  absolute_tolerance = 1e-08,
  relative_tolerance = 1e-06,
  role = c("equivalence", "diagnostic"),
  requirements = NULL,
  reference_version = NA_character_,
  interpretation = "",
  citations = character()
)

validate_external_reference_spec(x)

compare_external_reference(
  observed,
  reference,
  mode = c("numeric", "exact", "matrix", "subspace", "q_matrix"),
  absolute_tolerance = 1e-08,
  relative_tolerance = 1e-06
)

run_external_reference(spec, data)

external_reference_table(results)

external_reference_benchmark_spec(spec, dataset)
}
\arguments{
\item{id}{Stable comparison identifier.}
\item{analysis}{Analysis identifier such as \code{pca}, \code{ibs}, or \code{fst}.}
\item{reference_tool}{Reference package or executable name.}
\item{mode}{Comparison mode. Subspace comparisons use canonical correlations; Q-matrix comparisons align cluster labels before numerical comparison.}
\item{observed}{For the constructor, a function extracting the popgenVCF value from benchmark data. For direct comparison, the observed value.}
\item{reference}{For the constructor, a function extracting the reference value from benchmark data. For direct comparison, the reference value.}
\item{absolute_tolerance,relative_tolerance}{Nonnegative comparison tolerances.}
\item{role}{\code{equivalence} makes tolerance failures gating. \code{diagnostic} records differences without failing the comparison.}
\item{requirements}{Optional function returning \code{TRUE} or an actionable skip reason.}
\item{reference_version}{Optional reference implementation version.}
\item{interpretation}{Scientific interpretation of what agreement or disagreement means.}
\item{citations}{Scientific references.}
\item{x}{An external-reference specification.}
\item{spec}{An external-reference specification.}
\item{data}{Benchmark data passed to both adapters.}
\item{results}{One external-reference result or a list of results.}
\item{dataset}{A canonical benchmark dataset.}
}
\value{
The constructor returns a \code{PopgenVCFExternalReferenceSpec}. Direct comparison and table functions return data tables. \code{run_external_reference()} returns a \code{PopgenVCFExternalReferenceResult}; the benchmark adapter returns a \code{PopgenVCFBenchmarkSpec}.
}
\description{
Defines scientifically labelled, dependency-aware comparisons between popgenVCF outputs and independent reference implementations. Strict numerical equivalence is kept separate from transparent cross-method diagnostics so methodologically different estimators are not incorrectly presented as identical.
}
\examples{
spec <- new_external_reference_spec(
  id = "example_fst",
  analysis = "fst",
  reference_tool = "reference implementation",
  observed = function(x) x$observed,
  reference = function(x) x$reference
)
result <- run_external_reference(spec, list(observed = 0.1, reference = 0.1))
external_reference_table(result)
}
