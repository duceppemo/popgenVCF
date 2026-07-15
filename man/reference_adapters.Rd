\name{reference_adapters}
\alias{new_reference_adapter}
\alias{validate_reference_adapter}
\alias{reference_adapter_status}
\alias{reference_adapter_spec}
\alias{default_reference_adapter_registry}
\alias{run_reference_adapters}
\title{Established external-reference benchmark adapters}
\usage{
new_reference_adapter(id, analysis, tool,
  kind = c("package", "executable"), dependency,
  mode = c("numeric", "exact", "matrix", "subspace", "q_matrix"),
  role = c("equivalence", "diagnostic"), observed, reference,
  absolute_tolerance = 1e-8, relative_tolerance = 1e-6,
  interpretation = "", citations = character())

validate_reference_adapter(x)

reference_adapter_status(adapters)

reference_adapter_spec(adapter)

default_reference_adapter_registry()

run_reference_adapters(data,
  adapters = default_reference_adapter_registry(), analyses = NULL)
}
\arguments{
  \item{id, analysis, tool}{Stable adapter, analysis, and reference-tool identifiers.}
  \item{kind}{Whether the dependency is an R package or executable.}
  \item{dependency}{Package or executable identifier used for discovery.}
  \item{mode}{Canonical external-reference comparison mode.}
  \item{role}{Gating equivalence or non-gating diagnostic comparison.}
  \item{observed, reference}{Functions extracting values from a benchmark payload.}
  \item{absolute_tolerance, relative_tolerance}{Numerical tolerances.}
  \item{interpretation}{Scientific interpretation of the comparison.}
  \item{citations}{Reference citations.}
  \item{x, adapter}{A reference adapter.}
  \item{adapters}{An adapter list.}
  \item{data}{A payload containing \code{observed} and \code{references} lists.}
  \item{analyses}{Optional analysis filter.}
}
\value{
Constructors return validated adapter objects. Status returns a data table.
The runner returns adapter results, availability status, and a long-form table.
}
\description{
Defines dependency-aware adapters for established population-genetics packages
and executables. Unavailable dependencies are skipped transparently. The
built-in registry distinguishes strict equivalence from scientifically useful
cross-method diagnostics.
}
