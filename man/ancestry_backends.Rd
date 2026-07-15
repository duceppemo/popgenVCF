\name{ancestry_backends}
\alias{new_ancestry_backend}
\alias{new_ancestry_backend_registry}
\alias{register_ancestry_backend}
\alias{ancestry_backend_status}
\alias{default_ancestry_backend_registry}
\alias{run_ancestry}
\title{Unified ancestry backend plugins and execution}
\usage{
new_ancestry_backend(name, availability, execute, parse = identity,
  validate = function(task) invisible(task), description = "", version = "1.0")

new_ancestry_backend_registry(backends = list())

register_ancestry_backend(registry, backend, overwrite = FALSE)

ancestry_backend_status(registry = default_ancestry_backend_registry())

default_ancestry_backend_registry()

run_ancestry(input, sample_ids, backend = "auto", k_values = 2:10,
  replicates = 5L, seed = 42L,
  registry = default_ancestry_backend_registry(), fail_if_none = TRUE)
}
\arguments{
\item{name}{Stable lowercase backend identifier.}
\item{availability}{Function returning a logical value or a list with \code{available} and \code{reason}.}
\item{execute}{Function accepting one scheduled task and returning a backend-native result.}
\item{parse}{Function converting a native result and task to a \code{PopgenVCFAncestryReplicate}.}
\item{validate}{Optional backend-specific task validator.}
\item{description}{Human-readable backend description.}
\item{version}{Backend plugin contract version.}
\item{backends}{List of backend specifications.}
\item{registry}{A \code{PopgenVCFAncestryBackendRegistry}.}
\item{backend}{Backend specification when registering; backend name, names, \code{"auto"}, or \code{"all"} when running.}
\item{overwrite}{Replace an existing backend with the same name.}
\item{input}{Named list of backend inputs such as \code{plink_prefix}, \code{geno_file}, \code{output_dir}, and thread settings.}
\item{sample_ids}{Unique sample identifiers in exact Q-matrix row order.}
\item{k_values}{Positive integer K values greater than or equal to two.}
\item{replicates}{Independent replicates per K and backend.}
\item{seed}{Base seed used to derive deterministic task seeds.}
\item{fail_if_none}{Error when none of the requested backends is available.}
}
\value{
Backend constructors return plugin or registry objects. \code{ancestry_backend_status()} returns a \code{data.table}. \code{run_ancestry()} returns canonical results by backend, an execution record table, and the backend availability table.
}
\description{
Defines the backend-neutral plugin contract and unified ancestry execution API. Built-in adapters are supplied for ADMIXTURE, fastStructure, and LEA sNMF. Optional tools are discovered at runtime and unavailable backends can be skipped transparently.
}
\details{
A third-party backend supplies availability, validation, execution, and parsing callbacks. The parser must return a validated \code{PopgenVCFAncestryReplicate}; downstream label alignment, consensus estimation, K selection, plotting, and reporting are therefore independent of the native backend format.
}