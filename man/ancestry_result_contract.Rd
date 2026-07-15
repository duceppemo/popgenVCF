\name{ancestry_result_contract}
\alias{new_ancestry_replicate}
\alias{validate_ancestry_replicate}
\alias{new_ancestry_result}
\alias{validate_ancestry_result}
\alias{ancestry_result_table}
\alias{ancestry_q_table}
\title{Canonical ancestry result contracts}
\usage{
new_ancestry_replicate(sample_ids, q, backend, k = ncol(q),
  replicate = 1L, seed = NA_integer_, metrics = numeric(),
  converged = NA, runtime_seconds = NA_real_, provenance = list(),
  tolerance = 1e-06)

validate_ancestry_replicate(x, tolerance = 1e-06)

new_ancestry_result(replicates)

validate_ancestry_result(x)

ancestry_result_table(x)

ancestry_q_table(x)
}
\arguments{
\item{sample_ids}{Unique sample identifiers in Q-matrix row order.}
\item{q}{Numeric ancestry coefficient matrix with one row per sample and one column per inferred cluster.}
\item{backend}{One of \code{admixture}, \code{faststructure}, or \code{snmf}.}
\item{k}{Number of inferred clusters.}
\item{replicate}{Positive replicate identifier.}
\item{seed}{Integer random seed or \code{NA_integer_}.}
\item{metrics}{Named numeric fit metrics.}
\item{converged}{Logical convergence status or \code{NA}.}
\item{runtime_seconds}{Nonnegative runtime in seconds or \code{NA_real_}.}
\item{provenance}{Named list containing backend and execution provenance.}
\item{tolerance}{Allowed absolute deviation of Q-matrix row sums from one.}
\item{x}{An ancestry replicate or ancestry result object.}
\item{replicates}{List of canonical ancestry replicate objects.}
}
\value{
Constructors return validated S3 objects. Validators return their input invisibly. Table helpers return \code{data.table} objects.
}
\description{
Defines backend-independent ancestry result objects used to normalize ADMIXTURE, fastStructure, and sNMF output. The contract enforces sample identity, Q-matrix dimensions and simplex constraints, replicate uniqueness, fit metrics, runtime, convergence, and provenance.
}
