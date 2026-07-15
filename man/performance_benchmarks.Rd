\name{performance_benchmarks}
\alias{performance_environment_fingerprint}
\alias{new_performance_benchmark_spec}
\alias{run_performance_benchmark}
\alias{compare_performance_baseline}
\alias{performance_benchmark_table}
\alias{save_performance_baseline}
\alias{read_performance_baseline}
\title{Performance and resource regression benchmarking}
\description{
Create repeatable runtime, memory, disk, and thread-scaling measurements and compare them with machine-compatible baselines.
}
\usage{
performance_environment_fingerprint()

new_performance_benchmark_spec(
  id,
  runner,
  threads = 1L,
  warmup = 1L,
  iterations = 5L,
  seed = 1L,
  runtime_regression = 0.2,
  memory_regression = 0.25,
  disk_regression = 0.25,
  gating = FALSE,
  metadata = list()
)

run_performance_benchmark(
  spec,
  fingerprint = performance_environment_fingerprint()
)

compare_performance_baseline(
  observed,
  baseline,
  allow_incompatible = FALSE,
  gating = observed$gating
)

performance_benchmark_table(x)

save_performance_baseline(x, path)

read_performance_baseline(path)
}
\arguments{
  \item{id}{Stable benchmark identifier.}
  \item{runner}{Function accepting a named \code{threads} argument. A writable temporary directory is exposed through \code{POPGENVCF_PERFORMANCE_TEMP}.}
  \item{threads}{Positive integer thread counts.}
  \item{warmup}{Number of unrecorded warmup iterations.}
  \item{iterations}{Number of recorded iterations per thread count.}
  \item{seed}{Deterministic base seed.}
  \item{runtime_regression, memory_regression, disk_regression}{Allowed relative increases versus a baseline.}
  \item{gating}{Whether detected regressions make the comparison fail.}
  \item{metadata}{Additional named metadata.}
  \item{spec}{A \code{PopgenVCFPerformanceSpec}.}
  \item{fingerprint}{Named environment fingerprint.}
  \item{observed, baseline}{Compatible \code{PopgenVCFPerformanceResult} objects.}
  \item{allow_incompatible}{Permit explicitly requested cross-host comparisons.}
  \item{x}{A performance result or comparison.}
  \item{path}{RDS baseline path.}
}
\value{
A fingerprint list, performance specification, performance result, comparison, table, or baseline path depending on the function.
}
\details{
Results summarize median runtime, median absolute deviation, 5th and 95th percentiles, approximate R-managed memory, temporary disk use, speedup, and scaling efficiency. Fingerprint compatibility is required by default because runtime comparisons across different hosts, R versions, processor counts, or BLAS implementations are generally not valid. Shared-runner CI can use non-gating comparisons while stable scheduled or release runners can use gating baselines.
}
