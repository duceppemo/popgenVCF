\name{continuous-release-benchmarking}
\alias{new_release_performance_budget}
\alias{new_continuous_benchmark_observation}
\alias{validate_continuous_benchmark_observation}
\alias{compare_continuous_release_benchmark}
\alias{write_continuous_benchmark_evidence}
\title{Continuous release benchmarking}
\usage{
new_release_performance_budget(
  id,
  max_runtime_ratio = 1.1,
  max_memory_ratio = 1.1,
  min_throughput_ratio = 0.9,
  min_scaling_efficiency = 0.7,
  minimum_repetitions = 5L
)

new_continuous_benchmark_observation(
  benchmark_id, module, dataset_tier, release, git_sha,
  runtime_seconds, peak_memory_mb, throughput, scaling_efficiency,
  threads = 1L, repetitions = 5L, environment = list()
)

validate_continuous_benchmark_observation(observation)

compare_continuous_release_benchmark(current, baseline, budget)

write_continuous_benchmark_evidence(
  observations, comparisons = list(), output_dir,
  require_release_ready = FALSE
)
}
\arguments{
\item{id}{Stable performance-budget identifier.}
\item{max_runtime_ratio, max_memory_ratio}{Maximum permitted ratios versus baseline.}
\item{min_throughput_ratio, min_scaling_efficiency}{Minimum permitted performance ratios.}
\item{minimum_repetitions}{Minimum repetitions required for gating evidence.}
\item{benchmark_id, module, dataset_tier}{Stable benchmark descriptors.}
\item{release, git_sha}{Release identity and full source commit SHA.}
\item{runtime_seconds, peak_memory_mb, throughput, scaling_efficiency}{Performance measurements.}
\item{threads, repetitions}{Execution parameters.}
\item{environment}{Named environment and provenance metadata.}
\item{observation}{A continuous benchmark observation.}
\item{current, baseline}{Current and approved baseline observations.}
\item{budget}{A release performance budget.}
\item{observations}{Non-empty list of observations.}
\item{comparisons}{Optional list of benchmark comparisons.}
\item{output_dir}{Evidence destination directory.}
\item{require_release_ready}{Fail unless all comparisons are release ready.}
}
\description{
Creates deterministic benchmark observations, explicit performance budgets,
historical release comparisons, and machine-readable evidence for continuous
release benchmarking.
}
