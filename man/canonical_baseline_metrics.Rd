\name{canonical_baseline_metrics}
\alias{new_canonical_baseline_metric}
\alias{validate_canonical_baseline_metric}
\alias{new_canonical_baseline_registry}
\alias{register_canonical_baseline_metric}
\alias{validate_canonical_baseline_registry}
\alias{compare_canonical_baseline_metric}
\alias{evaluate_canonical_baselines}
\alias{canonical_baseline_table}
\alias{write_canonical_baseline_evidence}
\alias{canonical_baseline_validation}
\alias{canonical_baseline_coverage}
\title{Canonical quantitative baseline metrics}
\description{Construct, register, compare, evaluate, summarize, and write deterministic evidence for versioned quantitative scientific baselines.}
\usage{
new_canonical_baseline_metric(id, dataset_id, analysis, expected,
  comparator = "exact", tolerance = 0, version = "1.0", rationale,
  provenance = list())

validate_canonical_baseline_metric(metric)

new_canonical_baseline_registry(metrics = list())

register_canonical_baseline_metric(registry, metric, replace = FALSE)

validate_canonical_baseline_registry(registry)

compare_canonical_baseline_metric(metric, observed)

evaluate_canonical_baselines(registry, observed, dataset_id = NULL)

canonical_baseline_table(result)

write_canonical_baseline_evidence(result, output_dir)

canonical_baseline_validation(baseline_registry, observe)

canonical_baseline_coverage(registry)
}
\arguments{
  \item{id}{Stable metric identifier.}
  \item{dataset_id}{Canonical dataset identifier, or optional dataset filter during evaluation.}
  \item{analysis}{Analysis producing the metric.}
  \item{expected}{Expected scalar or vector value.}
  \item{comparator}{One of \code{exact}, \code{absolute}, \code{relative}, \code{set}, or \code{distribution}.}
  \item{tolerance}{Non-negative numerical acceptance tolerance.}
  \item{version}{Baseline version.}
  \item{rationale}{Scientific justification for the comparator and tolerance.}
  \item{provenance}{Optional named provenance metadata.}
  \item{metric}{A canonical baseline metric.}
  \item{metrics}{Optional list of baseline metrics.}
  \item{registry}{A canonical baseline registry.}
  \item{replace}{Whether an existing metric may be replaced.}
  \item{observed}{Observed value, or a named list keyed by metric identifier.}
  \item{result}{A canonical baseline result.}
  \item{output_dir}{Evidence output directory.}
  \item{baseline_registry}{Canonical baseline registry captured by a suite callback.}
  \item{observe}{Function accepting a descriptor and directory and returning named observations.}
}
\value{Constructors return validated metric or registry objects. Comparisons and summaries return deterministic data frames. Evaluation returns a \code{PopgenVCFCanonicalBaselineResult}. The suite adapter returns a validation function. The evidence writer returns normalized paths.}
