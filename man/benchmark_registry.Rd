\name{benchmark_registry}
\alias{new_benchmark_dataset}
\alias{validate_benchmark_dataset}
\alias{new_benchmark_spec}
\alias{validate_benchmark_spec}
\alias{new_benchmark_registry}
\alias{register_benchmark}
\alias{list_benchmarks}
\alias{run_benchmark}
\alias{run_benchmark_suite}
\alias{validate_benchmark_result}
\alias{validate_benchmark_suite}
\alias{benchmark_suite_table}
\alias{save_benchmark_suite}
\alias{read_benchmark_suite}
\title{Canonical scientific benchmark registry and execution engine}
\description{Create, register, execute, validate, summarize, and serialize reproducible scientific and performance benchmarks.}
\usage{
new_benchmark_dataset(id, scale = "tiny", loader, source = "embedded",
  checksum = NA_character_, metadata = list())
new_benchmark_spec(id, category, dataset, runner, reference = NULL,
  comparator = NULL, absolute_tolerance = 1e-8,
  relative_tolerance = 1e-6, runtime_budget_seconds = Inf,
  memory_budget_mb = Inf, disk_budget_mb = Inf,
  requirements = NULL, citations = character())
new_benchmark_registry(specs = list())
register_benchmark(registry, spec)
list_benchmarks(registry)
run_benchmark(spec)
run_benchmark_suite(registry, ids = NULL, categories = NULL)
benchmark_suite_table(x)
save_benchmark_suite(x, path)
read_benchmark_suite(path)
}
\arguments{
  \item{id}{Stable dataset or benchmark identifier.}
  \item{scale}{Dataset scale.}
  \item{loader}{Function returning benchmark data.}
  \item{source}{Human-readable dataset source.}
  \item{checksum}{Optional dataset checksum.}
  \item{metadata}{Named dataset metadata.}
  \item{category}{Benchmark category.}
  \item{dataset}{A benchmark dataset.}
  \item{runner}{Function calculating observed benchmark values.}
  \item{reference}{Optional fixed reference values.}
  \item{comparator}{Optional comparison function.}
  \item{absolute_tolerance,relative_tolerance}{Numerical tolerances.}
  \item{runtime_budget_seconds,memory_budget_mb,disk_budget_mb}{Optional resource budgets.}
  \item{requirements}{Optional availability function.}
  \item{citations}{Scientific references.}
  \item{specs}{Initial list of specifications.}
  \item{registry}{A benchmark registry.}
  \item{spec}{A benchmark specification.}
  \item{ids,categories}{Optional execution filters.}
  \item{x}{A benchmark object or suite.}
  \item{path}{RDS file path.}
}
\value{Canonical benchmark objects, registries, suites, or stable summary tables.}
\details{Unavailable optional dependencies produce explicit skipped results. Numerical comparisons enforce absolute or relative tolerances, while runtime, memory, and disk budgets are evaluated independently and recorded transparently.}
