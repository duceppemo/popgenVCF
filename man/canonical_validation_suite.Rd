\name{canonical_validation_suite}
\alias{new_canonical_validation_suite}
\alias{register_canonical_validation}
\alias{validate_canonical_validation_suite}
\alias{run_canonical_validation_suite}
\alias{canonical_validation_suite_table}
\alias{canonical_validation_coverage}
\alias{write_canonical_validation_suite}
\title{Canonical validation suites}
\description{Construct, execute, summarize, and write deterministic evidence for multi-dataset canonical validation suites.}
\usage{
new_canonical_validation_suite(id, title, fail_fast = TRUE, metadata = list())

register_canonical_validation(suite, id, directory, validation = NULL,
  replace = FALSE)

validate_canonical_validation_suite(suite)

run_canonical_validation_suite(suite, registry)

canonical_validation_suite_table(result)

canonical_validation_coverage(suite, registry)

write_canonical_validation_suite(result, output_dir)
}
\arguments{
  \item{id}{Stable suite or dataset identifier.}
  \item{title}{Human-readable suite title.}
  \item{fail_fast}{Whether execution stops after the first failure or error.}
  \item{metadata}{Optional named metadata list.}
  \item{suite}{A canonical validation suite.}
  \item{directory}{Materialized canonical dataset directory.}
  \item{validation}{Optional dataset-specific validation function returning a data frame with a logical \code{passed} column.}
  \item{replace}{Whether an existing suite entry may be replaced.}
  \item{registry}{An approved canonical dataset registry.}
  \item{result}{A canonical validation suite result.}
  \item{output_dir}{Evidence output directory.}
}
\value{
Constructors and registration functions return validated suite objects. Execution returns a \code{PopgenVCFCanonicalValidationSuiteResult}. Summary functions return deterministic data frames. The evidence writer returns normalized output paths.
}
