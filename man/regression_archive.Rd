\name{regression_archive}
\alias{new_release_benchmark_record}
\alias{validate_release_benchmark_record}
\alias{new_benchmark_archive}
\alias{register_release_benchmark}
\alias{get_release_benchmark}
\alias{benchmark_archive_table}
\alias{write_benchmark_archive}
\alias{read_benchmark_archive}
\alias{verify_benchmark_archive}
\title{Scientific Regression Archive}
\description{
Create, validate, serialize, retrieve, and verify append-only scientific benchmark records for tagged popgenVCF releases.
}
\usage{
new_release_benchmark_record(
  release,
  package_version,
  git_sha,
  components,
  provenance = list(),
  environment = list(),
  datasets = list(),
  parameters = list(),
  container_digest = NA_character_,
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

validate_release_benchmark_record(x)

new_benchmark_archive(records = list(), metadata = list())

register_release_benchmark(archive, record)

get_release_benchmark(archive, release)

benchmark_archive_table(x)

write_benchmark_archive(archive, path, overwrite = FALSE)

read_benchmark_archive(path, verify = TRUE)

verify_benchmark_archive(path)
}
\arguments{
  \item{release}{Stable release or tag identifier.}
  \item{package_version}{Package version represented by the release.}
  \item{git_sha}{Git commit SHA.}
  \item{components}{Named list of benchmark, validation, external-reference, performance, or other scientific result objects.}
  \item{provenance, environment, datasets, parameters}{Named metadata lists retained with the release.}
  \item{container_digest}{Optional container image digest.}
  \item{created_at}{UTC creation timestamp.}
  \item{x}{A release benchmark record or benchmark archive.}
  \item{records}{Optional list of release records.}
  \item{metadata}{Named archive metadata.}
  \item{archive}{A \code{PopgenVCFBenchmarkArchive}.}
  \item{record}{A \code{PopgenVCFReleaseBenchmarkRecord}.}
  \item{path}{Archive directory.}
  \item{overwrite}{Whether an existing release directory may be replaced. The default preserves append-only behavior.}
  \item{verify}{Whether manifest checksums should be verified while reading.}
}
\value{
Constructors return validated canonical objects. Table conversion returns a data table. Writing returns the normalized archive directory invisibly. Reading returns a validated archive. Verification returns \code{TRUE} invisibly.
}
\details{
Each release stores its complete component objects in RDS format, normalized component summaries in TSV format, provenance and environment metadata in JSON format, and SHA256 checksums in an archive manifest. Duplicate release identifiers are rejected by default. Manifest verification detects missing or modified archived files.
}
\examples{
record <- new_release_benchmark_record(
  release = "v0.10.0",
  package_version = "0.10.0",
  git_sha = paste(rep("a", 40), collapse = ""),
  components = list(validation = data.frame(check = "pca", passed = TRUE)),
  created_at = "2026-07-15 UTC"
)
archive <- new_benchmark_archive(list(record))
benchmark_archive_table(archive)
}
