\name{regression_reporting}
\alias{compare_release_benchmarks}
\alias{release_comparison_table}
\alias{latest_release_benchmark}
\alias{write_regression_report}
\title{Compare archived releases and generate regression reports}
\usage{
compare_release_benchmarks(current, baseline)

release_comparison_table(x)

latest_release_benchmark(archive, exclude = character())

write_regression_report(
  archive,
  output_dir,
  comparison = NULL,
  render = TRUE,
  title = "popgenVCF scientific regression report"
)
}
\arguments{
  \item{current, baseline}{Validated \code{PopgenVCFReleaseBenchmarkRecord} objects.}
  \item{x}{A \code{PopgenVCFReleaseComparison}.}
  \item{archive}{A \code{PopgenVCFBenchmarkArchive}, or for report writing an archive directory.}
  \item{exclude}{Release identifiers excluded from latest-release selection.}
  \item{output_dir}{Destination directory for source tables, report data, QMD, and optional HTML.}
  \item{comparison}{Optional release comparison.}
  \item{render}{Whether to render HTML when Quarto is installed.}
  \item{title}{Report title.}
}
\value{
\code{compare_release_benchmarks()} returns a \code{PopgenVCFReleaseComparison}.
\code{release_comparison_table()} returns a data table.
\code{latest_release_benchmark()} returns a release record.
\code{write_regression_report()} returns a named list of generated paths.
}
\description{
Compare immutable scientific benchmark records, select historical baselines, and
produce deterministic source-only or Quarto-rendered scientific regression reports.
Performance components use baseline-aware resource comparisons when machine
fingerprints are compatible; other components retain transparent digest-change
records rather than claiming numerical equivalence.
}
