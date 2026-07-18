\name{scientific_release_bundles}
\alias{new_scientific_release_bundle}
\alias{scientific_release_bundle_table}
\alias{validate_scientific_release_bundle}
\alias{render_scientific_release_bundle}
\alias{write_scientific_release_bundle}
\title{Deterministic scientific release bundles}
\usage{
new_scientific_release_bundle(release_id, package_version, git_commit, git_tag,
  release_date, digests, dependencies = NULL, package_name = "popgenVCF",
  r_version = R.version.string, platform = R.version$platform)

scientific_release_bundle_table(x)

validate_scientific_release_bundle(x)

render_scientific_release_bundle(x)

write_scientific_release_bundle(x, path, overwrite = FALSE)
}
\arguments{
\item{release_id}{Stable release identifier.}
\item{package_version}{Package version.}
\item{git_commit}{Full 40-character Git commit SHA.}
\item{git_tag}{Git release tag.}
\item{release_date}{ISO-8601 release date.}
\item{digests}{Named SHA256 values for every required reproducibility component.}
\item{dependencies}{Named dependency versions or a package/version data frame.}
\item{package_name}{Package name.}
\item{r_version}{R version string.}
\item{platform}{Platform string.}
\item{x}{A scientific release object or written release directory.}
\item{path}{Output directory.}
\item{overwrite}{Whether an existing output directory may be replaced.}
}
\value{The constructor returns a validated \code{PopgenVCFScientificRelease}; the table function returns the canonical digest chain; the renderer returns Markdown lines; the writer returns the normalized path invisibly; validation returns \code{TRUE} invisibly.}
\description{Captures an immutable scientific release identity, software environment, dependency versions, complete reproducibility digest chain, and deterministic checksum manifest.}
