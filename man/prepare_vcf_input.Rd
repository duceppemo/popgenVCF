\name{prepare_vcf_input}
\alias{prepare_vcf_input}
\title{Prepare and Index a VCF Input}
\usage{
prepare_vcf_input(vcf, cache_dir, force = FALSE)
}
\arguments{
\item{vcf}{Path to a \code{.vcf} or \code{.vcf.gz} file.}
\item{cache_dir}{Directory used for normalized cached inputs.}
\item{force}{Recreate the normalized cached copy and index.}
}
\value{
A list containing \code{path}, \code{index}, \code{source}, and \code{normalized}.
}
\description{
Accepts plain VCF and compressed VCF input. Existing indexed BGZF files are
reused. Missing indexes are created when possible. Plain VCF, ordinary gzip,
read-only compressed input, and files that cannot be indexed directly are
sorted and converted to an indexed BGZF copy using bcftools.
}
