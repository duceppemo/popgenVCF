\name{PopgenVCFAnalysis}
\alias{PopgenVCFAnalysis}
\alias{new_popgen_vcf_analysis}
\alias{is_popgen_vcf_analysis}
\alias{validate_analysis}
\alias{set_analysis_result}
\alias{get_analysis_result}
\alias{print.PopgenVCFAnalysis}
\alias{summary.PopgenVCFAnalysis}
\title{Canonical popgenVCF analysis state}
\description{Create, validate, inspect, and update the canonical serializable state object used by the popgenVCF pipeline.}
\usage{
new_popgen_vcf_analysis(config, dirs = NULL)

is_popgen_vcf_analysis(x)

validate_analysis(x, stage = NULL)

set_analysis_result(x, name, value)

get_analysis_result(x, name, default = NULL)

\method{print}{PopgenVCFAnalysis}(x, ...)

\method{summary}{PopgenVCFAnalysis}(object, ...)
}
\arguments{
  \item{config}{A validated popgenVCF configuration list.}
  \item{dirs}{Optional output-directory structure.}
  \item{x, object}{A \code{PopgenVCFAnalysis} object.}
  \item{stage}{Optional pipeline stage for stronger invariant checks.}
  \item{name}{One result name.}
  \item{value}{A result to store.}
  \item{default}{Value returned when the named result is absent.}
  \item{...}{Additional arguments reserved for methods.}
}
\value{A \code{PopgenVCFAnalysis} object, logical scalar, stored result, or one-row summary table, depending on the function.}
