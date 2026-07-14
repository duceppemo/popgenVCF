\name{module_validation}
\alias{module_validation}
\alias{validate_module_result}
\title{Validate an analysis-module result}
\description{Create or apply the validation contract used before registry module results are committed to an analysis state.}
\usage{
validate_module_result(result, analysis = NULL, context = NULL)
}
\arguments{
  \item{result}{Module result object.}
  \item{analysis}{Current \code{PopgenVCFAnalysis} state.}
  \item{context}{Runtime module context.}
}
\value{A \code{PopgenVCFValidation} object.}
