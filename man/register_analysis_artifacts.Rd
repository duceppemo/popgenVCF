\name{register_analysis_artifacts}
\alias{register_analysis_artifacts}
\title{Declare Required Artifacts for a Registered Analysis}
\usage{
register_analysis_artifacts(
  registry,
  name,
  artifacts,
  must_exist = FALSE
)
}
\arguments{
\item{registry}{A \code{PopgenVCFRegistry} object.}
\item{name}{Name of an already registered module.}
\item{artifacts}{Character vector of required artifact names.}
\item{must_exist}{Require the declared files to exist immediately after the module runs. Use \code{FALSE} when a later publishing stage materializes files.}
}
\value{
The updated registry.
}
\description{
Artifact declarations are opt-in so existing modules remain compatible. Once declared, the registry requires the module runner to return an \code{artifacts} manifest containing each named artifact under the module's own namespace.
}
