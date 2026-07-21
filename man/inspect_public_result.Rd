\name{inspect_public_result}
\alias{inspect_public_result}
\title{Inspect a Canonical Public Scientific Result}
\usage{
inspect_public_result(request, result)
}
\arguments{
  \item{request}{A canonical public API request for \code{result.inspect}.}
  \item{result}{A canonical core or ancestry result object.}
}
\value{A validated \code{PopgenVCFPublicAPIResponse}.}
\description{
Validate an existing canonical scientific result and translate its stable
scientific content into the Phase 10 public API.
}
\details{
Validation and primary-table extraction are delegated to the existing result
contracts. The response contains stable scientific content, validation records,
artifact identities, and a deterministic result identity. Parameters, raw
provenance, metadata, filesystem paths, software details, random seeds, runtime
measurements, and mutable execution fields are excluded.
}
