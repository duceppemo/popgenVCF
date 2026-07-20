\name{phase10_public_api}
\alias{phase10_api_descriptor}
\alias{phase10_api_operations}
\alias{new_public_analysis_request}
\alias{new_public_analysis_response}
\alias{validate_phase10_api_descriptor}
\alias{validate_public_analysis_request}
\alias{validate_public_analysis_response}
\alias{inspect_public_analysis_response}
\alias{write_public_api_record}
\alias{read_public_api_record}
\alias{phase10_public_fingerprint}
\title{Canonical Phase 10 public analysis and artifact API}
\description{
Defines deterministic, versioned public descriptors, operation discovery,
request and response envelopes, validation, inspection, and exact record
serialization above the unified popgenVCF runtime.
}
\usage{
phase10_api_descriptor(api_version = "1.0.0")

phase10_api_operations(descriptor = phase10_api_descriptor())

new_public_analysis_request(
  operation_id,
  analysis_id,
  parameters = list(),
  input_ids = character(),
  api_version = "1.0.0"
)

new_public_analysis_response(
  request,
  status,
  scientific_values = list(),
  artifact_ids = character(),
  provenance_ids = character(),
  warnings = character(),
  error = NULL
)

validate_phase10_api_descriptor(descriptor)

validate_public_analysis_request(request)

validate_public_analysis_response(response, request = NULL)

inspect_public_analysis_response(response)

write_public_api_record(x, path)

read_public_api_record(path)

phase10_public_fingerprint(x)
}
\arguments{
  \item{api_version}{Public API semantic version.}
  \item{descriptor}{A Phase 10 public API descriptor.}
  \item{operation_id}{Stable public operation identifier.}
  \item{analysis_id}{Stable analysis or request identity.}
  \item{parameters}{Named public parameters.}
  \item{input_ids}{Named immutable input identities.}
  \item{request}{A validated public API request.}
  \item{status}{A supported terminal public response status.}
  \item{scientific_values}{Named scientific values or immutable references.}
  \item{artifact_ids}{Named artifact identities.}
  \item{provenance_ids}{Named provenance identities.}
  \item{warnings}{Character warnings.}
  \item{error}{Optional stable public error record with \code{code} and \code{message}.}
  \item{response}{A validated public API response.}
  \item{x}{A descriptor, request, response, or fingerprint payload.}
  \item{path}{Path to a public API JSON envelope.}
}
\value{
Descriptors, operation tables, validated request or response records, a
one-row response summary, an invisible normalized path, a restored record,
or a lowercase SHA-256 fingerprint, depending on the function.
}
\details{
The public boundary exposes scientific values, immutable artifact identities,
provenance identities, warnings, and stable errors. Mutable executor,
scheduler, worker, retry, process, checkpoint, migration, and deprecation
fields are rejected. Unsupported major versions, unknown operations,
incompatible schemas, record mutation, and response/request identity drift
fail closed.
}
\examples{
descriptor <- phase10_api_descriptor()
phase10_api_operations(descriptor)

request <- new_public_analysis_request(
  "result.inspect",
  "analysis-001",
  input_ids = c(result = "sha256:result")
)

response <- new_public_analysis_response(
  request,
  "completed",
  scientific_values = list(global_fst = 0.12),
  artifact_ids = c(table = "artifact:fst-table"),
  provenance_ids = c(run = "provenance:run-001")
)

inspect_public_analysis_response(response)
}
