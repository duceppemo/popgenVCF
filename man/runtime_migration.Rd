\name{runtime_migration}
\alias{new_runtime_migration_registry}
\alias{register_runtime_migration}
\alias{runtime_migration_path}
\alias{migrate_runtime_integrity_envelope}
\title{Explicit deterministic runtime schema migration}
\description{
Creates migration registries, registers adjacent schema transformations,
resolves contiguous migration paths, and explicitly migrates integrity-protected
runtime envelopes while producing deterministic audit records.
}
\usage{
new_runtime_migration_registry()

register_runtime_migration(
  registry,
  kind,
  from_version,
  to_version,
  migrate,
  id
)

runtime_migration_path(
  registry,
  kind,
  from_version,
  to_version = unname(runtime_schema_versions()[[kind]])
)

migrate_runtime_integrity_envelope(envelope, registry)
}
\arguments{
\item{registry}{A \code{PopgenVCFRuntimeMigrationRegistry}.}
\item{kind}{A registered runtime artifact kind.}
\item{from_version}{The positive integer source schema version.}
\item{to_version}{The positive integer target schema version.}
\item{migrate}{A deterministic function transforming one payload version into the next.}
\item{id}{A stable, non-empty migration step identifier.}
\item{envelope}{A current or explicitly supported legacy runtime integrity envelope.}
}
\value{
\code{new_runtime_migration_registry()} and
\code{register_runtime_migration()} return migration registries.
\code{runtime_migration_path()} returns an ordered list of migration entries.
\code{migrate_runtime_integrity_envelope()} returns a
\code{PopgenVCFRuntimeMigrationResult} containing the migrated current envelope
and its canonical migration record.
}
\details{
Migrations are explicit and fail closed. Only adjacent registered steps are
accepted. Source envelopes are integrity-validated before transformation, each
step is executed twice to detect nondeterminism, and the final payload is placed
inside a new envelope using the current schema. Ordinary artifact readers remain
unchanged and continue to reject legacy schemas unless migration is invoked
explicitly.
}
