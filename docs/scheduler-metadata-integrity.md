# Scheduler metadata integrity

Scheduler metadata records the deterministic decisions that connect an execution plan to its observed runtime: dependency waves, batches, dispatch order, completion order, merge order, and worker assignments.

## Canonical object

`new_scheduler_metadata()` creates a `PopgenVCFSchedulerMetadata` data table. Each module must occur exactly once. Waves and batches are positive integers, dependency references must resolve to modules in the same record, and dependencies must occur in earlier waves. Dispatch, completion, and merge sequences are unique, positive, and contiguous from one whenever present.

## Persistence boundary

`write_scheduler_metadata()` stores the canonical object inside a versioned `scheduler_metadata` runtime integrity envelope. The RDS file is installed atomically and accompanied by a whole-file SHA-256 sidecar.

`read_scheduler_metadata()` validates, in order:

1. metadata and sidecar presence;
2. sidecar structure;
3. whole-file SHA-256 integrity;
4. RDS readability;
5. runtime-envelope type and artifact kind;
6. runtime schema compatibility;
7. envelope payload digest;
8. scheduler metadata invariants.

No scheduler provenance is accepted merely because it can be deserialized.

## Compatibility policy

Unsupported future schemas fail closed. Legacy unwrapped metadata also fails closed until an explicit ordered migration is registered. The reader returns the validated canonical data table without silently repairing malformed ordering or dependency records.

## Determinism

Equivalent scheduler metadata produces byte-identical persisted RDS files. Replay tests cover payload mutation, file corruption, malformed sidecars, unsupported future schemas, and ordering violations.
