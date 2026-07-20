# Runtime recovery equivalence

Phase 8.10.12 defines a fail-closed comparison between an uninterrupted execution and an execution resumed from a validated checkpoint.

## Scientific contract

A recovered execution is equivalent only when all of the following canonical components are identical:

- scientific analysis state and module results;
- execution context;
- module order and execution-plan structure;
- artifact manifest;
- terminal module outcomes.

The verifier computes a SHA-256 digest for each component and a final recovery fingerprint from the ordered component digests. Any divergent component aborts verification and is named in the error.

## Recovery-only metadata

Checkpoint reuse creates bookkeeping that is expected to differ from an uninterrupted run. The equivalence projection therefore excludes:

- `analysis$results$execution_engine`;
- `analysis$results$execution_ledger`;
- the execution-ledger `checkpoint_reused` column;
- timing and other non-scientific engine telemetry.

These exclusions are narrow and explicit. Scientific results, context, artifacts, plan identity, module order, and terminal statuses remain protected.

## Fail-closed validation

Verification rejects executions that contain:

- missing required execution components;
- duplicate, missing, or reordered modules;
- a module order inconsistent with the execution plan;
- nonterminal runtime states;
- scientific result, context, artifact, plan, or terminal-outcome drift.

## Usage

```r
reference <- execute_analysis_registry(analysis, context, registry)
checkpoint <- new_execution_checkpoint(interrupted, registry)
recovered <- resume_analysis_execution(checkpoint, registry)

verification <- verify_runtime_recovery_equivalence(reference, recovered)
verification$verified
verification$recovery_fingerprint
```

The reference execution must be produced from the same scientific inputs and module contracts as the recovered execution.