# Runtime state transitions

Phase 8.10.11 defines a fail-closed runtime state machine and an explicit migration-to-replay assembly boundary.

## Canonical states

The runtime recognizes `pending`, `running`, `success`, `failed`, `blocked`, `cancelled`, and `skipped`.

`success`, `cancelled`, and `skipped` are terminal. Once observed, they may only remain unchanged. This prevents completed or abandoned scientific work from silently returning to an executable state.

`failed` and `blocked` may enter a retry or resumed path. Persisted retry ledgers record attempt outcomes rather than every transient scheduler state, so a later observed success, failure, cancellation, skip, pending, running, or blocked state can follow them. All other transitions are represented explicitly in `runtime_state_transition_matrix()` and are rejected unless that matrix permits them.

## Validation

`validate_runtime_state_transition()` validates a single edge. `validate_runtime_state_history()` validates every adjacent edge in an ordered module history and includes the module identity in stable diagnostics.

The test suite iterates over the complete state matrix, proving that every allowed edge succeeds and every forbidden edge fails closed. It also covers retry histories and terminal-state regressions.

## Migration-to-replay boundary

`assemble_runtime_replay_from_envelopes()` is the only Phase 8.10.11 path that accepts legacy runtime envelopes for replay assembly. It:

1. migrates every envelope through the explicit migration registry;
2. validates every resulting envelope against the current runtime schema;
3. requires exactly one execution ledger and at most one attempt ledger;
4. rejects unsupported kinds and duplicate singleton artifacts;
5. extracts payloads only after current-schema integrity validation;
6. invokes cross-artifact replay verification;
7. sorts migration records canonically; and
8. produces an assembly fingerprint covering both replay and migration fingerprints.

Partial migration fails closed. Mixed current and legacy inputs are acceptable only when every legacy component has a complete registered path and every resulting current envelope passes integrity and semantic validation.

## Scientific safety

A replay assembly is accepted only when it represents one internally coherent execution history. Individually valid artifacts are insufficient when their state transitions are impossible, terminal outcomes regress, migrations are incomplete, singleton artifacts are duplicated, or envelope payloads have changed after digest construction.
