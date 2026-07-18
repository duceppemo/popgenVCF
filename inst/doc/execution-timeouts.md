# Deterministic execution timeouts

Phase 8.5 adds explicit elapsed-time budgets to the unified analysis execution engine.

## Safety model

Timeouts are disabled by default. A caller must provide a `PopgenVCFExecutionTimeoutPolicy` with either a finite default budget or named per-module overrides. The policy wraps registered module runners while leaving the normal execution engine responsible for dependency ordering, output validation, artifact validation, failure propagation, and retry decisions.

A module that exceeds its budget:

1. is interrupted with a stable timeout error;
2. contributes no analysis output or artifact;
3. is treated as failed while dependency propagation and retry classification run;
4. is recorded as `timed_out` in the returned final and attempt ledgers;
5. blocks dependent modules unless an explicit retry policy later obtains a validated success.

Timeouts are elapsed-time limits implemented through R's execution time-limit mechanism. They are reliable for R code and interruptible native routines. External programs should additionally enforce their own process-level timeouts because not every native or external call can be interrupted immediately by the R interpreter.

## Example

```r
policy <- new_execution_timeout_policy(
  default_seconds = Inf,
  module_seconds = c(admixture = 3600, tree = 1800),
  label = "production-budgets-v1"
)

result <- execute_analysis_registry_with_timeouts(
  analysis,
  context,
  registry,
  timeout_policy = policy
)
```

Timeouts can participate in bounded recovery:

```r
retry <- new_execution_retry_policy(
  max_attempts = 2,
  retryable = function(module, error_message, attempt, ledger) {
    grepl("Execution timeout", error_message, fixed = TRUE)
  },
  label = "retry-one-timeout"
)
```

Successful prerequisites are preserved between attempts. Failed or timed-out outputs are never reused.

## Recorded metadata

The execution-engine metadata records:

- the stable timeout-policy label;
- the default elapsed-time budget;
- named per-module overrides;
- modules whose final state is `timed_out`.

The attempt ledger preserves timeout events even when a later retry succeeds, providing an auditable recovery history.
