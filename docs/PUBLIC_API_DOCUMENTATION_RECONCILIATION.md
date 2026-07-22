# Public API and generated documentation reconciliation

## Purpose

The package namespace and Rd files are generated interfaces. Their authoritative inputs are the roxygen declarations in `R/`, not runtime namespace mutation or hand-maintained compatibility exports.

Phase 0.9.30 establishes one deterministic contract:

- every intended ordinary public function carries roxygen documentation and `@export`;
- every public S3 method carries roxygen documentation and is represented by `S3method()`;
- superseded compatibility definitions are internal and do not claim public export ownership;
- generated `NAMESPACE` and `man/*.Rd` files must be committed exactly as produced by roxygen;
- runtime `namespaceExport()` calls remain prohibited;
- the canonical public API baseline is refreshed only after intentional interface changes are reviewed.

## Reconciliation procedure

1. Inventory every roxygen `@export` declaration.
2. Match each declaration to either an ordinary namespace export or an S3 registration.
3. Require one roxygen owner for each public symbol.
4. Regenerate namespace and Rd files with:

   ```r
   roxygen2::roxygenise(".", roclets = c("namespace", "rd"))
   ```

5. Fail CI when regeneration changes tracked generated files.
6. Install the generated package and refresh the canonical API snapshot from the installed namespace.
7. Run package checks, pkgdown, and the public API compatibility workflow.

## CI enforcement

The `Generated documentation` workflow runs roxygen from a clean checkout and then requires:

```bash
git diff --exit-code -- NAMESPACE man
```

The release/API reconciliation separately blocks:

- documented exports without Rd aliases;
- duplicate namespace exports;
- runtime namespace mutation;
- undocumented S3 registrations;
- roxygen declarations absent from both `export()` and `S3method()`;
- release-version drift.

This separation is deliberate: roxygen verifies reproducible generation, while release reconciliation verifies semantic completeness of the resulting interface.

## Public API baseline

The canonical API snapshot under `inst/api-contract/` records the installed public interface, including arguments and defaults. It is regenerated only after the namespace and documentation reconciliation is complete. Intentional additions are reviewed and encoded in the baseline; removals, required-argument changes, and default changes remain fail-closed compatibility events.
