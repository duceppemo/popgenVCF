# Public API Contract

Phase 0.9.15 makes the user-facing R interface an explicit release contract derived from the installed package namespace.

## Contract contents

The deterministic snapshot records:

- every exported symbol;
- every registered S3 method;
- the ordered formal arguments of exported functions;
- whether each argument is required or optional;
- the deparsed default expression for optional arguments.

The snapshot is release evidence. `NAMESPACE`, package code, and generated documentation remain the sources of truth.

## Compatibility policy

The checker treats the following changes as blocking:

- removal of an exported symbol;
- removal of a registered S3 method;
- removal or incompatible reordering of required arguments;
- addition of a required argument;
- change to an existing argument default.

New exports and new optional arguments are advisory findings. They require review and an intentional baseline refresh, but they are not inherently backward incompatible.

## Evidence

`write_public_api_contract()` writes deterministic TSV files:

- `public-api-current.tsv`;
- `public-api-findings.tsv`;
- `public-api-summary.tsv`.

The command-line entrypoint is:

```bash
Rscript tools/check-public-api-contract.R . artifacts/public-api-contract inst/api-contract/public-api-baseline.tsv
```

A blocking finding causes a non-zero exit. The committed baseline must be refreshed only in a reviewed change that intentionally accepts the new public contract.
