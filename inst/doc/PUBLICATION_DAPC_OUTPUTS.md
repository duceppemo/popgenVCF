# Publication DAPC outputs

Phase 0.9.6 adds deterministic publication contracts around authoritative discriminant analysis of principal components (DAPC) results. The publication layer does not rerun clustering, cross-validation, or discriminant analysis. It validates, orders, fingerprints, reports, and exports results produced by the existing analysis runtime.

## Core objects

`new_publication_dapc_spec()` defines the sample identifier column, known-population column, inferred-cluster column, discriminant axes, selected K, source-data format, and specification version.

`new_publication_dapc_output()` binds a validated specification to authoritative discriminant coordinates and membership probabilities. Optional diagnostics, cross-validation summaries, confusion data, and publication figure-style bindings are retained in the output record.

Every specification and output carries a deterministic SHA-256 fingerprint. Validation fails closed when a specification is mutated, source-data tables drift from the bound output, sample identities do not match, probabilities are invalid, or selected K disagrees with the membership matrix.

## Minimal example

```r
spec <- new_publication_dapc_spec(
  axis_columns = c("LD1", "LD2"),
  selected_k = 3L
)

coordinates <- data.frame(
  sample = c("sample-1", "sample-2", "sample-3"),
  population = c("north", "north", "south"),
  cluster = c("1", "1", "2"),
  LD1 = c(-1.2, -0.7, 1.9),
  LD2 = c(0.4, -0.2, 0.1)
)

membership <- matrix(
  c(0.90, 0.08, 0.02,
    0.80, 0.15, 0.05,
    0.05, 0.20, 0.75),
  nrow = 3L,
  byrow = TRUE,
  dimnames = list(coordinates$sample, c("cluster_1", "cluster_2", "cluster_3"))
)

output <- new_publication_dapc_output(
  spec,
  coordinates,
  membership,
  result_fingerprint = "authoritative-dapc-result"
)

publication_dapc_caption(output, spec)
publication_dapc_report(output, spec)
```

## Deterministic ordering and source data

Coordinate rows are ordered by sample identity. Membership rows are then aligned to exactly the same order. The `source_data` member contains machine-readable coordinate, membership, diagnostics, cross-validation, and confusion tables suitable for supplementary material or downstream rendering.

## Figure-style bindings

An optional `PopgenVCFPublicationFigureStyleBinding` may be attached. Validation ensures that the binding has enough group capacity for all membership columns. The binding fingerprint is stored in the DAPC output so later style drift can be detected.

## Scientific boundary

These functions are presentation and reproducibility contracts. They must not be used as an alternative DAPC implementation. Numerical optimization, cross-validation, cluster selection, assignment, and membership estimation remain authoritative in the existing DAPC analysis runtime.
