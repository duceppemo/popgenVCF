# Canonical real-data validation

Phase 0.9.18 introduces an explicit, licensed, checksum-pinned workflow for canonical real datasets and external numerical comparisons.

## Design rules

Canonical datasets remain outside the source package. A descriptor records a stable identifier, version, title, license, citation, organism, supported analyses, file names, optional source locations, expected sizes, and mandatory SHA-256 checksums.

Normal package tests never download canonical data. Materialization requires either an explicit local mirror or `allow_download = TRUE`. Every file is verified before installation, and incomplete, corrupt, unlicensed, or unpinned descriptors fail closed.

## Offline materialization

```r
files <- data.frame(
  filename = "canonical.vcf.gz",
  sha256 = "<64-character-sha256>",
  size_bytes = 123456789
)

dataset <- new_canonical_dataset(
  id = "canonical_population_panel",
  version = "1",
  title = "Canonical population panel",
  license = "CC-BY-4.0",
  citation = "Dataset authors (year). Dataset title. DOI.",
  files = files,
  organism = "Organism name",
  analyses = c("qc", "pca", "ibs", "diversity", "fst")
)

materialize_canonical_dataset(
  dataset,
  destination = "validation-data/canonical_population_panel/1",
  source_dir = "/path/to/licensed/local-mirror"
)
```

## External comparisons

External results are aligned by explicit identifiers rather than row order. Numeric values are compared with declared absolute tolerances, and every row records the external tool and version. Missing identifiers are reported as `missing`; values outside tolerance are `fail`; values within tolerance are `pass`.

```r
comparison <- compare_external_results(
  observed = popgenvcf_results,
  reference = plink2_results,
  id_cols = c("sample_id"),
  value_cols = c("PC1", "PC2"),
  tolerance = c(PC1 = 1e-6, PC2 = 1e-6),
  tool = "PLINK 2",
  tool_version = "2.00"
)
```

`write_canonical_validation_evidence()` writes deterministic descriptor, verification, comparison, and methods records suitable for release evidence and archival.

## CI policy

Tiny synthetic fixtures exercise descriptor validation, checksum verification, offline materialization, identifier alignment, tolerance handling, and evidence generation on every pull request. Licensed real-data execution remains opt-in and belongs in a dedicated full-validation workflow so ordinary CI remains fast and network-independent.
