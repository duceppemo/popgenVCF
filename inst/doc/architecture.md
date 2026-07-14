# popgenVCF architecture

# Design goals

`popgenVCF` separates input validation, quality control, statistical analyses,
plotting, and reporting. The YAML configuration is the canonical record of a
run. Machine-readable TSV files are canonical outputs; figures and reports are
views derived from those data.

# SNP sets

The QC-passing set is used for diversity and FST. The LD-pruned set is used for
PCA, IBS, MDS, trees, DAPC, and chromosome-specific ordination. The fixed
SNPRelate pruning settings are MAF from configuration, missing rate 0.2,
correlation threshold `sqrt(0.2)`, unlimited base-pair span, 50-SNP window,
`start.pos = "first"`, and at most four threads.

## Canonical analysis state (v0.5.0)

The pipeline is orchestrated through a `PopgenVCFAnalysis` S3 object. Modules remain independently callable, but the pipeline records their validated outputs in a single state with the following stable top-level sections:

- `config`: validated YAML configuration;
- `inputs`: metadata, GDS cache path, and GDS identifiers;
- `samples`: harmonized sample IDs, metadata, QC, and participation;
- `variants`: variant audit, QC-passing IDs, LD-pruned IDs, and QC reports;
- `results`: named module outputs;
- `timings`: elapsed seconds by stage;
- `messages`: structured stage messages;
- `status`, `started_at`, and `completed_at`: lifecycle provenance.

The object is validated at module boundaries. In particular, metadata order must match sample order and the LD-pruned SNP set must remain a subset of the independently audited QC set. Large matrices are written to tabular output files and referenced from the state rather than duplicated in the compressed RDS file.
