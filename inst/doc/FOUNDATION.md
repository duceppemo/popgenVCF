# popgenVCF 0.5.0 foundation

popgenVCF is an installable R package and command-line population-genomics toolkit for VCF data.

## Stable foundation contracts

- Package and API identity: `popgenVCF`.
- Canonical state class: `PopgenVCFAnalysis`.
- Configuration schema: `1.0`.
- Analysis-object schema: `1.0`.
- Exact SNPRelate filtering contract: MAF from configuration, missing rate 0.2, correlation threshold `sqrt(0.2)`, infinite base-pair span, 50-SNP window, `start.pos = "first"`, and at most four LD-pruning threads.
- Dependency-aware module registry for statistical analyses.
- Canonical machine-readable TSV outputs and serialized analysis state.
- CLI launcher: `popgenVCF.R` or the installed script returned by `system.file("scripts", "popgenVCF", package = "popgenVCF")`.

New analyses must register a unique module name, declare dependencies, provide deterministic tests, document output schemas, and preserve the core QC contract.
