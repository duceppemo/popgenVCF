# Publication companion architecture

The publication companion layer transforms a completed `PopgenVCFProject` into a deterministic submission-support directory without rerunning analyses.

## Contracts

`PopgenVCFPublicationBundle` stores the project identity, style profile, methods source, software table, parameter table, module records, immutable artifact records, captions, and a project digest. It is a presentation plan, not a replacement for canonical scientific result objects.

`publication_style()` provides conservative label and citation profiles for generic, Nature, Molecular Ecology, G3, BMC, and PLOS outputs. These profiles do not claim full compliance with publisher Word, LaTeX, or submission-system templates.

## Output layout

`generate_publication_bundle()` writes:

- `manuscript/methods.md`
- `manuscript/software.tsv`
- `manuscript/parameters.tsv`
- `manuscript/modules.tsv`
- `manuscript/captions.tsv`
- `figures/`, `tables/`, and `supplementary/`
- `provenance/artifacts.tsv`
- `provenance/publication.json`
- optional `FAIR/`
- optional `supplementary/analysis.popgenvcf`
- `publication-bundle.rds`
- `publication-manifest.tsv`

Only existing files declared by immutable artifact lineage or the canonical artifact manifest are copied. Missing source paths remain represented in the provenance table but are not fabricated.

## Integrity

The manifest is written last and contains relative paths, sizes, and SHA256 digests. Validation checks file presence and checksums before accepting a bundle. The complete publication plan remains serialized in RDS, while JSON-facing records contain plain values only.

## Future extensions

Later Phase 7 units may add citation discovery, rendered supplementary documents, richer analysis-specific methods and captions, spreadsheet compilation, and journal template adapters. Those features should extend this contract rather than bypass artifact lineage or project identity.
