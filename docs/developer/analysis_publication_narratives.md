# Analysis-specific publication narratives

Phase 7.2 extends the publication companion with scientific text derived from canonical project results.

## Contract

`publication_analysis_narratives(project)` produces one deterministic row per embedded result with:

- analysis identifier;
- recognized scientific kind;
- methods text;
- figure legend;
- citation keys.

The engine supports PCA, IBS/MDS, neighbour-joining trees, diversity, FST, AMOVA, DAPC, isolation by distance, and ancestry results. Unknown canonical result types receive a conservative generic narrative.

## Scientific boundaries

- IBS-derived distance is never described as FST.
- FST is described as a population-level estimator.
- DAPC is not described as ancestry or admixture.
- Q-matrix ancestry language is reserved for ancestry backend results.
- IBD methods are emitted only when an IBD result exists.
- Missing optional result fields are omitted rather than inferred.

## Publication bundle integration

`generate_publication_bundle()` now writes:

- `manuscript/analysis-narratives.tsv`;
- module-specific sections in `manuscript/methods.md`;
- analysis-aware captions where artifact identifiers can be matched;
- `manuscript/references.bib` containing only citations required by present analyses;
- analysis identity and citation keys in `provenance/publication.json`.

All generated files are covered by the existing SHA256 publication manifest.
