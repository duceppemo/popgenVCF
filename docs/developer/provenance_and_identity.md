# Provenance DAG and sample identity architecture

## Provenance graph

`PopgenVCFProvenanceDAG` is a validated directed acyclic graph composed of canonical nodes and edges.

Nodes represent inputs, transformations, analyses, artifacts, or reports. They retain stable identifiers, optional digests, parameters, software metadata, status, and timestamps. Edges record explicit producer/consumer lineage and may identify the artifact transferred between nodes.

Validation rejects duplicate node IDs, duplicate edges, dangling references, self-edges, and cycles. Deterministic topological ordering and ancestor/descendant traversal make the graph suitable for project bundles, report explanations, replay planning, and analysis diffs.

A typical lineage is:

```text
source_vcf -> normalized_vcf -> ld_pruning -> pca -> pca_figure
```

The graph is dependency-free and serializes naturally inside `PopgenVCFProject$provenance`.

## Canonical sample identity

The immutable `sample` column always remains the exact VCF/GDS key. It is never replaced in genotype-facing calls.

Recognized identity columns are:

| Column | Cardinality | Purpose |
|---|---|---|
| `sample` | unique, mandatory | immutable VCF/GDS key |
| `alias` | unique when present | public display name |
| `individual` | reusable | biological individual across samples |
| `family` | reusable | pedigree or family grouping |
| `replicate` | reusable | technical/biological replicate grouping |
| `display_order` | unique when present | deterministic plot/table ordering |

`public_sample` uses `alias` when present and otherwise falls back to `sample`. Public names are globally unique, preventing an alias from colliding with another unaliased VCF identifier.

Existing alias helpers now route through this canonical identity model, so PCA, IBS, DAPC, QC, diversity, reports, dashboards, provenance, and project bundles share the same identity semantics.
