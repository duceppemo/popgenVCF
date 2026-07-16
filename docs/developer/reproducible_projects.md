# Reproducible analysis projects

`PopgenVCFProject` is the root persistence object for a completed analysis. It is
separate from the execution registry: execution produces canonical results;
the project layer captures them with the identities needed to reopen, audit,
and compare the analysis later.

## Contract

A project records:

- project UUID, name, creation time, package version, and Git SHA;
- R, platform, operating-system, locale, and RNG identity;
- checksummed input identities;
- parameters and module execution records;
- canonical result objects;
- artifact, report, and provenance records;
- component digests used for integrity checking and project diffs.

Result objects are embedded in `project.rds`. Input files are not copied in this
foundation phase; their normalized paths, sizes, and SHA256 identities are
recorded. A later project-materialization layer may optionally copy or link
inputs under a declared data-governance policy.

## Portable bundles

```r
project <- new_popgenvcf_project(
  "study-2026",
  results = analysis$results,
  inputs = c(vcf = "cohort.vcf.gz", metadata = "metadata.tsv"),
  parameters = config,
  modules = analysis$capabilities,
  artifacts = analysis$artifact_manifest,
  rng = new_project_rng(seed = 2026L)
)

write_popgenvcf_project(project, "study-2026.popgenvcf")
```

The bundle is a gzip-compressed tar archive containing:

- `project.rds`: complete canonical object;
- `project.json` and `project.tsv`: lightweight identity summaries;
- `inputs.tsv` and `results.tsv`;
- `manifest.tsv`: file sizes and SHA256 checksums.

`read_popgenvcf_project()` verifies the manifest by default and returns the
embedded project without retaining temporary extraction paths.

## Project comparison

```r
diff <- compare_popgenvcf_projects(
  read_popgenvcf_project("revision-2.popgenvcf"),
  read_popgenvcf_project("revision-1.popgenvcf")
)
project_table(diff)
```

The foundation comparison reports changed project/software identity, inputs,
and result digests. Later Phase 6 units will add semantic parameter diffs,
artifact/report diffs, scientific result comparators, and manuscript-ready
change summaries.

## Security and privacy

Bundles may contain metadata and scientific results. They must not be published
without reviewing consent, sample identifiers, geographic coordinates, and
other controlled information. Input paths can also disclose local directory
structure; a later redaction policy will support portable aliases.

## Planned Phase 6 extensions

1. provenance DAG and dependency lineage;
2. deterministic parallel RNG streams;
3. optional input materialization and redaction;
4. RO-Crate, DataCite, CodeMeta, ORCID, and CITATION exports;
5. manuscript companion and supplementary-data packages.
