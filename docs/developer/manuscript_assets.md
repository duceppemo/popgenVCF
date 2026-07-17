# Manuscript cross-references and embedded assets

Phase 7.3.1 extends the canonical manuscript source directory with portable assets, stable anchors, and bibliography preservation.

## Cross-reference contract

`manuscript_cross_reference_table()` derives one deterministic record per immutable artifact. Each record contains:

- the immutable artifact identifier;
- artifact category;
- publication label;
- stable Markdown anchor;
- manuscript-relative path;
- caption text;
- whether the artifact can be embedded directly in Markdown.

Anchors are derived only from the category and immutable artifact identifier. Reordering files or changing local source paths therefore does not change reference identities.

## Asset copying

`write_manuscript()` copies available artifact files into:

- `assets/figures/`;
- `assets/tables/`;
- `assets/supplementary/`.

Destination names contain a short SHA256 prefix derived from the immutable artifact identifier. This avoids collisions while keeping filenames recognizable.

Supported raster and vector image formats are embedded with Markdown image syntax. PDFs and other figures, tables, and supplementary artifacts are linked rather than embedded.

## Bibliography preservation

When the manuscript contains BibTeX text, `write_manuscript()` writes `references.bib` and points to it from the References section. This phase preserves canonical bibliography source but does not yet apply CSL or journal-specific citation rendering.

## Generated directory

A written manuscript may contain:

```text
manuscript.md
manuscript.rds
authors.tsv
captions.tsv
cross-references.tsv
references.bib
assets/
  figures/
  tables/
  supplementary/
manuscript-manifest.tsv
```

All generated and copied files except the manifest itself are covered by SHA256 checksums. `validate_manuscript()` detects missing, modified, or replaced assets.

## Scientific boundary

Artifact embedding and cross-reference generation are presentation and traceability operations. They do not alter canonical results, captions, author-supplied interpretation, or scientific conclusions.
