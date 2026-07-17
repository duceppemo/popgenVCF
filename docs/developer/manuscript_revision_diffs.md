# Manuscript revision diff reports

Phase 7.3.12 adds immutable manuscript revision records and deterministic section-level comparison reports.

## Revision records

`new_manuscript_revision()` records a stable manuscript ID, revision ID, optional parent revision, optional author-supplied summary, creator identity, and canonical sections. Sections are sorted by stable `section_id` values and include SHA256 content hashes plus character and word counts.

Revision objects are immutable records. Changing section content, titles, metadata, or ordering changes the revision digest.

## Comparison

`compare_manuscript_revisions()` classifies every canonical section as:

- `added`;
- `removed`;
- `modified`;
- `unchanged`.

The report records before and after content hashes, character counts, word counts, and count deltas. Optional annotations may provide an explicit author explanation and reviewer-comment identifiers for a section.

Strict validation fails when a changed section has no explicit author-supplied explanation. The package never infers why text changed or whether a reviewer concern was resolved.

## Deterministic bundles

`write_manuscript_revision_diff()` writes:

- `revision-diff.json`;
- `revision-diff.tsv`;
- `revision-diff.md`;
- `revision-diff-manifest.tsv`.

`validate_manuscript_revision_diff_bundle()` verifies required files and every recorded SHA256 checksum. Existing bundles are protected unless `overwrite = TRUE` is supplied.

## Scientific boundary

Revision diff reports identify textual and structural changes only. They do not evaluate scientific correctness, infer causal explanations, rewrite prose, or claim reviewer compliance.