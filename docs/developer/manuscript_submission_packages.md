# Manuscript submission packages

`write_submission_package()` assembles a canonical manuscript directory into a stable, verifiable journal-submission archive.

## Layout

The archive contains a single `submission/` root. Canonical manuscript files retain their manuscript-relative paths, including rendered documents, JATS XML, citation metadata, figures, tables, supplementary files, and provenance records.

Two package-level records are added:

- `submission-manifest.tsv` records each included file's role, destination, size, and SHA256 identity;
- `submission-record.json` records the package schema, generic profile, project identity, manuscript identities, file count, and manifest identity.

## Inclusion policy

All regular files in the validated manuscript directory are included except nested submission-package directories and common operating-system metadata files. The planner assigns stable semantic roles without rewriting source files.

Use `submission_package_plan()` to inspect inclusion decisions before creating an archive.

## Reproducibility

Files are sorted by normalized destination path. Staged timestamps are normalized before the internal R tar implementation creates the `.tar.gz` archive. Package verification extracts the archive and recomputes every recorded SHA256 value.

## Example

```r
plan <- submission_package_plan("analysis/manuscript")
package <- write_submission_package(
  "analysis/manuscript",
  "analysis/submission.tar.gz"
)
verify_submission_package(package$archive)
```

## Scientific boundary

Assembly copies immutable manuscript artifacts. It does not alter scientific results, manuscript prose, citation keys, rendered outputs, JATS XML, or author interpretation.
