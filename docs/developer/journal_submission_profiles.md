# Deterministic journal submission profiles

Journal submission profiles encode explicit manuscript and package requirements without changing scientific content. A profile is immutable: its identity is a SHA256 digest of its metadata, submission roles, filename mappings, structural requirements, limits, source metadata, and explicit overrides.

## Core API

- `new_journal_profile()` creates a deterministic profile.
- `validate_journal_profile()` verifies object identity or a written profile bundle.
- `generic_journal_profile()` preserves the neutral submission-role profile.
- `journal_profile()` returns conservative generic research-article, short-communication, and data-note profiles.
- `apply_journal_profile()` validates a submission plan and applies deterministic naming.
- `validate_journal_submission()` produces an actionable completeness report.
- `render_journal_profile()` and `write_journal_profile()` create deterministic profile documents.

## Named and verified profiles

A named profile should use `status = "verified"` and must include both `source_url` and `source_date`. This makes the provenance and age of encoded requirements visible. Live scraping and silently inferred policies are intentionally excluded.

## Encoded requirements

Profiles can record:

- required and optional manuscript sections;
- required declarations and companion documents;
- title, abstract, keyword, and highlight limits;
- graphical abstract requirements;
- figure, table, and supplementary-file limits;
- allowed figure extensions and filename patterns;
- required and optional submission roles;
- deterministic destination filenames;
- explicit named overrides.

Role and section sets are sorted and deduplicated. Required and optional sets cannot overlap. Filename mappings may only refer to declared roles and cannot create duplicate destinations.

## Completeness reports

`validate_journal_submission()` compares a canonical manuscript and optional companion records with a profile. It returns a deterministically ordered table containing the requirement, pass/fail status, observed value, expected value, and an actionable message.

Permissive validation returns the complete report. Strict validation raises one error listing every failed requirement.

## Profile bundles

`write_journal_profile()` creates an isolated `journal-profile/` directory containing:

- `journal-profile.json`;
- `journal-profile.md`;
- `journal-profile-manifest.tsv`.

The manifest stores file sizes and SHA256 checksums. Validation detects missing or modified files, and overwrite protection prevents accidental replacement.

## Scientific boundary

Profiles validate structure and packaging only. They do not rewrite manuscript text, invent declarations, infer author intent, alter figures or tables, convert bibliographies, or submit content to external portals.
