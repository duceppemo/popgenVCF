# Manuscript regeneration verification records

Phase 7.3.15 adds immutable records for explicit human review decisions associated with completed manuscript regeneration outputs.

## Boundary

Verification records are audit records. They do not assess scientific correctness, rewrite manuscript prose, infer reviewer intent, or alter canonical analysis results.

## Contract

`new_manuscript_regeneration_verification()` binds a verification record to the digest of a validated `PopgenVCFRegenerationExecution`. Each completed execution action with an output identity must have exactly one review row containing:

- `section_id`;
- `decision`: `accepted`, `rejected`, or `manual_review`;
- `reviewer_id`;
- an immutable `evidence_identity`;
- an optional author-supplied `note`.

Accepted decisions require an evidence identity. Reviews are sorted by section identity before hashing.

## Validation

When the source execution is supplied, validation rejects unknown, unverifiable, duplicate, or missing sections and verifies that the record references the exact execution digest. Strict validation requires every decision to be `accepted`.

## Deterministic bundle

`write_manuscript_regeneration_verification()` writes JSON, Markdown, TSV, and a checksum manifest. Directory validation detects missing or modified files, and overwrite protection is enabled by default.