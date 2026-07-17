# Submission companion documents

Submission companion documents are generated from a validated `PopgenVCFManuscript` plus explicit author inputs. The implementation deliberately separates canonical manuscript content from journal-facing companion material.

## Objects

`new_submission_companions()` creates an immutable `PopgenVCFSubmissionCompanions` record containing:

- manuscript and publication identities;
- target journal and optional editor;
- author-supplied significance and novelty statements;
- ordered highlights;
- suggested and opposed reviewer metadata;
- named author confirmations;
- canonical manuscript declarations;
- highlight limits and a SHA256 object digest.

The function never infers novelty, significance, reviewer conflicts, or confirmations. Missing content remains visible as a placeholder.

## Completeness

`validate_submission_companions(strict = FALSE)` verifies structure, digest identity, and highlight limits while permitting placeholders. Strict mode additionally requires the target journal, significance statement, novelty statement, at least one highlight, and a corresponding author.

## Written layout

`write_submission_companions()` writes:

- `cover-letter.md`;
- `highlights.md`;
- `author-declarations.md`;
- `companions-record.json`;
- `companions-manifest.tsv`.

The record exposes semantic submission roles. The manifest records file size and SHA256 identity. Directory validation detects missing or modified files.

## Scientific boundary

Companion documents organize author-provided metadata and existing declarations. They do not modify the canonical manuscript, scientific results, artifacts, citations, rendered documents, or JATS XML.