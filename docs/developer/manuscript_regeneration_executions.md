# Manuscript regeneration execution records

Phase 7.3.14 adds immutable records describing explicit actions taken in response to a validated manuscript regeneration plan.

## Boundary

Execution records are audit records, not manuscript editors. They do not generate prose, infer author intent, assess scientific correctness, or automatically resolve blocked scientific decisions.

## Contract

`new_manuscript_regeneration_execution()` links an execution to the immutable digest of a `PopgenVCFRegenerationPlan`. Each affected section has one canonical action row containing:

- `section_id`;
- `action`: `regenerate`, `manual_review`, `resolve_block`, or `no_action`;
- `status`: `pending`, `completed`, `skipped`, or `failed`;
- `executor_id`;
- an optional immutable `output_identity`;
- an optional author-supplied `note`.

Actions are sorted by section identity before hashing. The execution digest is computed from canonical plain-data-frame content so `data.table` reference metadata cannot alter identity.

## Validation

When the source plan is supplied, validation rejects unknown or missing required sections and verifies action compatibility:

| Plan state | Required action |
|---|---|
| `affected` | `regenerate` |
| `manual_review` | `manual_review` |
| `blocked` | `resolve_block` |

Completed regeneration and block-resolution actions require an explicit output identity. Strict validation additionally rejects every action that is not completed.

## Deterministic bundle

`write_manuscript_regeneration_execution()` writes:

- `regeneration-execution.json`;
- `regeneration-execution.md`;
- `regeneration-execution.tsv`;
- `regeneration-execution-manifest.tsv`.

The manifest stores file sizes and SHA256 identities. Directory validation detects missing or modified files, and overwrite protection is enabled by default.
