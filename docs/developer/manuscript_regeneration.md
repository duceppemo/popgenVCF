# Manuscript regeneration plans

`PopgenVCFRegenerationPlan` records which manuscript sections are affected by explicit changes to canonical inputs.

## Inputs

A plan requires two deterministic tables:

- `dependencies`: section-to-input or section-to-section mappings with a policy of `regenerate`, `manual_review`, or `blocked`;
- `changes`: changed input identifiers with before and after identities and a change category.

Section dependencies must form an acyclic graph. Duplicate mappings, unknown section references, and unknown changed inputs are rejected.

## Impact states

The planner assigns one state to every known section:

- `unaffected`: no changed dependency reaches the section;
- `affected`: regeneration is allowed by the recorded policy;
- `manual_review`: author review is required before any regeneration action;
- `blocked`: regeneration must not proceed until the dependency problem is resolved.

Direct changes are evaluated first. Impacts then propagate deterministically through section-to-section dependencies. More restrictive downstream policies take precedence.

## Scientific boundary

The plan is an impact-analysis artifact. It does not regenerate prose, interpret scientific meaning, alter results, or merge generated content with author-edited text.

## Output bundle

`write_manuscript_regeneration_plan()` writes JSON, Markdown, TSV, and a SHA256 manifest. Existing output is protected unless `overwrite = TRUE`, and written bundles can be revalidated for corruption.
