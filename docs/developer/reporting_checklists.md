# Deterministic reporting checklists

Phase 7.3.10 adds immutable reporting-checklist records and explicit completion reports for publication workflows.

## Scientific boundary

A checklist records structural reporting requirements. It does not infer compliance from manuscript prose, fabricate evidence, or silently mark an item complete. Completion requires an explicit author-supplied response. A `yes` response is only complete when it includes an evidence reference; a `not_applicable` response requires a rationale.

## Checklist contract

`new_reporting_checklist()` creates a `PopgenVCFReportingChecklist` containing:

- a stable identifier and version;
- title, organization, status, description, and source metadata;
- deterministically ordered checklist items;
- a SHA256 digest over the complete payload.

Each item contains `item_id`, `category`, `label`, `requirement`, and `guidance`. Item identifiers are unique, stable, lowercase machine keys. Requirement levels are `required` or `recommended`.

Verified checklists must include both `source_url` and `source_date`. Generic checklists may omit them.

## Generic population-genomics checklist

`generic_reporting_checklist()` provides a conservative built-in checklist covering:

- canonical sample identity and grouping definitions;
- variant filtering;
- software identities and analysis parameters;
- stochastic settings;
- data and code availability;
- artifact traceability;
- limitations.

It is not a replacement for journal-specific reporting standards or study-design-specific guidance.

## Explicit responses

`validate_reporting_checklist_responses()` accepts a table with:

- `item_id`;
- `response`: `yes`, `no`, `partial`, `not_applicable`, or `unanswered`;
- optional `evidence`;
- optional `notes`.

The deterministic report preserves every checklist item and assigns `pass`, `not_applicable`, or `incomplete`. Strict validation fails when any required item remains incomplete.

## Deterministic export

`write_reporting_checklist()` writes:

- `reporting-checklist.json`;
- `reporting-checklist.md`;
- `reporting-checklist-items.tsv`;
- `reporting-checklist-manifest.tsv`.

The manifest records file sizes and SHA256 checksums. `validate_reporting_checklist()` verifies written bundles and detects missing or modified files. Existing output directories are protected unless `overwrite = TRUE` is explicit.
