# Reviewer-response packages

`popgenVCF` stores a revision letter as an immutable `PopgenVCFReviewerResponse` record. It preserves reviewer comments and explicit author-supplied responses without generating rebuttals or inferring manuscript edits.

## Required columns

Each comment row contains `reviewer`, `comment_id`, `section`, `comment`, `status`, `response`, `action`, `evidence`, and `location`. The `(reviewer, comment_id)` pair must be unique. Rows are canonically sorted before the SHA256 identity is calculated.

Allowed statuses are `unanswered`, `addressed`, `partially_addressed`, `declined`, and `not_applicable`.

## Completeness rules

`reviewer_response_report()` applies conservative rules:

- addressed comments require a response, action or location, and evidence or location;
- partially addressed comments require a response and explicit action;
- declined and not-applicable comments require an explicit rationale;
- unanswered comments remain incomplete.

Strict mode raises an error for incomplete comments. Permissive mode returns the full actionable report.

## Example

```r
comments <- data.frame(
  reviewer = "Reviewer 1",
  comment_id = "1.1",
  section = "Methods",
  comment = "Report the filtering thresholds.",
  status = "addressed",
  response = "We added the requested filtering thresholds.",
  action = "Added a filtering paragraph.",
  evidence = "Revised Methods text",
  location = "Methods, paragraph 2"
)

response <- new_reviewer_response(
  comments,
  manuscript_id = "manuscript-2026-01",
  revision_id = "revision-1"
)

reviewer_response_report(response, strict = TRUE)
write_reviewer_response(response, "submission")
```

## Bundle

`write_reviewer_response()` creates JSON, Markdown, comment TSV, completion-report TSV, and checksum-manifest files. `validate_reviewer_response()` detects missing or modified bundle files.

This capability organizes author-supplied material only. It does not draft responses, infer compliance, modify manuscript science, or generate tracked changes.
