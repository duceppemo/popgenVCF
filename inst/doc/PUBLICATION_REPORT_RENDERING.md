# Phase 0.9.1 publication report rendering

Phase 0.9.1 introduces backend-independent contracts for deterministic publication report rendering.

## Contract chain

1. `new_publication_report_spec()` defines canonical formats and presentation requirements.
2. `new_publication_report_plan()` binds a validated manuscript, its canonical Markdown source, the publication identity, renderer identity, and expected outputs.
3. A renderer writes the planned HTML, PDF, or DOCX files without changing the plan.
4. `new_publication_report_output_manifest()` records output sizes and SHA-256 checksums.
5. `validate_publication_report_output_manifest()` detects missing, substituted, or mutated outputs.

Generated factual material remains bound to the manuscript's methods, captions, artifacts, software, parameters, and bibliography. Abstract, introduction, results interpretation, discussion, and declarations remain explicitly author-editable.

Supported canonical formats are `html`, `pdf`, and `docx`. Validation fails closed on unsupported formats, mutated records, manuscript or specification drift, missing outputs, and checksum mismatch.
