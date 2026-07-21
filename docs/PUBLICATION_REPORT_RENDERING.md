# Phase 0.9.1 publication report rendering

Phase 0.9.1 introduces backend-independent contracts for deterministic publication report rendering.

## Contract chain

1. `new_publication_report_spec()` defines canonical formats and presentation requirements.
2. `new_publication_report_plan()` binds a validated manuscript, its canonical Markdown source, the publication identity, renderer identity, and expected outputs.
3. A renderer writes the planned HTML, PDF, or DOCX files without changing the plan.
4. `new_publication_report_output_manifest()` records output sizes and SHA-256 checksums.
5. `validate_publication_report_output_manifest()` detects missing, substituted, or mutated outputs.

## Scientific boundaries

Generated factual material remains bound to the manuscript's methods, captions, artifacts, software, parameters, and bibliography. Abstract, introduction, results interpretation, discussion, and declarations remain explicitly author-editable.

The rendering contracts do not replace the canonical manuscript, publication bundle, artifact registry, provenance graph, report engine, or execution runtime. Renderer adapters are replaceable as long as they satisfy the same deterministic plan and output-manifest contracts.

## Supported formats

The canonical format identifiers are:

- `html`
- `pdf`
- `docx`

The initial contract layer records renderer identity and expected outputs but does not require one specific rendering backend.

## Failure behavior

Validation fails closed when:

- a format is unsupported;
- a specification, plan, or output manifest is mutated;
- a plan no longer matches its manuscript or specification;
- a rendered file is missing;
- a rendered checksum differs from the recorded checksum.
