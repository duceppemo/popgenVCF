# Deterministic Pandoc manuscript rendering

Phase 7.3.3 adds an explicit execution layer above the canonical manuscript source directory.

## API

- `pandoc_status()` records availability, executable path, and version identity.
- `pandoc_render_arguments()` builds deterministic HTML or DOCX command arguments.
- `render_manuscript()` executes or dry-runs Pandoc rendering.
- `validate_manuscript_render()` verifies successful output and SHA256 integrity.

## Output layout

Rendering writes into the manuscript directory without changing canonical source files:

```text
rendered/
  manuscript.html
  manuscript.docx
  pandoc-html.stdout.log
  pandoc-html.stderr.log
  pandoc-docx.stdout.log
  pandoc-docx.stderr.log
  render-html.json
  render-docx.json
```

Only the requested format is generated. Existing output is protected unless `overwrite = TRUE`.

## Dry runs

`dry_run = TRUE` returns the complete render record and deterministic command arguments without requiring Pandoc or writing rendered output. This supports CI validation and execution planning on systems where Pandoc is unavailable.

## Citation processing

The command includes `--citeproc`. Pandoc consumes the YAML bibliography and CSL paths already written into `manuscript.md`. Canonical BibTeX keys and citation-profile metadata are not rewritten by popgenVCF.

## Reproducibility record

Each completed render records:

- format;
- manuscript directory;
- Pandoc path and version;
- exact argument vector;
- output and log paths;
- process exit status;
- output SHA256.

## Scientific boundary

Rendering is a presentation operation. It must not alter manuscript Markdown, canonical scientific results, artifact identities, citation keys, generated statements, or author-supplied interpretation.
