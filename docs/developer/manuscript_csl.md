# CSL-aware manuscript citation metadata

Phase 7.3.2 adds a reproducible citation-profile layer to automatic manuscript generation.

## Citation profiles

`new_citation_profile()` creates an immutable `PopgenVCFCitationProfile` with:

- a stable style identity;
- an optional source CSL path;
- the original CSL filename;
- a SHA256 checksum;
- the fixed portable manuscript path `citation-style.csl`.

A profile may identify a built-in or externally managed style without embedding a file. When a custom CSL file is supplied, its extension, existence, and checksum are validated before it can be attached to a manuscript.

## Manuscript integration

`set_manuscript_citation_profile()` attaches the validated profile without changing canonical bibliography keys or manuscript text. The profile receives its own deterministic digest.

`manuscript_citation_keys()` extracts BibTeX entry keys, validates their portable syntax, removes duplicates, and returns them in sorted order. Keys remain canonical identities; CSL affects formatting only.

## Pandoc-ready Markdown

Generated Markdown begins with deterministic YAML front matter. Depending on available records, it contains:

```yaml
---
title: "Example manuscript"
bibliography: references.bib
csl: citation-style.csl
link-citations: true
---
```

The generator does not invoke Pandoc in this phase. It produces portable, renderer-ready source while leaving external rendering and journal polishing to later phases.

## Generated citation files

A manuscript directory may now contain:

```text
references.bib
citation-style.csl
citation-profile.json
citation-manifest.tsv
```

`citation-manifest.tsv` links every canonical citation key to its bibliography source, citation-style identity, portable CSL path, and CSL checksum.

All files are included in `manuscript-manifest.tsv`, so modified bibliography, citation metadata, or CSL content is detected by `validate_manuscript()`.

## Scientific boundary

Citation profiles control presentation only. They do not modify analysis results, methods statements, artifact identities, author-supplied interpretation, or canonical citation keys.
