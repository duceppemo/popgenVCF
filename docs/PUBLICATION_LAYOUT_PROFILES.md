# Phase 0.9.2 publication layout profiles

Phase 0.9.2 adds deterministic journal-oriented rendering layouts on top of the canonical journal submission profiles and Phase 0.9.1 report-rendering contracts.

## Built-in profiles

`publication_layout_profile()` provides five versioned presets:

- `general`
- `nature-style`
- `g3`
- `molecular-ecology`
- `plos`

These are stable layout conventions, not claims that publisher requirements can never change. Each profile records its own version, the digest of the existing journal submission profile it composes, supported formats, page geometry, typography, heading and numbering rules, caption placement, bibliography behavior, submission requirements, and backend-independent renderer parameters.

## Binding workflow

1. Create or select a journal submission profile.
2. Create a publication report specification.
3. Select a publication layout profile.
4. Use `bind_publication_layout()` to bind the profile to the report specification.
5. Pass `publication_layout_parameters()` to the renderer execution layer.
6. Record the layout fingerprint with the report execution and output manifest provenance.

Bindings normalize named overrides and fail closed on unsupported formats, unknown override fields, profile drift, report-specification drift, journal-profile drift, or fingerprint mutation.

## Architectural boundary

Publication layout profiles do not replace manuscript records, journal submission profiles, publication bundles, renderer adapters, report plans, execution records, artifact registries, or provenance graphs. They provide a deterministic translation layer between existing publication requirements and backend-neutral renderer parameters.
