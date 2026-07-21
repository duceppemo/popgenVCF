# Publication figure styles

Phase 0.9.3 adds deterministic presentation contracts for figures without changing scientific values, groups, or source-data exports.

## Built-in modes

- `standard-color` provides a deterministic color palette with redundant line and point encodings.
- `grayscale-safe` uses luminance-separated gray values plus distinct line types and point shapes.
- `accessibility-first` uses a color-vision-deficiency-aware palette and requires labels or legends plus redundant non-color encodings.

Each profile records ordered colors, fills, line types, point shapes, foreground and background colors, contrast requirements, labeling policy, grayscale guarantees, version, and fingerprint.

## Binding and validation

`bind_publication_figure_style()` binds a profile to an existing publication report specification and publication layout profile. The binding records all three fingerprints and fails closed when the requested number of scientific groups exceeds the available distinguishable aesthetics.

`validate_publication_figure_style_profile()` checks color validity, foreground/background contrast, grayscale luminance separation, profile consistency, and mutation. `validate_publication_figure_style_binding()` detects report, layout, style, format, and binding drift.

## Renderer parameters and audits

`publication_figure_parameters()` returns backend-independent plotting parameters. `publication_figure_accessibility_audit()` returns a deterministic audit containing contrast, grayscale luminance, redundant-encoding status, label requirements, and a fingerprint.

Style contracts affect only presentation. They must never alter analysis results, grouping, ordering, or source-data values.
