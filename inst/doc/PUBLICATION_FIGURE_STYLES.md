# Publication figure styles

popgenVCF uses deterministic presentation contracts for figures without changing scientific values, groups, ordering, or source-data exports. The same contracts now drive the figures emitted by the analysis pipeline.

## Built-in modes

- `standard-color` provides a deterministic color palette with redundant line and point encodings.
- `grayscale-safe` uses luminance-separated gray values plus distinct line types and point shapes.
- `accessibility-first` uses a color-vision-deficiency-aware palette and requires labels or legends plus redundant non-color encodings.

Each profile records ordered colors, fills, line types, point shapes, foreground and background colors, contrast requirements, labeling policy, grayscale guarantees, version, and fingerprint.

Select the runtime profile in `analysis.yml`:

```yaml
output:
  figure_formats: [pdf, png, svg]
  dpi: 600
  figure_style: accessibility-first
  base_font_size: 11
  label_samples: auto
```

`accessibility-first` is the default. PCA and IBS ordinations encode populations with both colour and point shape. DAPC uses population colour and cluster shape. Ancestry and membership bars receive an explicit deterministic fill scale instead of ggplot2's session-dependent default palette. Population colour tables written with the results use the selected profile.

The grayscale mode should be inspected carefully when a figure contains many groups. There is a finite number of gray levels that remain distinguishable in print; popgenVCF warns when the requested palette exceeds that practical capacity.

## Binding and validation

`bind_publication_figure_style()` binds a profile to an existing publication report specification and publication layout profile. The binding records all three fingerprints and fails closed when the requested number of scientific groups exceeds the available distinguishable aesthetics.

`validate_publication_figure_style_profile()` checks color validity, foreground/background contrast, grayscale luminance separation, profile consistency, and mutation. `validate_publication_figure_style_binding()` detects report, layout, style, format, and binding drift.

## Renderer parameters and audits

`publication_figure_parameters()` returns backend-independent plotting parameters. `publication_figure_accessibility_audit()` returns a deterministic audit containing contrast, grayscale luminance, redundant-encoding status, label requirements, and a fingerprint.

## Export quality and reproducibility

- PDF output uses a Cairo device when available for reliable font embedding and vector geometry.
- PNG output uses `ragg` when installed and otherwise falls back to the platform PNG device at the configured DPI.
- SVG output uses `svglite` when installed.
- Every device is rendered on an explicit white background with dimensions in inches.
- Randomly jittered sample points use the configured analysis seed, so repeated runs produce identical figure geometry.
- Bootstrap diversity intervals are plotted when available; figures explicitly state when intervals were not calculated.
- Thresholds, axes, legends, and statistical annotations use consistent wording and visual hierarchy.

Style contracts affect only presentation. They must never alter analysis results, grouping, ordering, or source-data values.
