# Style Guide

## Scope

This guide defines conventions for R code, configuration, tables, figures, reports, documentation, and user-facing terminology.

## R code

- Prefer clear functions with explicit inputs and return values.
- Use `snake_case` for objects and functions, `UpperCamelCase` only for formal class names, and uppercase constants sparingly.
- Avoid hidden global state and working-directory changes.
- Use `fs`-independent base paths or `file.path()` unless a dependency is justified.
- Validate arguments at public boundaries and produce actionable errors.
- Return structured results; do not rely on printed output as data.
- Do not install packages from analysis functions.
- Qualify optional-package calls with `package::function`.
- Limit data copies for genotype-scale objects.
- Comment scientific intent and non-obvious constraints, not line-by-line mechanics.

## Configuration

- YAML keys use lowercase `snake_case`.
- Every configurable parameter has a documented type, default, accepted range, and scientific meaning.
- Defaults must be stable and visible in generated configuration files.
- Unknown keys should be rejected or explicitly warned about.
- File paths and sample-order sources must never be guessed when ambiguity can alter results.

## Terminology

Use precise names consistently:

- `IBS distance` or `1 - IBS`, never FST;
- `population` for supplied biological group metadata;
- `cluster` for algorithmic groups;
- `ancestry membership` only for outputs from an appropriate model such as ADMIXTURE, fastStructure, or sNMF;
- `DAPC posterior membership`, not ancestry;
- `variant missingness` and `sample missingness`, not generic missing rate when the axis matters;
- `Weir-Cockerham FST` when that estimator is used.

Avoid causal or biological claims not supported by the analysis.

## Tables

- Canonical tables are TSV with UTF-8 encoding and explicit column names.
- Never encode important information only in row names.
- Include identifiers, units, estimator names, and confidence-interval definitions.
- Preserve full precision in canonical tables; apply display rounding only to publication tables.
- Missing values use `NA` in machine-readable outputs unless an external format requires otherwise.
- Publication tables include concise titles, footnotes for abbreviations, and sufficient estimator detail.

## Figures

Every publication figure should:

- be generated from canonical results;
- have deterministic dimensions and ordering;
- include informative axis labels and units;
- use accessible, color-vision-aware palettes;
- remain interpretable in grayscale when the selected preset requires it;
- avoid implying discrete groups through color unless groups are defined;
- export SVG and PDF plus a high-resolution PNG;
- include a machine-generated caption and source-data table.

Do not use three-dimensional plots when a two-dimensional representation communicates the result more accurately.

## Plot themes

Journal presets may alter typography, dimensions, line widths, and legend placement, but they must not alter data, statistical transformations, group ordering, or significance decisions.

Default themes should be restrained, high-contrast, and suitable for both screens and print.

## Reports

Generated reports distinguish:

- **Methods:** what was done and with which parameters;
- **Results:** direct summaries of computed outputs;
- **Interpretation guidance:** educational context and limitations;
- **Discussion prompts:** optional statements clearly marked as requiring researcher judgment.

Methods and results must be reproducible from canonical artifacts. Generated prose should include values from tables rather than independently recomputing statistics.

## Documentation

- Start with the scientific purpose, then usage.
- Define estimators and assumptions before interpretation.
- Include runnable examples based on versioned fixtures.
- Cite primary methodological sources where possible.
- State known differences from other tools.
- Use absolute paths only in clearly marked examples; never embed developer-machine paths.

## File naming

Use stable, descriptive names:

```text
Table_01_dataset_summary.tsv
Figure_02_pca_PC1_PC2.svg
Supplementary_03_pairwise_fst.tsv
methods_pca.md
validation_fst.json
```

Avoid spaces, locale-dependent characters, and filenames that differ only by case.

## Logging and errors

Log levels are `DEBUG`, `INFO`, `WARNING`, and `ERROR`. Normal compatibility handling should not produce alarming warnings. Errors should include module, failed condition, affected identifier or file, and a remediation when known.

## Versioning and provenance

Every final report records package version, source revision, configuration checksum, input checksums, external executable versions, R session information, seeds, start/end time, and platform.