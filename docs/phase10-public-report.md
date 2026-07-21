# Phase 10 public report rendering adapter

Phase 10.1.3 exposes the stable `report.render` operation through `render_public_report()`.

The adapter accepts a canonical public request plus either an existing `PopgenVCFReportPlan` or a named list of canonical analysis results. It delegates planning and writing to `build_population_genomics_report_plan()` and `write_population_genomics_report()` rather than introducing a second report engine.

Successful responses contain a deterministic report identity, canonical section identities, requested formats, rendering status, artifact identities, and a report-plan provenance identity. Output directories, generated paths, plan timestamps, R and platform metadata, Quarto process output, and renderer state remain internal.

By default, `render = FALSE` writes deterministic report sources and manifests without requiring Quarto. Setting `render = TRUE` invokes the existing Quarto-backed report engine for HTML or PDF output. Invalid inputs, unsupported formats, and renderer failures fail closed with stable public error codes.
