# Phase 0.9.2 publication layout profiles

Phase 0.9.2 provides deterministic `general`, `nature-style`, `g3`, `molecular-ecology`, and `plos` rendering presets.

Use `publication_layout_profile()` to select a preset, `bind_publication_layout()` to bind it to a publication report specification, `validate_publication_layout_binding()` to verify the binding, and `publication_layout_parameters()` to obtain canonically ordered renderer parameters.

Each profile composes the existing journal submission profile contract and records supported formats, geometry, typography, structure, bibliography behavior, submission rules, version, and fingerprint. Validation fails closed on unsupported formats, invalid overrides, profile drift, journal-profile drift, or binding mutation.
