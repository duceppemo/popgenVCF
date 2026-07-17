# Deterministic graphical abstract specifications

`PopgenVCFGraphicalAbstract` records author-supplied graphical-abstract messaging and ordered references to existing immutable publication assets.

## Design boundaries

The specification layer does not redraw, crop, recolour, summarize, or reinterpret scientific figures. It records assembly instructions only. Titles, central messages, panel captions, and accessibility descriptions remain explicit author inputs.

## Workflow

1. Create or load a canonical manuscript.
2. Select existing publication assets and their immutable artifact identities.
3. Create an ordered panel specification with `new_graphical_abstract()`.
4. Run permissive validation while drafting and strict validation before submission.
5. Write the isolated bundle with `write_graphical_abstract()`.

## Output

The `graphical-abstract/` directory contains:

- `graphical-abstract-record.json`: canonical specification and dimensions;
- `graphical-abstract-manifest.tsv`: ordered panel identities and SHA256 checksums;
- `graphical-abstract-brief.md`: human-readable assembly instructions with visible placeholders.

## Integrity

Each referenced panel file is hashed when the specification is created. Validation recomputes each SHA256 value and rejects missing or modified assets. The specification identity is derived deterministically from manuscript identity, author inputs, dimensions, panel order, and asset checksums.

## Submission integration

The written directory can be included in a manuscript submission package under a `graphical_abstract` semantic role. Journal profiles may rename the final deliverable while preserving the source specification and checksums.
