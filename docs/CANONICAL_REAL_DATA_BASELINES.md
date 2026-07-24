# Canonical real-data baseline adoption

Phase 0.9.27 promotes quantitative baselines from synthetic contract tests to formally reviewed real-data scientific records.

Phase 0.9.32 supplies the first bounded execution path for such a proposal: checksum-bound QC, LD-pruning, and PCA observations from a fixed chromosome 22 interval. The generated snapshot remains unapproved pending scientific review.

## Snapshot contract

A `PopgenVCFCanonicalRealDataBaselineSnapshot` binds:

- one approved canonical dataset identifier and version;
- the SHA-256 inventory of every acquired dataset artifact;
- a versioned canonical baseline registry;
- complete, deterministically ordered sample metadata;
- the generating workflow, UTC timestamp, and full Git commit SHA;
- an explicit `proposed` or `approved` review state.

The required sample metadata fields are `sample_id`, `population`, `superpopulation`, and `sex`. Missing values, blank values, and duplicate sample identifiers are rejected.

## Approval gate

Snapshots are generated as `proposed`. Scientific reviewers promote a snapshot to `approved` only after checking dataset identity, sample metadata, metric definitions, expected values, tolerances, and provenance. Production validation and release assembly must call validation or serialization with `require_approved = TRUE`; proposed snapshots then fail closed.

No numerical value becomes authoritative merely because a workflow produced it. Approval is an explicit scientific action recorded with reviewer identity and ISO-8601 review date.

## CI policy

Ordinary pull-request CI remains synthetic, deterministic, offline, and fast. Acquisition and computation against the approved 1000 Genomes Phase 3 chromosome 22 or chromosome Y datasets belong only in opt-in full-validation CI.

The full-validation workflow should:

1. acquire the checksum-pinned source files;
2. verify upstream MD5 values and promote the inventory to SHA-256;
3. validate complete sample metadata against the VCF samples;
4. compute the production validation observations;
5. write a proposed real-data baseline snapshot;
6. compare observations with the currently approved snapshot when one exists;
7. publish all descriptors, metrics, logs, and snapshot evidence as workflow artifacts.

## Phase boundary

The adoption contract and chromosome 22 proposal execution remain fail closed. The first checked-in approved snapshot must originate from retained production evidence, undergo scientific review, and record named approval separately; expected values must never be invented or copied from synthetic fixtures.

See [Autosomal quantitative baseline proposal](developer/canonical-autosomal-baseline-proposal.md) for the fixed interval, analysis settings, evidence inventory, and approval boundary.
