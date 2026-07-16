# FAIR research objects

`PopgenVCFFAIRMetadata` is the standards-facing metadata contract for a reproducible analysis project.

## Identity

- The project identifier is `urn:popgenvcf:project:<project UUID>`.
- Artifact URNs are derived from the project UUID and immutable lineage artifact ID.
- The original VCF sample identity and project UUID are never replaced by display aliases or repository identifiers.

## Creators and rights

Creators are explicit records containing a name and optional ORCID, affiliation, and email. ORCIDs are normalized to the canonical hyphenated identifier. The analysis license is stored independently from the popgenVCF software license, allowing dataset and analysis rights to be represented accurately.

## Generated documents

`write_fair_bundle()` writes:

- `ro-crate-metadata.json`;
- `codemeta.json`;
- `datacite.json`;
- `CITATION.cff`;
- `fair-metadata.rds`;
- `fair-manifest.tsv`.

The RDS file retains the complete validated object. JSON and CFF files contain plain standards-facing records only.

## Integrity

`validate_fair_bundle()` checks required files, parses all JSON documents, validates the canonical metadata object, and recomputes every SHA256 in the manifest. The FAIR object can also be embedded in a `.popgenvcf` project with `set_project_fair_metadata()`.

## Repository deposition

The DataCite record is a deposition-ready metadata payload, not an automatic DOI minting operation. Repository-specific identifiers, DOI prefixes, embargoes, funding records, and upload APIs remain the responsibility of the destination repository integration.
