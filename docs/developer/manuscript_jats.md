# Deterministic JATS manuscript generation

Phase 7.3.4 adds a deterministic JATS XML representation of a canonical popgenVCF manuscript.

## Public API

- `render_manuscript_jats()` creates deterministic JATS XML text from a validated manuscript object.
- `write_manuscript_jats()` writes `jats/manuscript.xml` plus an execution record and checksum manifest.
- `validate_manuscript_jats()` validates the generated article structure and output checksum identity.

## Article structure

The generated article contains:

- article and project identity;
- title, abstract, and keywords;
- contributor names, affiliations, ORCID identifiers, email addresses, and correspondence markers;
- Introduction, Methods, Results, Discussion, declaration, and supplementary sections;
- stable figure, table, and supplementary identifiers derived from immutable manuscript artifact identities;
- canonical bibliography keys represented as stable JATS reference identities.

## Output directory

```text
jats/
  manuscript.xml
  jats-record.json
  jats-manifest.tsv
```

The generation record stores the JATS profile identity, project identity, normalized output path, and SHA256 digest. The manifest provides an independent tabular checksum record.

## Determinism

JATS identifiers are derived from canonical project, section, artifact, and citation identities. Input ordering is preserved only where it is scientifically meaningful, such as author order. XML-special characters are escaped before output.

## Validation boundary

Validation is offline and does not retrieve external DTDs or schemas. It verifies the generated article envelope, balanced structural expectations, file existence, and checksum integrity. Journal-specific DTD validation is deferred to a later phase.

## Scientific boundary

JATS generation transforms manuscript structure and presentation only. It does not alter canonical results, citation keys, artifact identities, generated scientific statements, or author-supplied interpretation.
