# Automatic manuscript generation

Phase 7.3 introduces a canonical manuscript layer above `PopgenVCFProject` and `PopgenVCFPublicationBundle`.

## Scope of the foundation

The first implementation provides:

- `PopgenVCFManuscript`, a validated immutable manuscript specification;
- deterministic Markdown assembly;
- title, authors, abstract, keywords, and author-controlled narrative sections;
- generated Methods content from the publication companion;
- figure, table, and supplementary indexes from immutable artifact records;
- data-availability, software-availability, and reproducibility statements;
- serialized manuscript records and SHA256 manifests;
- explicit placeholders wherever scientific interpretation must be supplied by authors.

The manuscript layer does not infer biological conclusions. It assembles factual records and clearly labels sections that require author interpretation.

## Basic use

```r
project <- new_popgenvcf_project("Study")
publication <- new_publication_bundle(project)

manuscript <- new_manuscript(
  project,
  publication = publication,
  title = "Population genomic structure of the study cohort",
  authors = data.frame(
    name = c("Jane Doe", "John Smith"),
    affiliation = c("Population Genomics Lab", "Population Genomics Lab"),
    corresponding = c(TRUE, FALSE)
  ),
  abstract = "Author-supplied abstract.",
  introduction = "Author-supplied introduction.",
  results = "Author-supplied interpretation of the canonical outputs.",
  discussion = "Author-supplied discussion.",
  keywords = c("population genomics", "VCF", "reproducibility")
)

write_manuscript(manuscript, "publication/manuscript")
validate_manuscript("publication/manuscript")
```

## Generated directory

```text
manuscript/
├── manuscript.md
├── manuscript.rds
├── authors.tsv
├── captions.tsv
└── manuscript-manifest.tsv
```

`manuscript.rds` is the canonical object. Markdown and TSV files are editable/export-oriented representations. The manifest records file sizes and SHA256 hashes and is validated by `validate_manuscript()`.

## Generated versus author-controlled content

Generated factual content includes:

- project and publication identities;
- methods narratives;
- software and parameter provenance;
- artifact identities, captions, and paths;
- reproducibility statements derived from canonical project records.

Author-controlled content includes:

- abstract;
- introduction;
- scientific interpretation in Results;
- discussion;
- funding, contributions, competing interests, and repository-specific availability details.

Missing author-controlled sections are represented by visible placeholders rather than invented prose.

## Determinism

Given the same canonical project, publication bundle, author metadata, and supplied prose, `render_manuscript_markdown()` returns identical source. Keywords are normalized and sorted. Artifact order follows the canonical publication bundle.

## Planned extensions

Later Phase 7.3 work will add richer cross-references and artifact embedding. Subsequent roadmap items will add CSL rendering, JATS XML, DOCX output, journal-specific templates, required-statement checklists, and deterministic submission packages.
