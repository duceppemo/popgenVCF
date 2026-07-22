# GitHub Actions supply-chain policy

All third-party GitHub Actions used by popgenVCF workflows must be pinned to an immutable, full-length commit SHA.

## Required reference form

```yaml
uses: actions/checkout@11d5960a326750d5838078e36cf38b85af677262 # v4
```

The SHA is the executed identity. The trailing semantic-version comment records the reviewed upstream release line and allows Dependabot to prepare understandable update pull requests.

Mutable branches and tags such as `@main`, `@v4`, or `@latest` are prohibited in executable workflow references. Repository-local actions beginning with `./` and explicit `docker://` references are classified separately by the audit.

## Validation

`scripts/validate_workflow_action_pins.py` scans every YAML file under `.github/workflows/` and fails when:

- an external action has no `@ref`;
- the ref is not a lowercase 40-character commit SHA;
- the action path is malformed;
- the retained version comment is missing;
- no external actions are discovered.

The script emits a deterministic JSON inventory containing the workflow path, line number, action path, commit, and version comment. `.github/workflows/supply-chain.yml` runs the audit whenever workflows, the validator, Dependabot policy, or this document changes.

## Updating actions

Dependabot checks GitHub Actions weekly and opens reviewable pull requests. An update should be merged only after:

1. the new commit belongs to the intended official action repository and release line;
2. upstream release notes and security implications have been reviewed;
3. the inline version comment remains accurate;
4. the complete affected workflow matrix passes;
5. release-facing permissions and behavior have not broadened unexpectedly.

Do not replace a SHA with a floating tag to resolve an update conflict. Resolve the upstream tag to its exact commit and retain that immutable identity.
