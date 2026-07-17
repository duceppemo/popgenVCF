# Journal submission profiles

Journal profiles are deterministic metadata records layered on top of submission-package planning. They define semantic file-role requirements and destination filenames without changing source artifacts.

## Core API

- `new_journal_profile()` creates an immutable profile.
- `validate_journal_profile()` verifies structure and digest identity.
- `generic_journal_profile()` returns the built-in neutral profile.
- `apply_journal_profile()` validates a submission plan and applies deterministic role-based naming.

## Profile model

Each profile records:

- a stable identifier and description;
- required semantic roles;
- optional semantic roles;
- a named role-to-filename mapping;
- a SHA256 digest over the canonical profile payload.

Role sets are sorted and deduplicated. Required and optional roles cannot overlap. Filename mappings may only refer to declared roles and cannot create duplicate destinations.

## Scientific boundary

Profiles affect validation and destination naming only. Manuscript text, rendered files, JATS XML, bibliography keys, scientific results, artifact identities, and author interpretation remain unchanged.

Named journal profiles should only be bundled after their requirements have been reviewed and versioned. User-defined profiles can represent local or journal-specific rules in the meantime.
