# Phase 10 public artifact and provenance adapters

Phase 10.1.2 exposes two stable read-only operations over existing package contracts.

## `artifact.list`

`list_public_artifacts()` accepts a canonical public request and an existing `PopgenVCFArtifactManifest`. The response uses deterministic `module::name` identifiers and exposes stable artifact type, format, description, and required status. Filesystem paths and arbitrary artifact metadata remain internal.

## `provenance.inspect`

`inspect_public_provenance()` accepts a canonical public request and an existing `PopgenVCFProvenanceDAG`. The response contains canonically ordered nodes, edges, and the deterministic topological order. Runtime timestamps, parameters, software records, scheduler state, and other implementation-only details remain internal.

Both adapters validate their authoritative source objects, fail closed with stable public error codes, and return standard Phase 10 response envelopes. They do not implement a second artifact registry or provenance graph.
