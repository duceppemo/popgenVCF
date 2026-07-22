# Package-level static-analysis declarations.
#
# Imports are declared here so unqualified calls remain explicit in generated
# namespace metadata. The global-variable inventory contains only symbols used
# intentionally inside data.table non-standard evaluation expressions.

#' @importFrom stats aggregate setNames
#' @importFrom utils capture.output head modifyList object.size str tail
NULL

utils::globalVariables(c(
    "..category", "..clusters", "..keep", "..required",
    "..semantic", ".missing_order", "absolute_error", "affiliation",
    "alias", "allowed_relative_change", "analysis", "attempt",
    "attempt_count", "available", "backend", "caption",
    "category", "change_type", "character_count", "character_count_after",
    "character_count_before", "character_delta", "check", "checkpoint_reused",
    "citation_keys", "comment_id", "completion", "content",
    "content_sha256", "content_sha256_after", "content_sha256_before", "corresponding",
    "dependency_id", "destination", "disk_median_mb_baseline", "disk_median_mb_observed",
    "display_order", "display_sample", "dominant_cluster", "eigenvalue",
    "email", "evidence", "explanation", "from",
    "id", "improvement", "item_id", "k",
    "kind", "label", "memory_median_mb_baseline", "memory_median_mb_observed",
    "name", "notes", "objective", "orcid",
    "passed", "path", "peak_memory_mb", "positive_variance_percent",
    "public_sample", "recommended", "recovered", "regressed",
    "relative_change", "relative_error", "relative_improvement", "response",
    "reviewer", "reviewer_comments", "role", "runtime_median_baseline",
    "runtime_median_observed", "runtime_seconds", "sample_order", "section_id",
    "sha256", "size_bytes", "stability", "staged_name",
    "status", "temporary_disk_mb", "threads", "title",
    "title_after", "title_before", "to", "type",
    "urn", "vcf_sample", "wave", "word_count",
    "word_count_after", "word_count_before", "word_delta"
))
