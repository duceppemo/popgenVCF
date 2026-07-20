# Deterministic execution checkpoint contracts
#
# Phase 9.7 establishes canonical checkpoint and recovery records for
# resumable scientific module execution. These constructors intentionally
# fail closed on malformed or ambiguous state.

.checkpoint_scalar_character <- function(x, field, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be one non-missing character value.", field), call. = FALSE)
  }
  if (!allow_empty && !nzchar(x)) {
    stop(sprintf("`%s` must not be empty.", field), call. = FALSE)
  }
  x
}

.checkpoint_named_character <- function(x, field) {
  if (is.null(x)) {
    return(setNames(character(), character()))
  }
  if (!is.character(x) || anyNA(x)) {
    stop(sprintf("`%s` must be a named character vector without missing values.", field), call. = FALSE)
  }
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    stop(sprintf("`%s` must have non-empty, non-missing names.", field), call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    stop(sprintf("`%s` contains duplicate identities.", field), call. = FALSE)
  }
  x[order(nms, method = "radix")]
}

.checkpoint_digest <- function(x) {
  raw <- serialize(x, connection = NULL, version = 3L)
  ints <- as.integer(raw)
  acc <- 2166136261
  for (value in ints) {
    acc <- bitwXor(acc, value)
    acc <- (acc * 16777619) %% 2^32
  }
  sprintf("%08x", as.integer(acc %% .Machine$integer.max))
}

new_execution_checkpoint <- function(
    plan_id,
    module_id,
    module_version,
    state,
    scientific_inputs = character(),
    dependencies = character(),
    outputs = character(),
    cache_keys = character(),
    schema_fingerprints = character(),
    environment_fingerprint,
    executor_id,
    sequence,
    payload_checksum,
    previous_checkpoint_id = NULL,
    metadata = list()) {
  plan_id <- .checkpoint_scalar_character(plan_id, "plan_id")
  module_id <- .checkpoint_scalar_character(module_id, "module_id")
  module_version <- .checkpoint_scalar_character(module_version, "module_version")
  state <- match.arg(
    .checkpoint_scalar_character(state, "state"),
    c("created", "committed", "superseded", "restored", "invalidated", "abandoned")
  )
  environment_fingerprint <- .checkpoint_scalar_character(
    environment_fingerprint,
    "environment_fingerprint"
  )
  executor_id <- .checkpoint_scalar_character(executor_id, "executor_id")
  payload_checksum <- .checkpoint_scalar_character(payload_checksum, "payload_checksum")

  if (!is.numeric(sequence) || length(sequence) != 1L || is.na(sequence) ||
      sequence < 0 || sequence != as.integer(sequence)) {
    stop("`sequence` must be one non-negative integer.", call. = FALSE)
  }
  sequence <- as.integer(sequence)

  if (!is.null(previous_checkpoint_id)) {
    previous_checkpoint_id <- .checkpoint_scalar_character(
      previous_checkpoint_id,
      "previous_checkpoint_id"
    )
  }
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }

  checkpoint <- list(
    contract = "popgenvcf.execution-checkpoint",
    contract_version = "1.0.0",
    plan_id = plan_id,
    module_id = module_id,
    module_version = module_version,
    state = state,
    sequence = sequence,
    scientific_inputs = .checkpoint_named_character(scientific_inputs, "scientific_inputs"),
    dependencies = .checkpoint_named_character(dependencies, "dependencies"),
    outputs = .checkpoint_named_character(outputs, "outputs"),
    cache_keys = .checkpoint_named_character(cache_keys, "cache_keys"),
    schema_fingerprints = .checkpoint_named_character(
      schema_fingerprints,
      "schema_fingerprints"
    ),
    environment_fingerprint = environment_fingerprint,
    executor_id = executor_id,
    payload_checksum = payload_checksum,
    previous_checkpoint_id = previous_checkpoint_id,
    metadata = metadata
  )

  checkpoint$checkpoint_id <- paste0("checkpoint:", .checkpoint_digest(checkpoint))
  checkpoint$fingerprint <- .checkpoint_digest(checkpoint)
  class(checkpoint) <- c("popgenvcf_execution_checkpoint", "list")
  checkpoint
}

validate_execution_checkpoint <- function(
    checkpoint,
    expected_plan_id = NULL,
    expected_module_id = NULL,
    expected_module_version = NULL,
    expected_environment_fingerprint = NULL,
    observed_payload_checksum = NULL) {
  errors <- character()
  warnings <- character()

  if (!inherits(checkpoint, "popgenvcf_execution_checkpoint")) {
    errors <- c(errors, "checkpoint_class_invalid")
  }
  required <- c(
    "contract", "contract_version", "plan_id", "module_id", "module_version",
    "state", "sequence", "environment_fingerprint", "executor_id",
    "payload_checksum", "checkpoint_id", "fingerprint"
  )
  missing_fields <- setdiff(required, names(checkpoint))
  if (length(missing_fields)) {
    errors <- c(errors, paste0("missing_field:", sort(missing_fields, method = "radix")))
  }

  compare_expected <- function(field, expected, code) {
    if (!is.null(expected) && (!field %in% names(checkpoint) ||
        !identical(checkpoint[[field]], expected))) {
      errors <<- c(errors, code)
    }
  }
  compare_expected("plan_id", expected_plan_id, "plan_mismatch")
  compare_expected("module_id", expected_module_id, "module_mismatch")
  compare_expected("module_version", expected_module_version, "module_version_mismatch")
  compare_expected(
    "environment_fingerprint",
    expected_environment_fingerprint,
    "environment_mismatch"
  )
  compare_expected("payload_checksum", observed_payload_checksum, "payload_checksum_mismatch")

  if (!length(missing_fields)) {
    original_fingerprint <- checkpoint$fingerprint
    candidate <- checkpoint
    candidate$fingerprint <- NULL
    candidate$checkpoint_id <- NULL
    class(candidate) <- "list"
    expected_fingerprint <- .checkpoint_digest(candidate)
    if (!identical(original_fingerprint, expected_fingerprint)) {
      errors <- c(errors, "checkpoint_fingerprint_mismatch")
    }
  }

  errors <- sort(unique(errors), method = "radix")
  warnings <- sort(unique(warnings), method = "radix")

  structure(
    list(
      valid = length(errors) == 0L,
      decision = if (length(errors)) "reject" else "accept",
      errors = errors,
      warnings = warnings
    ),
    class = c("popgenvcf_checkpoint_validation", "list")
  )
}

new_recovery_decision <- function(
    plan_id,
    checkpoint_id = NULL,
    action,
    reason_codes,
    compatible,
    selected_sequence = NULL,
    diagnostics = list()) {
  plan_id <- .checkpoint_scalar_character(plan_id, "plan_id")
  action <- match.arg(
    .checkpoint_scalar_character(action, "action"),
    c("resume", "restart", "reuse_cache", "rollback", "reject")
  )
  if (!is.null(checkpoint_id)) {
    checkpoint_id <- .checkpoint_scalar_character(checkpoint_id, "checkpoint_id")
  }
  if (!is.character(reason_codes) || anyNA(reason_codes) || any(!nzchar(reason_codes))) {
    stop("`reason_codes` must be non-empty character values without missing entries.", call. = FALSE)
  }
  reason_codes <- sort(unique(reason_codes), method = "radix")
  if (!is.logical(compatible) || length(compatible) != 1L || is.na(compatible)) {
    stop("`compatible` must be one non-missing logical value.", call. = FALSE)
  }
  if (compatible && action == "reject") {
    stop("A compatible recovery candidate cannot use action `reject`.", call. = FALSE)
  }
  if (!compatible && action %in% c("resume", "reuse_cache")) {
    stop("An incompatible recovery candidate cannot be resumed or reused.", call. = FALSE)
  }
  if (!is.null(selected_sequence)) {
    if (!is.numeric(selected_sequence) || length(selected_sequence) != 1L ||
        is.na(selected_sequence) || selected_sequence < 0 ||
        selected_sequence != as.integer(selected_sequence)) {
      stop("`selected_sequence` must be one non-negative integer.", call. = FALSE)
    }
    selected_sequence <- as.integer(selected_sequence)
  }
  if (!is.list(diagnostics)) {
    stop("`diagnostics` must be a list.", call. = FALSE)
  }

  decision <- list(
    contract = "popgenvcf.recovery-decision",
    contract_version = "1.0.0",
    plan_id = plan_id,
    checkpoint_id = checkpoint_id,
    action = action,
    compatible = compatible,
    selected_sequence = selected_sequence,
    reason_codes = reason_codes,
    diagnostics = diagnostics
  )
  decision$decision_id <- paste0("recovery:", .checkpoint_digest(decision))
  decision$fingerprint <- .checkpoint_digest(decision)
  class(decision) <- c("popgenvcf_recovery_decision", "list")
  decision
}

select_execution_checkpoint <- function(checkpoints) {
  if (!is.list(checkpoints) || !length(checkpoints)) {
    return(new_recovery_decision(
      plan_id = "unknown",
      action = "restart",
      reason_codes = "no_checkpoint_candidates",
      compatible = TRUE
    ))
  }

  valid <- vapply(
    checkpoints,
    function(x) isTRUE(validate_execution_checkpoint(x)$valid),
    logical(1)
  )
  candidates <- checkpoints[valid]
  if (!length(candidates)) {
    plan_id <- if (!is.null(checkpoints[[1L]]$plan_id)) checkpoints[[1L]]$plan_id else "unknown"
    return(new_recovery_decision(
      plan_id = plan_id,
      action = "reject",
      reason_codes = "no_valid_checkpoint_candidates",
      compatible = FALSE
    ))
  }

  sequences <- vapply(candidates, `[[`, integer(1), "sequence")
  highest <- max(sequences)
  finalists <- candidates[sequences == highest]
  ids <- vapply(finalists, `[[`, character(1), "checkpoint_id")
  if (length(unique(ids)) != 1L) {
    return(new_recovery_decision(
      plan_id = finalists[[1L]]$plan_id,
      action = "reject",
      reason_codes = "ambiguous_checkpoint_candidates",
      compatible = FALSE,
      selected_sequence = highest
    ))
  }

  selected <- finalists[[order(ids, method = "radix")[[1L]]]]
  new_recovery_decision(
    plan_id = selected$plan_id,
    checkpoint_id = selected$checkpoint_id,
    action = "resume",
    reason_codes = "highest_valid_checkpoint_selected",
    compatible = TRUE,
    selected_sequence = selected$sequence
  )
}
