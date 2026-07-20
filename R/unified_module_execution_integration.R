# Unified Phase 9 module execution integration
#
# Phase 9.8 introduces the deterministic boundary that connects the canonical
# Phase 9 contracts to the established Phase 8 runtime. The initial contract is
# deliberately independent of executor implementation details so validation can
# fail before scientific work is dispatched.

.integration_scalar_character <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be one non-empty character value.", field), call. = FALSE)
  }
  x
}

.integration_named_character <- function(x, field, allow_empty = TRUE) {
  if (!is.character(x) || anyNA(x)) {
    stop(sprintf("`%s` must be a named character vector without missing values.", field), call. = FALSE)
  }
  if (!length(x) && allow_empty) {
    return(setNames(character(), character()))
  }
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    stop(sprintf("`%s` must have non-empty, non-missing names.", field), call. = FALSE)
  }
  if (any(!nzchar(x)) || anyDuplicated(nms)) {
    stop(sprintf("`%s` must contain unique named non-empty values.", field), call. = FALSE)
  }
  x[order(nms, method = "radix")]
}

.integration_canonicalize <- function(x, field) {
  if (!is.list(x)) {
    stop(sprintf("`%s` must be a list.", field), call. = FALSE)
  }
  if (!length(x)) {
    return(list())
  }
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop(sprintf("`%s` must be a uniquely named list.", field), call. = FALSE)
  }
  x[order(nms, method = "radix")]
}

.integration_digest <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

#' Create a unified module execution request
#'
#' Construct the canonical Phase 9 request that binds a module plugin to
#' scientific inputs, schemas, execution parameters, cache policy, resource
#' policy, and runtime environment identity before dispatch to the Phase 8
#' execution engine.
#'
#' @param module_plugin A validated `popgen_module_plugin` descriptor.
#' @param scientific_inputs Named scientific-object fingerprints.
#' @param schema_bindings Named schema identifiers for declared inputs and
#'   outputs.
#' @param parameters Uniquely named normalized module parameters.
#' @param cache_policy One of `use`, `refresh`, or `bypass`.
#' @param resource_policy Uniquely named resource-policy values.
#' @param environment_fingerprint Canonical runtime-environment fingerprint.
#' @param request_metadata Optional uniquely named non-scientific metadata.
#'
#' @return A validated `popgenvcf_module_execution_request` object.
new_module_execution_integration_request <- function(
    module_plugin,
    scientific_inputs,
    schema_bindings,
    parameters = list(),
    cache_policy = "use",
    resource_policy = list(),
    environment_fingerprint,
    request_metadata = list()) {
  validate_module_plugin(module_plugin)
  scientific_inputs <- .integration_named_character(
    scientific_inputs,
    "scientific_inputs",
    allow_empty = FALSE
  )
  schema_bindings <- .integration_named_character(
    schema_bindings,
    "schema_bindings",
    allow_empty = FALSE
  )
  parameters <- .integration_canonicalize(parameters, "parameters")
  resource_policy <- .integration_canonicalize(resource_policy, "resource_policy")
  request_metadata <- .integration_canonicalize(request_metadata, "request_metadata")
  cache_policy <- match.arg(
    .integration_scalar_character(cache_policy, "cache_policy"),
    c("use", "refresh", "bypass")
  )
  environment_fingerprint <- .integration_scalar_character(
    environment_fingerprint,
    "environment_fingerprint"
  )

  request <- list(
    contract = "popgenvcf.module-execution-integration-request",
    contract_version = "1.0.0",
    module_id = module_plugin$id,
    module_version = module_plugin$version,
    module_contract_fingerprint = .integration_digest(unclass(module_plugin)),
    deterministic = isTRUE(module_plugin$deterministic),
    scientific_inputs = scientific_inputs,
    schema_bindings = schema_bindings,
    parameters = parameters,
    cache_policy = cache_policy,
    resource_policy = resource_policy,
    environment_fingerprint = environment_fingerprint,
    request_metadata = request_metadata
  )
  request$request_id <- paste0("integration-request:", .integration_digest(request))
  request$fingerprint <- .integration_digest(request)
  class(request) <- c("popgenvcf_module_execution_request", "list")
  validate_module_execution_integration_request(request)
  request
}

#' Validate a unified module execution request
#'
#' @param request Candidate integration request.
#' @param module_plugin Optional current plugin descriptor used to detect
#'   module-contract drift.
#' @param environment_fingerprint Optional current environment fingerprint.
#'
#' @return A structured fail-closed validation report.
validate_module_execution_integration_request <- function(
    request,
    module_plugin = NULL,
    environment_fingerprint = NULL) {
  errors <- character()
  required <- c(
    "contract", "contract_version", "module_id", "module_version",
    "module_contract_fingerprint", "deterministic", "scientific_inputs",
    "schema_bindings", "parameters", "cache_policy", "resource_policy",
    "environment_fingerprint", "request_metadata", "request_id", "fingerprint"
  )

  if (!inherits(request, "popgenvcf_module_execution_request")) {
    errors <- c(errors, "integration_request_class_invalid")
  }
  missing_fields <- setdiff(required, names(request))
  if (length(missing_fields)) {
    errors <- c(errors, paste0("missing_field:", sort(missing_fields, method = "radix")))
  }

  if (!length(missing_fields)) {
    if (!identical(request$contract, "popgenvcf.module-execution-integration-request")) {
      errors <- c(errors, "integration_request_contract_invalid")
    }
    if (!identical(request$contract_version, "1.0.0")) {
      errors <- c(errors, "integration_request_version_unsupported")
    }
    if (!request$cache_policy %in% c("use", "refresh", "bypass")) {
      errors <- c(errors, "integration_request_cache_policy_invalid")
    }
    if (!is.logical(request$deterministic) || length(request$deterministic) != 1L ||
        is.na(request$deterministic)) {
      errors <- c(errors, "integration_request_determinism_invalid")
    }

    candidate <- request
    observed_fingerprint <- candidate$fingerprint
    observed_request_id <- candidate$request_id
    candidate$fingerprint <- NULL
    candidate$request_id <- NULL
    class(candidate) <- "list"
    expected_request_id <- paste0("integration-request:", .integration_digest(candidate))
    candidate$request_id <- expected_request_id
    expected_fingerprint <- .integration_digest(candidate)
    if (!identical(observed_request_id, expected_request_id)) {
      errors <- c(errors, "integration_request_id_mismatch")
    }
    if (!identical(observed_fingerprint, expected_fingerprint)) {
      errors <- c(errors, "integration_request_fingerprint_mismatch")
    }
  }

  if (!is.null(module_plugin)) {
    plugin_error <- tryCatch({
      validate_module_plugin(module_plugin)
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(plugin_error)) {
      errors <- c(errors, "module_plugin_invalid")
    } else if (!length(missing_fields)) {
      expected_plugin_fingerprint <- .integration_digest(unclass(module_plugin))
      if (!identical(request$module_id, module_plugin$id)) {
        errors <- c(errors, "module_identity_mismatch")
      }
      if (!identical(request$module_version, module_plugin$version)) {
        errors <- c(errors, "module_version_mismatch")
      }
      if (!identical(request$module_contract_fingerprint, expected_plugin_fingerprint)) {
        errors <- c(errors, "module_contract_drift")
      }
    }
  }

  if (!is.null(environment_fingerprint) && !length(missing_fields)) {
    environment_fingerprint <- .integration_scalar_character(
      environment_fingerprint,
      "environment_fingerprint"
    )
    if (!identical(request$environment_fingerprint, environment_fingerprint)) {
      errors <- c(errors, "runtime_environment_mismatch")
    }
  }

  errors <- sort(unique(errors), method = "radix")
  structure(
    list(
      valid = length(errors) == 0L,
      decision = if (length(errors)) "reject" else "accept",
      errors = errors
    ),
    class = c("popgenvcf_module_execution_request_validation", "list")
  )
}

#' Create a unified module execution integration record
#'
#' Bind deterministic identities emitted by the Phase 9 planning, caching,
#' recovery, result, provenance, and publication boundaries to one Phase 8
#' runtime execution identity.
#'
#' @param request_id Integration request identifier.
#' @param plan_id Phase 9 execution-plan identifier.
#' @param runtime_execution_fingerprint Phase 8 execution or replay fingerprint.
#' @param cache_decision_id Deterministic cache decision identifier.
#' @param result_id Canonical Phase 9 result identifier.
#' @param recovery_decision_id Optional recovery-decision identifier.
#' @param provenance_fingerprint Provenance graph fingerprint.
#' @param publication_fingerprint Publication artifact or bundle fingerprint.
#' @param status One of `success`, `failed`, `cancelled`, or `rejected`.
#'
#' @return A canonical `popgenvcf_module_execution_integration` record.
new_module_execution_integration_record <- function(
    request_id,
    plan_id,
    runtime_execution_fingerprint,
    cache_decision_id,
    result_id,
    recovery_decision_id = NULL,
    provenance_fingerprint,
    publication_fingerprint,
    status) {
  fields <- list(
    request_id = request_id,
    plan_id = plan_id,
    runtime_execution_fingerprint = runtime_execution_fingerprint,
    cache_decision_id = cache_decision_id,
    result_id = result_id,
    provenance_fingerprint = provenance_fingerprint,
    publication_fingerprint = publication_fingerprint
  )
  fields <- lapply(names(fields), function(name) {
    .integration_scalar_character(fields[[name]], name)
  })
  names(fields) <- c(
    "request_id", "plan_id", "runtime_execution_fingerprint",
    "cache_decision_id", "result_id", "provenance_fingerprint",
    "publication_fingerprint"
  )
  if (!is.null(recovery_decision_id)) {
    recovery_decision_id <- .integration_scalar_character(
      recovery_decision_id,
      "recovery_decision_id"
    )
  }
  status <- match.arg(
    .integration_scalar_character(status, "status"),
    c("success", "failed", "cancelled", "rejected")
  )

  record <- c(
    list(
      contract = "popgenvcf.module-execution-integration",
      contract_version = "1.0.0"
    ),
    fields,
    list(recovery_decision_id = recovery_decision_id, status = status)
  )
  record$integration_id <- paste0("integration:", .integration_digest(record))
  record$fingerprint <- .integration_digest(record)
  class(record) <- c("popgenvcf_module_execution_integration", "list")
  record
}
