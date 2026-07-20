#' Create a canonical module execution plan
#'
#' Constructs a deterministic, machine-readable execution plan for a Phase 9
#' analysis module. The plan records the module contract, normalized parameters,
#' scientific inputs, expected outputs, dependency identities, resource requests,
#' cache policy, and execution environment identity.
#'
#' @param module_id Stable module identifier.
#' @param module_version Semantic module version.
#' @param parameters Named list of normalized module parameters.
#' @param inputs Named list of canonical scientific-object identities.
#' @param outputs Named list of expected canonical output identities.
#' @param dependencies Character vector of execution-plan identifiers.
#' @param resources Named list of requested execution resources.
#' @param cache_policy One of `use`, `refresh`, or `bypass`.
#' @param environment Named list describing the execution environment.
#'
#' @return A `popgen_execution_plan` object.
#' @export
new_execution_plan <- function(module_id,
                               module_version,
                               parameters = list(),
                               inputs = list(),
                               outputs = list(),
                               dependencies = character(),
                               resources = list(),
                               cache_policy = "use",
                               environment = list()) {
  module_id <- validate_execution_scalar(module_id, "module_id")
  module_version <- validate_execution_scalar(module_version, "module_version")
  cache_policy <- match.arg(cache_policy, c("use", "refresh", "bypass"))

  parameters <- canonical_named_execution_list(parameters, "parameters")
  inputs <- canonical_named_execution_list(inputs, "inputs")
  outputs <- canonical_named_execution_list(outputs, "outputs")
  resources <- canonical_named_execution_list(resources, "resources")
  environment <- canonical_named_execution_list(environment, "environment")

  dependencies <- sort(unique(as.character(dependencies)))
  if (anyNA(dependencies) || any(!nzchar(dependencies))) {
    stop("`dependencies` must contain non-empty execution-plan identifiers.",
         call. = FALSE)
  }

  plan <- list(
    schema_id = "popgenVCF.execution-plan",
    schema_version = "1.0.0",
    module_id = module_id,
    module_version = module_version,
    parameters = parameters,
    inputs = inputs,
    outputs = outputs,
    dependencies = dependencies,
    resources = resources,
    cache_policy = cache_policy,
    environment = environment
  )

  plan$plan_id <- execution_plan_fingerprint(plan)
  class(plan) <- c("popgen_execution_plan", "list")
  plan
}

#' Validate a module execution plan
#'
#' @param plan Execution plan to validate.
#' @param known_dependencies Optional character vector of known plan identifiers.
#'
#' @return A structured `popgen_execution_validation` object.
#' @export
validate_execution_plan <- function(plan, known_dependencies = NULL) {
  errors <- character()
  warnings <- character()

  if (!inherits(plan, "popgen_execution_plan")) {
    errors <- c(errors, "Object does not inherit from `popgen_execution_plan`.")
  }

  required <- c(
    "schema_id", "schema_version", "module_id", "module_version",
    "parameters", "inputs", "outputs", "dependencies", "resources",
    "cache_policy", "environment", "plan_id"
  )
  missing_fields <- setdiff(required, names(plan))
  if (length(missing_fields)) {
    errors <- c(errors, paste0(
      "Missing required field(s): ", paste(sort(missing_fields), collapse = ", "), "."
    ))
  }

  if (!length(missing_fields)) {
    expected_id <- execution_plan_fingerprint(plan[names(plan) != "plan_id"])
    if (!identical(plan$plan_id, expected_id)) {
      errors <- c(errors, "Execution-plan fingerprint does not match plan content.")
    }

    if (!plan$cache_policy %in% c("use", "refresh", "bypass")) {
      errors <- c(errors, "Unknown cache policy.")
    }

    if (!is.null(known_dependencies)) {
      unknown <- setdiff(plan$dependencies, as.character(known_dependencies))
      if (length(unknown)) {
        errors <- c(errors, paste0(
          "Unknown execution-plan dependencies: ",
          paste(sort(unknown), collapse = ", "), "."
        ))
      }
    }
  }

  structure(
    list(
      valid = !length(errors),
      errors = sort(unique(errors)),
      warnings = sort(unique(warnings)),
      plan_id = if (!is.null(plan$plan_id)) plan$plan_id else NA_character_
    ),
    class = c("popgen_execution_validation", "list")
  )
}

#' Create an execution state-transition record
#'
#' @param plan_id Stable execution-plan identifier.
#' @param from Previous state.
#' @param to New state.
#' @param sequence Positive transition sequence number.
#' @param details Optional named transition metadata.
#'
#' @return A `popgen_execution_transition` object.
#' @export
new_execution_transition <- function(plan_id,
                                     from,
                                     to,
                                     sequence,
                                     details = list()) {
  states <- c(
    "planned", "validated", "running", "completed", "failed",
    "cancelled", "cached", "resumed"
  )
  plan_id <- validate_execution_scalar(plan_id, "plan_id")
  from <- match.arg(from, states)
  to <- match.arg(to, states)

  sequence <- as.integer(sequence)
  if (length(sequence) != 1L || is.na(sequence) || sequence < 1L) {
    stop("`sequence` must be one positive integer.", call. = FALSE)
  }

  allowed <- list(
    planned = c("validated", "cancelled"),
    validated = c("running", "cached", "cancelled"),
    running = c("completed", "failed", "cancelled"),
    failed = c("resumed"),
    resumed = c("running", "failed", "cancelled"),
    completed = character(),
    cached = character(),
    cancelled = character()
  )

  if (!to %in% allowed[[from]]) {
    stop(sprintf("Invalid execution transition: %s -> %s.", from, to),
         call. = FALSE)
  }

  record <- list(
    schema_id = "popgenVCF.execution-transition",
    schema_version = "1.0.0",
    plan_id = plan_id,
    sequence = sequence,
    from = from,
    to = to,
    details = canonical_named_execution_list(details, "details")
  )
  record$transition_id <- execution_plan_fingerprint(record)
  class(record) <- c("popgen_execution_transition", "list")
  record
}

execution_plan_fingerprint <- function(x) {
  raw <- serialize(canonicalize_execution_value(x), NULL, version = 3)
  paste0("sha256-placeholder-", sprintf("%08x", sum(as.integer(raw)) %% 2^31))
}

canonicalize_execution_value <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      x <- x[order(names(x), method = "radix")]
    }
    return(lapply(x, canonicalize_execution_value))
  }
  if (is.character(x)) {
    return(enc2utf8(x))
  }
  x
}

canonical_named_execution_list <- function(x, field) {
  if (!is.list(x)) {
    stop(sprintf("`%s` must be a list.", field), call. = FALSE)
  }
  if (!length(x)) {
    return(list())
  }
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop(sprintf("`%s` must be uniquely named with non-empty names.", field),
         call. = FALSE)
  }
  x[order(nms, method = "radix")]
}

validate_execution_scalar <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be one non-empty character value.", field),
         call. = FALSE)
  }
  enc2utf8(x)
}
