#' Create a self-contained analysis module descriptor
#'
#' A module descriptor owns the complete registry contract for one analysis:
#' execution, dependencies, enablement, validation, result names, scientific
#' references, resource class, parallel-safety declaration, and required
#' publication artifacts.
#'
#' @param name Unique module name.
#' @param run Function accepting `(analysis, context)`.
#' @param requires Prerequisite module names.
#' @param enabled Logical value or configuration predicate.
#' @param description Short module description.
#' @param validate Statistical-result validator.
#' @param outputs Declared analysis result names.
#' @param references Scientific references.
#' @param resource_class One of `light`, `standard`, `heavy`, or `external`.
#' @param parallel_safe Whether the module may run concurrently with independent
#'   modules without modifying the shared runtime context.
#' @param contract_version Module contract version.
#' @param artifacts Required artifact identifiers, without the module prefix.
#' @param artifacts_must_exist Whether artifact files must exist after execution.
#' @return A `PopgenVCFModuleSpec` object.
#' @export
new_analysis_module_spec <- function(name, run, requires = character(), enabled = TRUE,
                                     description = "", validate = validate_module_result,
                                     outputs = name, references = character(),
                                     resource_class = "standard", parallel_safe = FALSE,
                                     contract_version = "1.0", artifacts = character(),
                                     artifacts_must_exist = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("module name must be one non-empty string", call. = FALSE)
  }
  if (!is.function(run)) stop("run must be a function", call. = FALSE)
  if (!is.logical(enabled) && !is.function(enabled)) {
    stop("enabled must be logical or a function", call. = FALSE)
  }
  if (!is.function(validate)) stop("validate must be a function", call. = FALSE)
  if (!resource_class %in% c("light", "standard", "heavy", "external")) {
    stop("invalid resource_class", call. = FALSE)
  }
  if (!is.logical(parallel_safe) || length(parallel_safe) != 1L || is.na(parallel_safe)) {
    stop("parallel_safe must be TRUE or FALSE", call. = FALSE)
  }

  requires <- unique(as.character(requires))
  if (name %in% requires) stop("a module cannot require itself", call. = FALSE)
  outputs <- unique(as.character(outputs))
  artifacts <- unique(as.character(artifacts))
  if (any(!nzchar(outputs))) stop("outputs must contain non-empty names", call. = FALSE)
  if (any(!nzchar(artifacts))) stop("artifacts must contain non-empty names", call. = FALSE)

  structure(
    list(
      name = name,
      run = run,
      requires = requires,
      enabled = enabled,
      description = as.character(description)[1L],
      validate = validate,
      outputs = outputs,
      references = as.character(references),
      resource_class = resource_class,
      parallel_safe = isTRUE(parallel_safe),
      contract_version = as.character(contract_version)[1L],
      artifacts = artifacts,
      artifacts_must_exist = isTRUE(artifacts_must_exist)
    ),
    class = "PopgenVCFModuleSpec"
  )
}

#' Register a self-contained analysis module descriptor
#'
#' @param registry A `PopgenVCFRegistry` object.
#' @param module A `PopgenVCFModuleSpec` object.
#' @return The updated registry.
#' @export
register_analysis_module <- function(registry, module) {
  if (!inherits(module, "PopgenVCFModuleSpec")) {
    stop("module must be a PopgenVCFModuleSpec", call. = FALSE)
  }

  registry <- register_analysis(
    registry = registry,
    name = module$name,
    run = module$run,
    requires = module$requires,
    enabled = module$enabled,
    description = module$description,
    validate = module$validate,
    outputs = module$outputs,
    references = module$references,
    resource_class = module$resource_class,
    parallel_safe = module$parallel_safe,
    contract_version = module$contract_version
  )

  if (length(module$artifacts)) {
    registry <- register_analysis_artifacts(
      registry,
      module$name,
      module$artifacts,
      must_exist = module$artifacts_must_exist
    )
  }
  registry
}

#' @export
print.PopgenVCFModuleSpec <- function(x, ...) {
  cat("<PopgenVCFModuleSpec>", x$name, "\n")
  cat("  requires:", if (length(x$requires)) paste(x$requires, collapse = ", ") else "none", "\n")
  cat("  outputs:", paste(x$outputs, collapse = ", "), "\n")
  cat("  parallel safe:", x$parallel_safe, "\n")
  cat("  artifacts:", if (length(x$artifacts)) paste(x$artifacts, collapse = ", ") else "none", "\n")
  invisible(x)
}
