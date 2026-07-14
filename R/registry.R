#' Create an analysis registry
#'
#' The registry stores analysis module contracts and resolves their dependency
#' graph before execution. It is intentionally independent of package-global
#' state so applications and tests can construct isolated registries.
#'
#' @return An empty `PopgenVCFRegistry` object.
#' @export
new_analysis_registry <- function() {
  structure(list(modules = list()), class = "PopgenVCFRegistry")
}

#' Register an analysis module
#'
#' @param registry A `PopgenVCFRegistry` object.
#' @param name Unique module name.
#' @param run Function accepting `(analysis, context)` and returning a list with
#'   updated `analysis` and `context` elements.
#' @param requires Names of prerequisite modules.
#' @param enabled Logical value or function accepting the validated config.
#' @param description Short module description.
#' @param validate Validator accepting `(result, analysis, context)`.
#' @param outputs Declared analysis-result names.
#' @param references Scientific references supporting the module.
#' @param resource_class One of `light`, `standard`, `heavy`, or `external`.
#' @param contract_version Module-contract version.
#' @return The updated registry.
#' @export
register_analysis <- function(registry, name, run, requires = character(), enabled = TRUE,
                              description = "", validate = validate_module_result,
                              outputs = name, references = character(),
                              resource_class = "standard", contract_version = "1.0") {
  if (!inherits(registry, "PopgenVCFRegistry")) stop("registry must be a PopgenVCFRegistry", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) stop("module name must be one non-empty string", call. = FALSE)
  if (!is.function(run)) stop("run must be a function", call. = FALSE)
  if (!is.logical(enabled) && !is.function(enabled)) stop("enabled must be logical or a function", call. = FALSE)
  if (!is.function(validate)) stop("validate must be a function", call. = FALSE)
  if (!resource_class %in% c("light", "standard", "heavy", "external")) stop("invalid resource_class", call. = FALSE)
  requires <- unique(as.character(requires))
  if (name %in% requires) stop("a module cannot require itself", call. = FALSE)
  registry$modules[[name]] <- list(
    name = name, run = run, requires = requires, enabled = enabled,
    description = as.character(description)[1], validate = validate,
    outputs = unique(as.character(outputs)), references = as.character(references),
    resource_class = resource_class, contract_version = as.character(contract_version)[1],
    artifacts = character(), artifacts_must_exist = FALSE
  )
  registry
}

#' List registered analysis modules
#' @param registry A `PopgenVCFRegistry` object.
#' @return A data table describing registered modules.
#' @export
list_analyses <- function(registry) {
  if (!inherits(registry, "PopgenVCFRegistry")) stop("registry must be a PopgenVCFRegistry", call. = FALSE)
  if (!length(registry$modules)) return(data.table::data.table())
  data.table::rbindlist(lapply(registry$modules, function(x) {
    data.table::data.table(
      name = x$name,
      requires = paste(x$requires, collapse = ","),
      description = x$description,
      outputs = paste(x$outputs, collapse = ","),
      artifacts = paste(x$artifacts %||% character(), collapse = ","),
      artifacts_must_exist = isTRUE(x$artifacts_must_exist),
      resource_class = x$resource_class,
      contract_version = x$contract_version,
      references = paste(x$references, collapse = "; ")
    )
  }))
}

module_is_enabled <- function(module, config) {
  if (is.function(module$enabled)) isTRUE(module$enabled(config)) else isTRUE(module$enabled)
}

#' Resolve module execution order
#'
#' @param registry A `PopgenVCFRegistry` object.
#' @param config Validated configuration.
#' @param selected Optional module names. Enabled modules are used when omitted.
#' @return Character vector in topological execution order.
#' @export
resolve_analysis_order <- function(registry, config, selected = NULL) {
  modules <- registry$modules
  if (!length(modules)) return(character())
  if (is.null(selected)) {
    selected <- names(modules)[vapply(modules, module_is_enabled, logical(1), config = config)]
  }
  unknown <- setdiff(selected, names(modules))
  if (length(unknown)) stop("Unknown analysis module(s): ", paste(unknown, collapse = ", "), call. = FALSE)

  closure <- character()
  add_with_dependencies <- function(name, trail = character()) {
    if (name %in% trail) stop("Circular analysis dependency: ", paste(c(trail, name), collapse = " -> "), call. = FALSE)
    if (name %in% closure) return(invisible(NULL))
    req <- modules[[name]]$requires
    missing <- setdiff(req, names(modules))
    if (length(missing)) stop("Module '", name, "' requires unregistered module(s): ", paste(missing, collapse = ", "), call. = FALSE)
    for (dep in req) add_with_dependencies(dep, c(trail, name))
    closure <<- c(closure, name)
    invisible(NULL)
  }
  for (name in selected) add_with_dependencies(name)
  unique(closure)
}

#' Execute registered analysis modules
#'
#' Module runners may optionally return an `artifacts` element containing a
#' `PopgenVCFArtifactManifest`. Declared artifacts are validated after the
#' statistical result and accumulated into the returned manifest.
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context shared by module runners.
#' @param registry A `PopgenVCFRegistry` object.
#' @param selected Optional module names.
#' @return A list containing updated `analysis`, `context`, execution `order`,
#'   and the combined `artifacts` manifest.
#' @export
execute_analysis_registry <- function(analysis, context, registry, selected = NULL) {
  validate_analysis(analysis, "ordination")
  order <- resolve_analysis_order(registry, analysis$config, selected)
  artifacts <- new_artifact_manifest()
  for (name in order) {
    module <- registry$modules[[name]]
    t0 <- proc.time()[["elapsed"]]
    out <- run_stage(name, module$run(analysis, context))
    if (!is.list(out) || is.null(out$analysis) || is.null(out$context)) {
      stop("Analysis module '", name, "' returned an invalid result", call. = FALSE)
    }
    candidate <- out$analysis
    context <- out$context
    missing_outputs <- setdiff(module$outputs, names(candidate$results))
    if (length(missing_outputs)) {
      stop("Module '", name, "' did not produce declared output(s): ",
           paste(missing_outputs, collapse = ", "), call. = FALSE)
    }
    validation <- module$validate(candidate$results[[module$outputs[[1]]]], candidate, context)
    assert_module_validation(validation, name)

    module_artifacts <- module_artifact_manifest(out)
    validate_module_artifacts(
      module_name = name,
      declared = module$artifacts %||% character(),
      manifest = module_artifacts,
      must_exist = isTRUE(module$artifacts_must_exist)
    )
    artifacts <- append_artifact_manifest(artifacts, module_artifacts)

    candidate$results$validation <- candidate$results$validation %||% list()
    candidate$results$validation[[name]] <- validation
    analysis <- candidate
    analysis <- record_analysis_timing(analysis, name, proc.time()[["elapsed"]] - t0)
    analysis <- record_analysis_message(analysis, "SUCCESS", name, "completed and validated")
    validate_analysis(analysis)
  }
  list(analysis = analysis, context = context, order = order, artifacts = artifacts)
}
