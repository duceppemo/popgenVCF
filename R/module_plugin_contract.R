#' Canonical module plugin descriptor
#'
#' Constructs and validates the versioned public contract used by analysis
#' modules registered with the popgenVCF execution runtime.
#'
#' @param id Stable lower-case module identifier.
#' @param version Semantic module version.
#' @param dependencies Character vector of module identifiers.
#' @param inputs Character vector of declared input object types.
#' @param outputs Character vector of declared output artifact types.
#' @param execute Name of the execution entry point.
#' @param validate Name of the result-validation entry point.
#' @param lifecycle One of `stable`, `experimental`, or `deprecated`.
#' @param deterministic Whether equivalent inputs and configuration must yield
#'   scientifically equivalent canonical results.
#' @param schema_version Plugin contract schema version.
#'
#' @return A validated `popgen_module_plugin` descriptor.
#' @export
new_module_plugin <- function(
    id,
    version,
    dependencies = character(),
    inputs = character(),
    outputs,
    execute,
    validate,
    lifecycle = "experimental",
    deterministic = TRUE,
    schema_version = 1L) {
  descriptor <- list(
    schema_version = schema_version,
    id = id,
    version = version,
    dependencies = sort(unique(dependencies)),
    inputs = sort(unique(inputs)),
    outputs = sort(unique(outputs)),
    execute = execute,
    validate = validate,
    lifecycle = lifecycle,
    deterministic = deterministic
  )

  validate_module_plugin(descriptor)
  class(descriptor) <- c("popgen_module_plugin", "list")
  descriptor
}

#' Validate a module plugin descriptor
#'
#' @param x Candidate descriptor.
#' @return `x`, invisibly, when valid.
#' @export
validate_module_plugin <- function(x) {
  required <- c(
    "schema_version", "id", "version", "dependencies", "inputs", "outputs",
    "execute", "validate", "lifecycle", "deterministic"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Module plugin descriptor is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported module plugin schema version.", call. = FALSE)
  }
  scalar_text <- function(value, field) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop("`", field, "` must be one non-empty string.", call. = FALSE)
    }
  }
  scalar_text(x$id, "id")
  if (!grepl("^[a-z][a-z0-9_]*$", x$id)) {
    stop("`id` must use canonical lower-case snake-case syntax.", call. = FALSE)
  }
  scalar_text(x$version, "version")
  scalar_text(x$execute, "execute")
  scalar_text(x$validate, "validate")
  if (!x$lifecycle %in% c("stable", "experimental", "deprecated")) {
    stop("Unsupported module lifecycle classification.", call. = FALSE)
  }
  if (!is.logical(x$deterministic) || length(x$deterministic) != 1L || is.na(x$deterministic)) {
    stop("`deterministic` must be one non-missing logical value.", call. = FALSE)
  }
  for (field in c("dependencies", "inputs", "outputs")) {
    value <- x[[field]]
    if (!is.character(value) || anyNA(value) || any(!nzchar(value)) || anyDuplicated(value)) {
      stop("`", field, "` must contain unique non-empty strings.", call. = FALSE)
    }
  }
  if (!length(x$outputs)) {
    stop("A module plugin must declare at least one output.", call. = FALSE)
  }
  if (x$id %in% x$dependencies) {
    stop("A module plugin cannot depend on itself.", call. = FALSE)
  }
  invisible(x)
}
