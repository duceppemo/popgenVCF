#' Create an execution resource policy
#'
#' Resource policies describe capacity available to the deterministic execution
#' runtime. They govern admission only; operating-system enforcement belongs to
#' supervised external-process execution.
#'
#' @param threads Maximum admitted threads.
#' @param memory_mb Maximum admitted memory in MiB.
#' @param temp_mb Maximum admitted temporary storage in MiB.
#' @param processes Maximum admitted processes.
#' @param label Stable policy identifier recorded in execution metadata.
#' @return A validated `PopgenVCFExecutionResourcePolicy` object.
#' @export
new_execution_resource_policy <- function(threads = 1L,
                                          memory_mb = Inf,
                                          temp_mb = Inf,
                                          processes = 1L,
                                          label = "default-resource-policy") {
  values <- c(
    threads = as.numeric(threads)[1],
    memory_mb = as.numeric(memory_mb)[1],
    temp_mb = as.numeric(temp_mb)[1],
    processes = as.numeric(processes)[1]
  )
  if (anyNA(values) || any(values <= 0)) {
    stop("resource capacities must be positive or Inf", call. = FALSE)
  }
  if (!isTRUE(all.equal(values[["threads"]], floor(values[["threads"]]))) ||
      !isTRUE(all.equal(values[["processes"]], floor(values[["processes"]])))) {
    stop("threads and processes must be whole numbers", call. = FALSE)
  }
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }
  structure(
    list(
      threads = as.integer(values[["threads"]]),
      memory_mb = values[["memory_mb"]],
      temp_mb = values[["temp_mb"]],
      processes = as.integer(values[["processes"]]),
      label = label
    ),
    class = "PopgenVCFExecutionResourcePolicy"
  )
}

#' Create module resource requirements
#'
#' @param threads Required threads.
#' @param memory_mb Required memory in MiB.
#' @param temp_mb Required temporary storage in MiB.
#' @param processes Required processes.
#' @return A validated named numeric vector.
#' @export
new_module_resource_requirements <- function(threads = 1L,
                                             memory_mb = 0,
                                             temp_mb = 0,
                                             processes = 1L) {
  values <- c(
    threads = as.numeric(threads)[1],
    memory_mb = as.numeric(memory_mb)[1],
    temp_mb = as.numeric(temp_mb)[1],
    processes = as.numeric(processes)[1]
  )
  if (anyNA(values) || values[["threads"]] < 1 || values[["processes"]] < 1 ||
      values[["memory_mb"]] < 0 || values[["temp_mb"]] < 0) {
    stop("resource requirements must be non-negative with at least one thread and process", call. = FALSE)
  }
  if (!isTRUE(all.equal(values[["threads"]], floor(values[["threads"]]))) ||
      !isTRUE(all.equal(values[["processes"]], floor(values[["processes"]])))) {
    stop("required threads and processes must be whole numbers", call. = FALSE)
  }
  values
}

#' Evaluate deterministic resource admission
#'
#' Each call is independent and stateless: it only checks `requirements`
#' against `policy`'s fixed capacity, exactly as documented above. Callers
#' that admit multiple overlapping (not-yet-finalized) requests against the
#' same policy -- as [start_supervised_external_command()] does -- must pass
#' `in_use` themselves to have those already-admitted requests actually
#' count against capacity; nothing here tracks that automatically.
#'
#' @param requirements Module requirements from [new_module_resource_requirements()].
#' @param policy Execution resource policy.
#' @param in_use Resources already committed against `policy` by other
#'   still-running admissions, as a named vector with the same names as
#'   `requirements` (default: none). Subtracted from `policy`'s capacity
#'   before checking `requirements`.
#' @return A `PopgenVCFExecutionAdmissionDecision` object.
#' @export
admit_execution_resources <- function(requirements,
                                      policy = new_execution_resource_policy(),
                                      in_use = c(threads = 0, memory_mb = 0, temp_mb = 0, processes = 0)) {
  if (!inherits(policy, "PopgenVCFExecutionResourcePolicy")) {
    stop("policy must be a PopgenVCFExecutionResourcePolicy", call. = FALSE)
  }
  required_names <- c("threads", "memory_mb", "temp_mb", "processes")
  if (!is.numeric(requirements) || !identical(names(requirements), required_names) || anyNA(requirements)) {
    stop("requirements must be a validated module resource vector", call. = FALSE)
  }
  if (!is.numeric(in_use) || !identical(names(in_use), required_names) || anyNA(in_use) || any(in_use < 0)) {
    stop("in_use must be a non-negative resource vector", call. = FALSE)
  }
  capacity <- unlist(policy[required_names], use.names = TRUE)
  available <- capacity - in_use
  exceeded <- required_names[requirements > available]
  admitted <- !length(exceeded)
  structure(
    list(
      admitted = admitted,
      status = if (admitted) "admitted" else "resource_unavailable",
      exceeded = exceeded,
      requirements = requirements,
      capacity = capacity,
      in_use = in_use,
      available = available,
      policy = policy$label
    ),
    class = "PopgenVCFExecutionAdmissionDecision"
  )
}
