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
#' @param requirements Module requirements from [new_module_resource_requirements()].
#' @param policy Execution resource policy.
#' @return A `PopgenVCFExecutionAdmissionDecision` object.
#' @export
admit_execution_resources <- function(requirements,
                                      policy = new_execution_resource_policy()) {
  if (!inherits(policy, "PopgenVCFExecutionResourcePolicy")) {
    stop("policy must be a PopgenVCFExecutionResourcePolicy", call. = FALSE)
  }
  required_names <- c("threads", "memory_mb", "temp_mb", "processes")
  if (!is.numeric(requirements) || !identical(names(requirements), required_names) || anyNA(requirements)) {
    stop("requirements must be a validated module resource vector", call. = FALSE)
  }
  capacity <- unlist(policy[required_names], use.names = TRUE)
  exceeded <- required_names[requirements > capacity]
  admitted <- !length(exceeded)
  structure(
    list(
      admitted = admitted,
      status = if (admitted) "admitted" else "resource_unavailable",
      exceeded = exceeded,
      requirements = requirements,
      capacity = capacity,
      policy = policy$label
    ),
    class = "PopgenVCFExecutionAdmissionDecision"
  )
}
