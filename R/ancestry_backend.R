#' Create an ancestry backend plugin specification
#'
#' @param name Stable backend identifier.
#' @param availability Function returning a logical value or a list with
#'   `available` and `reason`.
#' @param execute Function accepting a single task and returning a canonical
#'   ancestry replicate or backend-native result.
#' @param parse Function converting the execute result and task to a canonical
#'   `PopgenVCFAncestryReplicate`.
#' @param validate Optional backend-specific task validator.
#' @param description Human-readable backend description.
#' @param version Plugin contract version.
#' @return A `PopgenVCFAncestryBackend` object.
#' @export
new_ancestry_backend <- function(name, availability, execute, parse = identity,
                                 validate = function(task) invisible(task),
                                 description = "", version = "1.0") {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("backend name must be one non-empty string", call. = FALSE)
  }
  callbacks <- list(availability = availability, execute = execute,
                    parse = parse, validate = validate)
  if (!all(vapply(callbacks, is.function, logical(1L)))) {
    stop("backend callbacks must be functions", call. = FALSE)
  }
  structure(list(name = tolower(name), description = as.character(description)[1L],
                 version = as.character(version)[1L], callbacks = callbacks),
            class = "PopgenVCFAncestryBackend")
}

#' Create an ancestry backend registry
#' @param backends Optional list of backend specifications.
#' @return A `PopgenVCFAncestryBackendRegistry`.
#' @export
new_ancestry_backend_registry <- function(backends = list()) {
  registry <- structure(list(backends = list()),
                        class = "PopgenVCFAncestryBackendRegistry")
  for (backend in backends) registry <- register_ancestry_backend(registry, backend)
  registry
}

#' Register an ancestry backend
#' @param registry Backend registry.
#' @param backend Backend specification.
#' @param overwrite Replace an existing backend with the same name.
#' @return Updated registry.
#' @export
register_ancestry_backend <- function(registry, backend, overwrite = FALSE) {
  if (!inherits(registry, "PopgenVCFAncestryBackendRegistry")) {
    stop("registry must be a PopgenVCFAncestryBackendRegistry", call. = FALSE)
  }
  if (!inherits(backend, "PopgenVCFAncestryBackend")) {
    stop("backend must be a PopgenVCFAncestryBackend", call. = FALSE)
  }
  if (!isTRUE(overwrite) && backend$name %in% names(registry$backends)) {
    stop(sprintf("ancestry backend '%s' is already registered", backend$name), call. = FALSE)
  }
  registry$backends[[backend$name]] <- backend
  registry
}

#' Inspect ancestry backend availability
#' @param registry Backend registry.
#' @return A data.table with backend availability and reasons.
#' @export
ancestry_backend_status <- function(registry = default_ancestry_backend_registry()) {
  if (!inherits(registry, "PopgenVCFAncestryBackendRegistry")) {
    stop("registry must be a PopgenVCFAncestryBackendRegistry", call. = FALSE)
  }
  data.table::rbindlist(lapply(registry$backends, function(backend) {
    status <- backend$callbacks$availability()
    if (is.logical(status) && length(status) == 1L) {
      status <- list(available = isTRUE(status),
                     reason = if (isTRUE(status)) "available" else "unavailable")
    }
    if (!is.list(status) || length(status$available) != 1L) {
      stop(sprintf("backend '%s' returned an invalid availability result", backend$name), call. = FALSE)
    }
    data.table::data.table(backend = backend$name,
                           available = isTRUE(status$available),
                           reason = as.character(status$reason %||% "")[[1L]],
                           description = backend$description,
                           contract_version = backend$version)
  }), fill = TRUE)
}

#' Run ancestry analyses through one backend-neutral interface
#'
#' @param input Named list of backend inputs, such as `plink_prefix`,
#'   `geno_file`, and `output_dir`.
#' @param sample_ids Sample identifiers in exact Q-matrix row order.
#' @param backend Backend name, vector of names, `"auto"` for the first
#'   available backend, or `"all"` for every available backend.
#' @param k_values Positive integer K values.
#' @param replicates Number of independent replicates per K.
#' @param seed Base deterministic seed.
#' @param registry Backend registry.
#' @param fail_if_none Error when no requested backend is available.
#' @return A list with canonical ancestry results, execution records, and
#'   backend availability status.
#' @export
run_ancestry <- function(input, sample_ids, backend = "auto", k_values = 2:10,
                         replicates = 5L, seed = 42L,
                         registry = default_ancestry_backend_registry(),
                         fail_if_none = TRUE) {
  if (!is.list(input)) stop("input must be a named list", call. = FALSE)
  sample_ids <- as.character(sample_ids)
  if (!length(sample_ids) || anyNA(sample_ids) || anyDuplicated(sample_ids)) {
    stop("sample_ids must be non-empty, unique, and non-missing", call. = FALSE)
  }
  k_values <- sort(unique(as.integer(k_values)))
  if (!length(k_values) || anyNA(k_values) || any(k_values < 2L)) {
    stop("k_values must contain integers greater than or equal to two", call. = FALSE)
  }
  replicates <- as.integer(replicates)[1L]
  if (is.na(replicates) || replicates < 1L) stop("replicates must be positive", call. = FALSE)
  status <- ancestry_backend_status(registry)
  requested <- tolower(as.character(backend))
  if (identical(requested, "auto")) {
    selected <- status[available == TRUE, backend][1L]
  } else if (identical(requested, "all")) {
    selected <- status[available == TRUE, backend]
  } else {
    unknown <- setdiff(requested, status$backend)
    if (length(unknown)) stop(sprintf("unknown ancestry backend(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
    selected <- intersect(requested, status[available == TRUE, backend])
  }
  selected <- selected[!is.na(selected)]
  if (!length(selected)) {
    if (isTRUE(fail_if_none)) stop("no requested ancestry backend is available", call. = FALSE)
    return(list(results = list(), records = data.table::data.table(), status = status))
  }

  results <- list()
  records <- list()
  for (backend_name in selected) {
    spec <- registry$backends[[backend_name]]
    backend_reps <- list()
    for (k in k_values) for (replicate_id in seq_len(replicates)) {
      task_seed <- as.integer(seed + k * 10000L + replicate_id)
      task <- c(input, list(backend = backend_name, k = k,
                            replicate = replicate_id, seed = task_seed,
                            sample_ids = sample_ids))
      spec$callbacks$validate(task)
      started <- proc.time()[[3L]]
      native <- spec$callbacks$execute(task)
      elapsed <- proc.time()[[3L]] - started
      parsed <- spec$callbacks$parse(native, task)
      if (!inherits(parsed, "PopgenVCFAncestryReplicate")) {
        stop(sprintf("backend '%s' parser did not return a canonical ancestry replicate", backend_name), call. = FALSE)
      }
      parsed$runtime_seconds <- as.numeric(elapsed)
      parsed$provenance$backend_plugin <- list(name = spec$name, version = spec$version)
      validate_ancestry_replicate(parsed)
      backend_reps[[length(backend_reps) + 1L]] <- parsed
      records[[length(records) + 1L]] <- data.table::data.table(
        backend = backend_name, k = k, replicate = replicate_id,
        seed = task_seed, runtime_seconds = as.numeric(elapsed), status = "success")
    }
    results[[backend_name]] <- new_ancestry_result(backend_reps)
  }
  list(results = results, records = data.table::rbindlist(records), status = status)
}

ancestry_backend_availability <- function(available, reason) {
  list(available = isTRUE(available), reason = reason)
}

admixture_backend <- function(executable = "admixture") {
  new_ancestry_backend(
    "admixture",
    availability = function() ancestry_backend_availability(nzchar(Sys.which(executable)),
      if (nzchar(Sys.which(executable))) "ADMIXTURE executable found" else "ADMIXTURE executable not found"),
    validate = function(task) {
      if (is.null(task$plink_prefix)) stop("ADMIXTURE backend requires input$plink_prefix", call. = FALSE)
    },
    execute = function(task) {
      output_dir <- task$output_dir %||% tempdir()
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      cv <- run_admixture_cv(executable, task$plink_prefix, task$k,
        threads = task$threads %||% 1L, cv_folds = task$cv_folds %||% 5L,
        output_dir = output_dir, seed = task$seed)
      bed_base <- basename(task$plink_prefix)
      q_path <- file.path(output_dir, sprintf("%s.%d.Q", bed_base, task$k))
      list(cv = cv, q = normalize_q_matrix(data.table::fread(q_path, header = FALSE)))
    },
    parse = function(native, task) new_ancestry_replicate(
      task$sample_ids, native$q, "admixture", k = task$k,
      replicate = task$replicate, metrics = c(cv_error = native$cv$cv_error[[1L]]),
      seed = task$seed, provenance = list(input = task$plink_prefix)),
    description = "ADMIXTURE maximum-likelihood ancestry estimation")
}

faststructure_backend <- function(structure_executable = "structure.py",
                                  choosek_executable = "chooseK.py") {
  new_ancestry_backend(
    "faststructure",
    availability = function() ancestry_backend_availability(nzchar(Sys.which(structure_executable)),
      if (nzchar(Sys.which(structure_executable))) "fastStructure executable found" else "fastStructure executable not found"),
    validate = function(task) {
      if (is.null(task$plink_prefix)) stop("fastStructure backend requires input$plink_prefix", call. = FALSE)
    },
    execute = function(task) run_faststructure(structure_executable, choosek_executable,
      task$plink_prefix, task$k, task$output_dir %||% tempdir(), task$seed),
    parse = function(native, task) new_ancestry_replicate(
      task$sample_ids, native$q[[as.character(task$k)]], "faststructure",
      k = task$k, replicate = task$replicate, seed = task$seed,
      provenance = list(input = task$plink_prefix, suggested_k = native$suggested_k)),
    description = "fastStructure variational Bayesian ancestry estimation")
}

snmf_backend <- function() {
  new_ancestry_backend(
    "snmf",
    availability = function() ancestry_backend_availability(requireNamespace("LEA", quietly = TRUE),
      if (requireNamespace("LEA", quietly = TRUE)) "LEA package available" else "R package 'LEA' not installed"),
    validate = function(task) {
      if (is.null(task$geno_file)) stop("sNMF backend requires input$geno_file", call. = FALSE)
    },
    execute = function(task) run_snmf(task$geno_file, task$k, repetitions = 1L,
      entropy = task$entropy %||% TRUE, seed = task$seed,
      project_mode = task$project_mode %||% "new"),
    parse = function(native, task) {
      metric <- native$diagnostics$cross_entropy[[1L]]
      new_ancestry_replicate(task$sample_ids, native$q[[as.character(task$k)]],
        "snmf", k = task$k, replicate = task$replicate,
        metrics = c(cross_entropy = metric), seed = task$seed,
        provenance = list(input = task$geno_file))
    },
    description = "LEA sNMF sparse nonnegative matrix factorization")
}

#' Default ancestry backend registry
#' @return Registry containing ADMIXTURE, fastStructure, and sNMF adapters.
#' @export
default_ancestry_backend_registry <- function() {
  new_ancestry_backend_registry(list(admixture_backend(), faststructure_backend(), snmf_backend()))
}

#' @export
print.PopgenVCFAncestryBackend <- function(x, ...) {
  cat("<PopgenVCFAncestryBackend>", x$name, "contract", x$version, "\n")
  invisible(x)
}