faststructure_install_directory <- function() {
  configured <- Sys.getenv("POPGENVCF_FASTSTRUCTURE_HOME", unset = "")
  if (nzchar(configured)) path.expand(configured) else path.expand("~/.local/opt/fastStructure3")
}

faststructure_environment_name <- function() {
  configured <- Sys.getenv("POPGENVCF_FASTSTRUCTURE_ENV", unset = "")
  if (nzchar(configured)) configured else "popgenvcf-faststructure"
}

resolve_faststructure_script <- function(requested, script_name,
                                         install_dir = faststructure_install_directory(),
                                         locator = Sys.which,
                                         file_exists = file.exists) {
  requested <- as.character(requested %||% script_name)[1L]
  if (is.na(requested) || !nzchar(requested)) requested <- script_name

  located <- unname(as.character(locator(requested))[1L])
  candidates <- list()
  if (!is.na(located) && nzchar(located)) {
    candidates[[length(candidates) + 1L]] <- list(path = located, source = "PATH")
  }
  if (grepl("[/\\\\]", requested)) {
    candidates[[length(candidates) + 1L]] <- list(
      path = path.expand(requested), source = "configured path"
    )
  }
  candidates[[length(candidates) + 1L]] <- list(
    path = file.path(path.expand(install_dir), basename(requested)),
    source = "fastStructure install directory"
  )
  if (!identical(basename(requested), script_name)) {
    candidates[[length(candidates) + 1L]] <- list(
      path = file.path(path.expand(install_dir), script_name),
      source = "fastStructure install directory"
    )
  }

  seen <- character()
  for (candidate in candidates) {
    path <- as.character(candidate$path)[1L]
    if (is.na(path) || !nzchar(path) || path %in% seen) next
    seen <- c(seen, path)
    if (isTRUE(file_exists(path))) {
      return(list(
        path = normalizePath(path, mustWork = TRUE),
        source = candidate$source,
        on_path = identical(candidate$source, "PATH")
      ))
    }
  }

  stop(
    paste0(
      "fastStructure script not found: ", requested, ". Expected it on PATH or under ",
      path.expand(install_dir), ". Create the isolated runtime with `mamba env create --file ",
      "inst/conda/faststructure-environment.yml`, activate it, and run ",
      "`bash inst/scripts/install-faststructure.sh ", path.expand(install_dir), "`."
    ),
    call. = FALSE
  )
}

resolve_faststructure_launcher <- function(script,
                                           env_name = faststructure_environment_name(),
                                           locator = Sys.which,
                                           active_env = Sys.getenv("CONDA_DEFAULT_ENV", unset = ""),
                                           conda_executable = Sys.getenv("CONDA_EXE", unset = ""),
                                           python_executable = Sys.getenv(
                                             "POPGENVCF_FASTSTRUCTURE_PYTHON", unset = ""
                                           )) {
  script_path <- normalizePath(script$path, mustWork = TRUE)
  extension <- tolower(tools::file_ext(script_path))

  # A command already installed on PATH (for example a Bioconda entry point or
  # a user wrapper) is self-contained and should be invoked directly.
  if (isTRUE(script$on_path)) {
    return(list(command = script_path, prefix_args = character(), mode = "direct"))
  }

  # Non-Python configured executables are assumed to be intentional wrappers.
  if (!identical(extension, "py") && file.access(script_path, mode = 1L) == 0L) {
    return(list(command = script_path, prefix_args = character(), mode = "direct"))
  }

  python_executable <- path.expand(as.character(python_executable)[1L])
  if (!is.na(python_executable) && nzchar(python_executable) && file.exists(python_executable)) {
    return(list(
      command = normalizePath(python_executable, mustWork = TRUE),
      prefix_args = script_path,
      mode = "configured Python"
    ))
  }

  if (identical(active_env, env_name)) {
    python <- unname(as.character(locator("python"))[1L])
    if (!is.na(python) && nzchar(python)) {
      return(list(
        command = python,
        prefix_args = script_path,
        mode = paste0("active Conda environment ", env_name)
      ))
    }
  }

  conda_executable <- as.character(conda_executable)[1L]
  if (is.na(conda_executable) || !nzchar(conda_executable)) {
    conda_executable <- unname(as.character(locator("conda"))[1L])
  }
  if (!is.na(conda_executable) && nzchar(conda_executable)) {
    return(list(
      command = conda_executable,
      prefix_args = c("run", "-n", env_name, "python", script_path),
      mode = paste0("Conda environment ", env_name)
    ))
  }

  stop(
    paste0(
      "fastStructure requires its isolated Python runtime. Conda was not found and ",
      "POPGENVCF_FASTSTRUCTURE_PYTHON is unset. Set that variable to the environment's ",
      "Python executable or make conda available on PATH."
    ),
    call. = FALSE
  )
}

faststructure_output_tail <- function(output, n = 12L) {
  text <- trimws(as.character(output))
  text <- text[nzchar(text)]
  if (!length(text)) return("no diagnostic output was produced")
  paste(utils::tail(text, as.integer(n)), collapse = " | ")
}

run_faststructure_process <- function(launcher, arguments, log_file) {
  output <- tryCatch(
    suppressWarnings(system2(
      launcher$command,
      c(launcher$prefix_args, arguments),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(e) structure(conditionMessage(e), status = NA_integer_)
  )
  writeLines(as.character(output), log_file, useBytes = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  status <- suppressWarnings(as.integer(status)[1L])
  list(output = output, status = status)
}

# Late-loaded replacement supporting the isolated Python 3 runtime documented
# by popgenVCF while retaining direct PATH-based installations and wrappers.
run_faststructure <- function(structure_executable = "structure.py",
                              choosek_executable = "chooseK.py",
                              plink_prefix, k_values, output_dir = ".",
                              seed = 42L) {
  paths <- plink_bundle_paths(plink_prefix)
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "fastStructure requires a complete PLINK bundle; missing ",
      paste0(".", missing, collapse = ", "),
      call. = FALSE
    )
  }

  structure_script <- resolve_faststructure_script(
    structure_executable, "structure.py"
  )
  choosek_script <- resolve_faststructure_script(
    choosek_executable, "chooseK.py"
  )
  structure_launcher <- resolve_faststructure_launcher(structure_script)
  choosek_launcher <- resolve_faststructure_launcher(choosek_script)

  k_values <- unique(as.integer(k_values))
  if (!length(k_values) || anyNA(k_values) || any(k_values < 1L)) {
    stop("fastStructure K values must be positive integers", call. = FALSE)
  }
  seed <- as.integer(seed)[1L]
  if (is.na(seed)) stop("fastStructure seed must be an integer", call. = FALSE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  plink_prefix <- normalizePath(plink_prefix, mustWork = FALSE)
  prefix <- file.path(output_dir, "faststructure")

  log_msg(
    "Using fastStructure via ", structure_launcher$mode,
    "; script: ", structure_script$path,
    level = "INFO"
  )

  runs <- vector("list", length(k_values))
  q <- vector("list", length(k_values))
  names(q) <- as.character(k_values)
  for (i in seq_along(k_values)) {
    k <- k_values[[i]]
    log_file <- file.path(output_dir, sprintf("fastStructure_K%d.log", k))
    qfile <- sprintf("%s.%d.meanQ", prefix, k)
    unlink(qfile, force = TRUE)
    arguments <- c(
      "-K", as.character(k),
      "--input", plink_prefix,
      "--output", prefix,
      "--seed", as.character(seed + k),
      "--format", "bed"
    )
    process <- run_faststructure_process(
      structure_launcher, arguments, log_file
    )
    if (is.na(process$status) || process$status != 0L) {
      stop(
        sprintf(
          paste0(
            "fastStructure failed for K=%d with exit status %s; see %s; ",
            "backend output: %s"
          ),
          k,
          if (is.na(process$status)) "unknown" else as.character(process$status),
          log_file,
          faststructure_output_tail(process$output)
        ),
        call. = FALSE
      )
    }
    if (!file.exists(qfile)) {
      stop(
        sprintf(
          "fastStructure completed for K=%d but did not create %s; see %s; backend output: %s",
          k, qfile, log_file, faststructure_output_tail(process$output)
        ),
        call. = FALSE
      )
    }
    q[[i]] <- normalize_q_matrix(data.table::fread(qfile, header = FALSE))
    runs[[i]] <- data.table::data.table(
      K = k,
      exit_status = process$status,
      runtime = structure_launcher$mode,
      log_file = log_file,
      q_file = qfile
    )
  }

  choose_log <- file.path(output_dir, "fastStructure_chooseK.log")
  choose_process <- run_faststructure_process(
    choosek_launcher,
    c("--input", prefix),
    choose_log
  )
  if (is.na(choose_process$status) || choose_process$status != 0L) {
    stop(
      sprintf(
        "fastStructure chooseK failed with exit status %s; see %s; backend output: %s",
        if (is.na(choose_process$status)) "unknown" else as.character(choose_process$status),
        choose_log,
        faststructure_output_tail(choose_process$output)
      ),
      call. = FALSE
    )
  }

  list(
    runs = data.table::rbindlist(runs, fill = TRUE),
    q = q,
    choose_k_text = as.character(choose_process$output),
    suggested_k = parse_faststructure_k(
      paste(choose_process$output, collapse = "\n")
    ),
    runtime = list(
      structure_script = structure_script$path,
      choosek_script = choosek_script$path,
      mode = structure_launcher$mode,
      environment = faststructure_environment_name()
    )
  )
}
