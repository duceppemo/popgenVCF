resolve_faststructure_executable <- function(requested, label,
                                             locator = Sys.which,
                                             file_exists = file.exists) {
  requested <- as.character(requested %||% label)[1L]
  if (is.na(requested) || !nzchar(requested)) requested <- label

  located <- unname(as.character(locator(requested))[1L])
  if (!is.na(located) && nzchar(located)) {
    return(normalizePath(located, mustWork = TRUE))
  }

  if (grepl("[/\\\\]", requested)) {
    candidate <- path.expand(requested)
    if (isTRUE(file_exists(candidate))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }

  stop(
    paste0(
      "fastStructure executable not found: ", requested,
      ". Install it in the active popgenVCF environment with ",
      "`mamba install bioconda::faststructure`, or configure an absolute path."
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

run_faststructure_process <- function(executable, arguments, log_file) {
  output <- tryCatch(
    suppressWarnings(system2(
      executable,
      arguments,
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

# Late-loaded replacement with strict process diagnostics and output validation.
# The supported Bioconda package installs structure.py and chooseK.py directly
# into the same Conda environment as popgenVCF.
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

  structure_command <- resolve_faststructure_executable(
    structure_executable, "structure.py"
  )
  choosek_command <- resolve_faststructure_executable(
    choosek_executable, "chooseK.py"
  )

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
    "Using fastStructure executable: ", structure_command,
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
      structure_command, arguments, log_file
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
      executable = structure_command,
      log_file = log_file,
      q_file = qfile
    )
  }

  choose_log <- file.path(output_dir, "fastStructure_chooseK.log")
  choose_process <- run_faststructure_process(
    choosek_command,
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
      structure_executable = structure_command,
      choosek_executable = choosek_command
    )
  )
}
