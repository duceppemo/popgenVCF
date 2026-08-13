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

# fastStructure writes its per-run diagnostics (including the converged
# marginal likelihood) to its own <output-prefix>.<K>.log file, not to the
# structure.py process's stdout/stderr -- run_faststructure_process() only
# captures the latter, which is typically empty. Parse the log file directly.
parse_faststructure_marginal_likelihood <- function(log_lines) {
  text <- trimws(as.character(log_lines))
  matches <- regmatches(text, regexpr("(?<=^Marginal Likelihood = )[-0-9.eE]+", text, perl = TRUE))
  matches <- matches[nzchar(matches)]
  if (!length(matches)) return(NA_real_)
  suppressWarnings(as.numeric(matches[[length(matches)]]))
}

run_faststructure_process <- function(executable, arguments, log_file,
                                      working_directory = dirname(log_file),
                                      timeout_seconds = 14400) {
  run <- run_supervised_line_command(executable, arguments, working_directory, timeout_seconds)
  writeLines(as.character(run$output), log_file, useBytes = TRUE)
  list(output = run$output, status = run$status)
}

# The supported Bioconda package installs structure.py and chooseK.py directly
# into the same Conda environment as popgenVCF.
#' Run external fastStructure across K values
#'
#' @param structure_executable Path or command for structure.py.
#' @param choosek_executable Path or command for chooseK.py.
#' @param plink_prefix PLINK BED/BIM/FAM prefix.
#' @param k_values Integer K values.
#' @param output_dir Output directory.
#' @param seed Random seed.
#' @param timeout_seconds Maximum wall-clock seconds allowed per subprocess
#'   call (each K value's `structure.py` run, and the final `chooseK.py`
#'   run) before it (and its full process tree) is killed and treated as a
#'   failure, guarding against a hang rather than blocking the pipeline
#'   forever with no way to recover.
#' @return A list containing run records, Q matrices, and chooseK output.
#' @export
run_faststructure <- function(structure_executable = "structure.py",
                              choosek_executable = "chooseK.py",
                              plink_prefix, k_values, output_dir = ".",
                              seed = 42L, timeout_seconds = 14400) {
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
      structure_command, arguments, log_file,
      working_directory = output_dir, timeout_seconds = timeout_seconds
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
    fastructure_native_log <- sprintf("%s.%d.log", prefix, k)
    marginal_likelihood <- if (file.exists(fastructure_native_log)) {
      parse_faststructure_marginal_likelihood(readLines(fastructure_native_log, warn = FALSE))
    } else NA_real_
    runs[[i]] <- data.table::data.table(
      K = k,
      exit_status = process$status,
      executable = structure_command,
      log_file = log_file,
      q_file = qfile,
      marginal_likelihood = marginal_likelihood
    )
  }

  choose_log <- file.path(output_dir, "fastStructure_chooseK.log")
  choose_process <- run_faststructure_process(
    choosek_command,
    c("--input", prefix),
    choose_log,
    working_directory = output_dir, timeout_seconds = timeout_seconds
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
    choose_k_votes = parse_faststructure_k_votes(
      paste(choose_process$output, collapse = "\n"), as.integer(k_values)
    ),
    runtime = list(
      structure_executable = structure_command,
      choosek_executable = choosek_command
    )
  )
}
