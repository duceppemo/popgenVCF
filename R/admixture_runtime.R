normalize_admixture_bim_chromosomes <- function(plink_prefix) {
  bim_path <- paste0(plink_prefix, ".bim")
  if (!file.exists(bim_path)) {
    stop("ADMIXTURE chromosome normalization requires a .bim file: ", bim_path,
         call. = FALSE)
  }

  bim <- tryCatch(
    data.table::fread(
      bim_path,
      header = FALSE,
      fill = TRUE,
      showProgress = FALSE,
      colClasses = list(character = 1L)
    ),
    error = function(e) e
  )
  if (inherits(bim, "error") || ncol(bim) < 6L || !nrow(bim)) {
    stop("ADMIXTURE cannot read a valid six-column BIM file: ", bim_path,
         call. = FALSE)
  }

  chromosome <- trimws(as.character(bim[[1L]]))
  integer_code <- !is.na(chromosome) & grepl("^[0-9]+$", chromosome)
  changed <- !integer_code
  if (!any(changed)) {
    return(invisible(list(
      changed = FALSE,
      bim = bim_path,
      mapping_file = NULL,
      changed_rows = 0L
    )))
  }

  normalized <- chromosome
  normalized[changed] <- "0"
  mapping <- unique(data.table::data.table(
    original_chromosome = chromosome[changed],
    admixture_chromosome = normalized[changed]
  ))
  mapping_file <- paste0(plink_prefix, ".admixture_chromosome_map.tsv")
  data.table::fwrite(
    mapping,
    mapping_file,
    sep = "\t",
    quote = FALSE,
    na = "NA"
  )

  bim[[1L]] <- normalized
  temporary <- tempfile("admixture-bim-", tmpdir = dirname(bim_path))
  backup <- paste0(bim_path, ".pre_admixture")
  on.exit(unlink(c(temporary, backup), force = TRUE), add = TRUE)
  data.table::fwrite(
    bim,
    temporary,
    sep = "\t",
    quote = FALSE,
    col.names = FALSE,
    na = "0"
  )
  unlink(backup, force = TRUE)
  if (!file.rename(bim_path, backup)) {
    stop("Unable to stage the original BIM file for ADMIXTURE normalization",
         call. = FALSE)
  }
  if (!file.rename(temporary, bim_path)) {
    file.rename(backup, bim_path)
    stop("Unable to install the ADMIXTURE-compatible BIM file", call. = FALSE)
  }
  unlink(backup, force = TRUE)

  log_msg(
    "Normalized ", sum(changed), " BIM chromosome entr",
    if (sum(changed) == 1L) "y" else "ies",
    " across ", nrow(mapping), " non-integer chromosome label(s) to code 0; ",
    "mapping: ", mapping_file,
    level = "WARNING"
  )
  invisible(list(
    changed = TRUE,
    bim = bim_path,
    mapping_file = mapping_file,
    changed_rows = as.integer(sum(changed))
  ))
}

admixture_command_arguments <- function(bed, k, cv_folds, threads, seed) {
  k <- as.integer(k)[1L]
  cv_folds <- as.integer(cv_folds)[1L]
  threads <- as.integer(threads)[1L]
  seed <- as.integer(seed)[1L]
  if (is.na(k) || k < 1L) stop("ADMIXTURE K must be a positive integer", call. = FALSE)
  if (is.na(cv_folds) || cv_folds < 2L) {
    stop("ADMIXTURE cross-validation folds must be at least two", call. = FALSE)
  }
  if (is.na(threads) || threads < 1L) threads <- 1L
  if (is.na(seed)) stop("ADMIXTURE seed must be a finite integer", call. = FALSE)

  # Follow the ordering documented by the ADMIXTURE manual: cross-validation
  # option, seed option, dataset, K, then the multithreading option. The
  # ADMIXTURE binary reads --seed=X directly; it does not honor an
  # ADMIXTURE_SEED environment variable (confirmed empirically: with only the
  # env var set, repeated runs and runs with different seed values produced
  # byte-identical Q matrices and log-likelihoods).
  c(
    sprintf("--cv=%d", cv_folds),
    sprintf("--seed=%d", seed),
    normalizePath(bed, mustWork = TRUE),
    as.character(k),
    sprintf("-j%d", threads)
  )
}

admixture_output_tail <- function(output, n = 12L) {
  text <- trimws(as.character(output))
  text <- text[nzchar(text)]
  if (!length(text)) return("no diagnostic output was produced")
  paste(utils::tail(text, as.integer(n)), collapse = " | ")
}

# Besides handling the strict ADMIXTURE chromosome parser, this treats every
# external-command failure as a first-class error and never attempts to sort
# an empty CV result table.
#' Run ADMIXTURE cross-validation across K values
#'
#' @param executable ADMIXTURE executable name or path.
#' @param plink_prefix Prefix of the PLINK BED dataset.
#' @param k_values Integer ancestry-cluster values to evaluate.
#' @param threads Number of ADMIXTURE worker threads.
#' @param cv_folds Number of cross-validation folds.
#' @param output_dir Directory for ADMIXTURE logs and outputs.
#' @param seed Deterministic ADMIXTURE seed.
#' @param timeout_seconds Maximum wall-clock seconds allowed per K value
#'   before ADMIXTURE (and its full process tree) is killed and treated as a
#'   failure, guarding against a hang on a degenerate or malformed genotype
#'   matrix -- a documented real ADMIXTURE failure mode -- rather than
#'   blocking the pipeline forever with no way to recover.
#' @return A data table of K values and cross-validation errors.
#' @export
run_admixture_cv <- function(executable, plink_prefix, k_values, threads = 1L,
                             cv_folds = 5L, output_dir = ".", seed = 42L,
                             timeout_seconds = 14400) {
  plink_prefix <- path.expand(as.character(plink_prefix)[1L])
  paths <- plink_bundle_paths(plink_prefix)
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "ADMIXTURE requires a complete PLINK bundle; missing ",
      paste0(".", missing, collapse = ", "),
      call. = FALSE
    )
  }

  executable <- as.character(executable)[1L]
  exe <- unname(Sys.which(executable))
  if (is.na(exe) || !nzchar(exe)) {
    stop("ADMIXTURE executable not found: ", executable, call. = FALSE)
  }

  k_values <- unique(as.integer(k_values))
  if (!length(k_values) || anyNA(k_values) || any(k_values < 1L)) {
    stop("ADMIXTURE K values must be positive integers", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  # ADMIXTURE's requirement of integer BIM chromosome codes is specific to
  # ADMIXTURE; plink_prefix may be the same canonical PLINK bundle another
  # ancestry backend (e.g. fastStructure) reuses by default -- see
  # prepare_structure_plink_input()'s caching, which both
  # run_module_admixture() and run_module_faststructure() share when neither
  # configures its own plink_prefix. Normalizing that shared bundle's .bim
  # file in place would leak an ADMIXTURE-only requirement into another
  # backend's input and make its result depend on execution order (whichever
  # backend happened to run first). Working from a private copy scoped to
  # output_dir instead keeps the shared bundle untouched regardless of order.
  private_prefix <- file.path(output_dir, basename(plink_prefix))
  private_paths <- plink_bundle_paths(private_prefix)
  for (ext in names(paths)) {
    source_real <- normalizePath(paths[[ext]], mustWork = TRUE)
    dest_real <- suppressWarnings(normalizePath(private_paths[[ext]], mustWork = FALSE))
    if (!identical(source_real, dest_real)) {
      if (!file.copy(paths[[ext]], private_paths[[ext]], overwrite = TRUE)) {
        stop("Unable to stage a private PLINK bundle copy for ADMIXTURE: ",
             private_paths[[ext]], call. = FALSE)
      }
    }
  }
  normalize_admixture_bim_chromosomes(private_prefix)
  bed <- normalizePath(private_paths[["bed"]], mustWork = TRUE)

  results <- vector("list", length(k_values))
  names(results) <- as.character(k_values)
  for (i in seq_along(k_values)) {
    k <- k_values[[i]]
    log_file <- file.path(output_dir, sprintf("admixture_K%d.log", k))
    output_stub <- file.path(
      output_dir,
      sprintf("%s.%d", basename(plink_prefix), k)
    )
    unlink(paste0(output_stub, c(".Q", ".P")), force = TRUE)

    args <- admixture_command_arguments(bed, k, cv_folds, threads, seed)
    run <- run_supervised_line_command(exe, args, output_dir, timeout_seconds)
    out <- run$output
    writeLines(as.character(out), log_file, useBytes = TRUE)

    status <- run$status
    if (is.na(status) || status != 0L) {
      stop(
        sprintf(
          "ADMIXTURE failed for K=%d with exit status %s; see %s; backend output: %s",
          k,
          if (is.na(status)) "unknown" else as.character(status),
          log_file,
          admixture_output_tail(out)
        ),
        call. = FALSE
      )
    }

    parsed <- parse_admixture_cv(paste(out, collapse = "\n"))
    if (is.null(parsed) || !nrow(parsed)) {
      stop(
        sprintf(
          "ADMIXTURE completed for K=%d but reported no cross-validation error; see %s; backend output: %s",
          k, log_file, admixture_output_tail(out)
        ),
        call. = FALSE
      )
    }
    results[[i]] <- parsed
  }

  cv <- data.table::rbindlist(results, fill = TRUE)
  if (!nrow(cv) || !all(c("K", "cv_error") %in% names(cv))) {
    stop("ADMIXTURE produced no valid cross-validation results", call. = FALSE)
  }
  data.table::setorder(cv, K)
  cv
}
