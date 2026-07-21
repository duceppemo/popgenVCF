# Phase 0.9.1.2 - publication report renderer adapters and execution

# Fingerprints describe record content, not the in-memory S3 class used to
# dispatch methods. This definition intentionally supersedes the initial
# contract helper after all package files are sourced.
.publication_report_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

#' Create a publication report renderer adapter
#'
#' @param id Stable renderer identifier.
#' @param version Stable renderer version.
#' @param formats Supported output formats.
#' @param render Function accepting `source_path`, `output_path`, `format`, and
#'   `parameters`, and returning a list with `status`, `stdout`, `stderr`, and
#'   optional `warnings`.
#' @return A validated renderer adapter.
#' @export
new_publication_report_renderer <- function(id, version, formats, render) {
  .publication_report_scalar(id, "id")
  .publication_report_scalar(version, "version")
  formats <- sort(unique(tolower(as.character(formats))))
  if (!length(formats) || any(!formats %in% .publication_report_formats)) {
    stop("renderer formats must contain only html, pdf, or docx.", call. = FALSE)
  }
  if (!is.function(render)) stop("render must be a function.", call. = FALSE)
  structure(list(id = id, version = version, formats = formats, render = render),
            class = c("PopgenVCFPublicationReportRenderer", "list"))
}

#' Validate a publication report renderer adapter
#' @param renderer A renderer adapter.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_report_renderer <- function(renderer) {
  if (!inherits(renderer, "PopgenVCFPublicationReportRenderer")) {
    stop("renderer must be a publication report renderer.", call. = FALSE)
  }
  .publication_report_scalar(renderer$id, "renderer id")
  .publication_report_scalar(renderer$version, "renderer version")
  if (!is.function(renderer$render) || !length(renderer$formats) ||
      any(!renderer$formats %in% .publication_report_formats) ||
      !identical(renderer$formats, sort(unique(renderer$formats)))) {
    stop("Malformed publication report renderer.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create the Quarto publication report renderer
#'
#' @param executable Path to the Quarto executable.
#' @param version Optional stable version identity.
#' @return A renderer adapter supporting HTML, PDF, and DOCX.
#' @export
quarto_publication_report_renderer <- function(
    executable = Sys.which("quarto"), version = NULL) {
  .publication_report_scalar(executable, "executable")
  if (!nzchar(executable) || !file.exists(executable)) {
    stop("Quarto executable is unavailable.", call. = FALSE)
  }
  if (is.null(version)) {
    out <- system2(executable, "--version", stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status") %||% 0L
    if (status != 0L || !length(out)) stop("Unable to identify Quarto version.", call. = FALSE)
    version <- trimws(out[[1L]])
  }
  new_publication_report_renderer(
    "quarto", version, .publication_report_formats,
    function(source_path, output_path, format, parameters) {
      output_dir <- dirname(output_path)
      args <- c("render", source_path, "--to", format,
                "--output", basename(output_path),
                "--output-dir", normalizePath(output_dir, mustWork = TRUE))
      out <- system2(executable, args, stdout = TRUE, stderr = TRUE)
      list(
        status = as.integer(attr(out, "status") %||% 0L),
        stdout = as.character(out), stderr = character(), warnings = character()
      )
    }
  )
}

.publication_report_execution_row <- function(format, path, status, stdout, stderr, warnings) {
  data.frame(
    format = format,
    path = path,
    status = as.integer(status),
    stdout = paste(as.character(stdout), collapse = "\n"),
    stderr = paste(as.character(stderr), collapse = "\n"),
    warnings = paste(sort(unique(as.character(warnings))), collapse = "\n"),
    stringsAsFactors = FALSE
  )
}

#' Execute a publication report rendering plan
#'
#' @param plan A validated publication report plan.
#' @param manuscript The originating manuscript.
#' @param spec The originating report specification.
#' @param renderer A renderer adapter matching the plan identity.
#' @param output_dir Destination directory.
#' @param parameters Stable named rendering parameters.
#' @return A deterministic execution record. Renderer failures are represented
#'   in the record and do not produce a successful output manifest.
#' @export
execute_publication_report_plan <- function(
    plan, manuscript, spec, renderer, output_dir, parameters = list()) {
  validate_publication_report_plan(plan, manuscript, spec)
  validate_publication_report_renderer(renderer)
  .publication_report_scalar(output_dir, "output_dir")
  if (!identical(plan$renderer, list(id = renderer$id, version = renderer$version))) {
    stop("Renderer identity does not match the publication report plan.", call. = FALSE)
  }
  unsupported <- setdiff(plan$outputs$format, renderer$formats)
  if (length(unsupported)) {
    stop(sprintf("Renderer does not support format(s): %s", paste(unsupported, collapse = ", ")), call. = FALSE)
  }
  if (!is.list(parameters) || (length(parameters) && (is.null(names(parameters)) || any(!nzchar(names(parameters)))))) {
    stop("parameters must be a named list.", call. = FALSE)
  }
  parameters <- parameters[sort(names(parameters))]
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  source_path <- file.path(output_dir, "manuscript.md")
  writeLines(render_manuscript_markdown(manuscript), source_path, useBytes = TRUE)
  source_sha256 <- digest::digest(source_path, algo = "sha256", file = TRUE)

  rows <- vector("list", nrow(plan$outputs))
  halted <- FALSE
  for (i in seq_len(nrow(plan$outputs))) {
    item <- plan$outputs[i, , drop = FALSE]
    output_path <- file.path(output_dir, item$path)
    if (halted) {
      rows[[i]] <- .publication_report_execution_row(
        item$format, item$path, 125L, character(),
        "Skipped because a previous format failed.", character()
      )
      next
    }
    result <- tryCatch(
      renderer$render(source_path, output_path, item$format, parameters),
      error = function(e) list(status = 1L, stdout = character(),
                               stderr = conditionMessage(e), warnings = character())
    )
    required <- c("status", "stdout", "stderr")
    if (!is.list(result) || any(!required %in% names(result)) ||
        length(result$status) != 1L || is.na(result$status)) {
      result <- list(status = 1L, stdout = character(),
                     stderr = "Renderer returned a malformed result.", warnings = character())
    }
    result$warnings <- result$warnings %||% character()
    rows[[i]] <- .publication_report_execution_row(
      item$format, item$path, result$status, result$stdout,
      result$stderr, result$warnings
    )
    halted <- result$status != 0L || !file.exists(output_path)
    if (result$status == 0L && !file.exists(output_path)) {
      rows[[i]]$status <- 1L
      rows[[i]]$stderr <- "Renderer reported success but did not create the expected output."
    }
  }
  attempts <- do.call(rbind, rows)
  succeeded <- all(attempts$status == 0L)
  warnings <- sort(unique(unlist(strsplit(attempts$warnings[nzchar(attempts$warnings)], "\n", fixed = TRUE))))
  execution <- list(
    record_type = "popgenvcf_publication_report_execution",
    schema_version = "1.0.0",
    plan_fingerprint = plan$fingerprint,
    renderer = list(id = renderer$id, version = renderer$version),
    parameters = parameters,
    source = list(path = "manuscript.md", sha256 = source_sha256),
    attempts = attempts,
    warnings = warnings,
    succeeded = succeeded,
    failure = if (succeeded) NULL else attempts$stderr[which(attempts$status != 0L)[[1L]]]
  )
  if (succeeded) {
    execution$output_manifest <- new_publication_report_output_manifest(plan, output_dir, warnings)
  } else {
    execution$output_manifest <- NULL
  }
  execution$fingerprint <- .publication_report_fingerprint(execution)
  class(execution) <- c("PopgenVCFPublicationReportExecution", "list")
  execution
}

#' Validate a publication report execution record
#' @param execution An execution record.
#' @param plan The originating plan.
#' @param output_dir Destination directory used for execution.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_report_execution <- function(execution, plan, output_dir) {
  if (!inherits(execution, "PopgenVCFPublicationReportExecution")) {
    stop("execution must be a publication report execution record.", call. = FALSE)
  }
  if (!inherits(plan, "PopgenVCFPublicationReportPlan") ||
      !identical(execution$plan_fingerprint, plan$fingerprint) ||
      !identical(execution$renderer, plan$renderer)) {
    stop("Publication report execution is not bound to the supplied plan.", call. = FALSE)
  }
  if (!identical(execution$fingerprint, .publication_report_fingerprint(execution))) {
    stop("Publication report execution fingerprint mismatch.", call. = FALSE)
  }
  source_path <- file.path(output_dir, execution$source$path)
  if (!file.exists(source_path) ||
      !identical(execution$source$sha256,
                 digest::digest(source_path, algo = "sha256", file = TRUE))) {
    stop("Publication report execution source checksum mismatch.", call. = FALSE)
  }
  if (isTRUE(execution$succeeded)) {
    if (is.null(execution$output_manifest)) {
      stop("Successful publication report execution lacks an output manifest.", call. = FALSE)
    }
    validate_publication_report_output_manifest(execution$output_manifest, plan, output_dir)
  } else if (!is.null(execution$output_manifest) || is.null(execution$failure) || !nzchar(execution$failure)) {
    stop("Failed publication report execution is malformed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Render a publication report execution summary
#' @param execution A publication report execution record.
#' @return Character vector containing Markdown report lines.
#' @export
publication_report_execution_report <- function(execution) {
  if (!inherits(execution, "PopgenVCFPublicationReportExecution") ||
      !identical(execution$fingerprint, .publication_report_fingerprint(execution))) {
    stop("Invalid publication report execution record.", call. = FALSE)
  }
  c(
    "# Publication report rendering execution",
    "",
    sprintf("- Succeeded: `%s`", tolower(as.character(execution$succeeded))),
    sprintf("- Renderer: `%s` (`%s`)", execution$renderer$id, execution$renderer$version),
    sprintf("- Formats attempted: `%d`", nrow(execution$attempts)),
    sprintf("- Warnings: `%d`", length(execution$warnings)),
    sprintf("- Execution fingerprint: `%s`", execution$fingerprint)
  )
}
