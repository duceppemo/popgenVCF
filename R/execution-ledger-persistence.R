execution_ledger_required_columns <- function() {
  c("module", "status")
}

#' Create a canonical persisted execution ledger
#'
#' This constructor is intentionally distinct from the scheduler's internal
#' `new_execution_ledger(plan, registry, batches)` helper.
#'
#' @param ledger A data frame containing execution records.
#' @return A validated `PopgenVCFExecutionLedger` data table.
#' @export
new_persisted_execution_ledger <- function(ledger) {
  if (!is.data.frame(ledger)) {
    stop("ledger must be a data frame", call. = FALSE)
  }
  ledger <- data.table::as.data.table(data.table::copy(ledger))
  data.table::setattr(
    ledger,
    "class",
    unique(c("PopgenVCFExecutionLedger", class(ledger)))
  )
  validate_execution_ledger(ledger)
  ledger
}

#' Validate an execution ledger
#'
#' @param ledger An execution ledger.
#' @return `ledger`, invisibly.
#' @export
validate_execution_ledger <- function(ledger) {
  if (!inherits(ledger, "PopgenVCFExecutionLedger") ||
      !data.table::is.data.table(ledger)) {
    stop("ledger must be a PopgenVCFExecutionLedger data table", call. = FALSE)
  }
  missing <- setdiff(execution_ledger_required_columns(), names(ledger))
  if (length(missing)) {
    stop(
      "execution ledger is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(ledger$module) || any(!nzchar(as.character(ledger$module)))) {
    stop("execution ledger module identities must be non-empty", call. = FALSE)
  }
  if (anyDuplicated(as.character(ledger$module))) {
    stop("execution ledger module identities must be unique", call. = FALSE)
  }
  allowed_status <- c("pending", "running", "success", "failed", "blocked",
                      "cancelled", "skipped")
  if (anyNA(ledger$status) ||
      !all(as.character(ledger$status) %in% allowed_status)) {
    stop("execution ledger contains an unsupported status", call. = FALSE)
  }
  if ("attempt" %in% names(ledger)) {
    attempt <- suppressWarnings(as.integer(ledger$attempt))
    if (anyNA(attempt) || any(attempt < 1L) ||
        !identical(attempt, as.integer(ledger$attempt))) {
      stop("execution ledger attempts must be positive integers", call. = FALSE)
    }
  }
  invisible(ledger)
}

execution_ledger_sidecar_digest <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

read_execution_ledger_sidecar <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) != 1L) {
    stop("execution ledger SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields <- strsplit(lines, "[[:space:]]+")[[1]]
  if (length(fields) < 1L || !grepl("^[0-9a-f]{64}$", fields[[1]])) {
    stop("execution ledger SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields[[1]]
}

#' Write an execution ledger
#'
#' @param ledger A validated execution ledger or compatible data frame.
#' @param path Destination `.rds` path.
#' @param overwrite Whether an existing ledger may be replaced.
#' @return The normalized ledger path, invisibly.
#' @export
write_execution_ledger <- function(ledger, path, overwrite = FALSE) {
  if (!inherits(ledger, "PopgenVCFExecutionLedger")) {
    ledger <- new_persisted_execution_ledger(ledger)
  }
  validate_execution_ledger(ledger)
  path <- normalizePath(path, mustWork = FALSE)
  checksum_path <- paste0(path, ".sha256")
  if (!overwrite && (file.exists(path) || file.exists(checksum_path))) {
    stop("execution ledger already exists", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  envelope <- new_runtime_integrity_envelope("execution_ledger", ledger)
  tmp <- tempfile("execution-ledger-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(envelope, tmp, version = 3, compress = "xz")
  checksum <- execution_ledger_sidecar_digest(tmp)
  if (!file.rename(tmp, path)) {
    stop("unable to install execution ledger", call. = FALSE)
  }
  writeLines(paste(checksum, basename(path)), checksum_path, useBytes = TRUE)
  invisible(path)
}

#' Read and verify an execution ledger
#'
#' @param path Execution-ledger `.rds` path.
#' @return A validated `PopgenVCFExecutionLedger` data table.
#' @export
read_execution_ledger <- function(path) {
  checksum_path <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(checksum_path)) {
    stop("execution ledger and SHA-256 sidecar are required", call. = FALSE)
  }
  expected <- read_execution_ledger_sidecar(checksum_path)
  observed <- execution_ledger_sidecar_digest(path)
  if (!identical(expected, observed)) {
    stop("execution ledger file checksum mismatch", call. = FALSE)
  }
  envelope <- tryCatch(
    readRDS(path),
    error = function(error) {
      stop("execution ledger is unreadable or truncated", call. = FALSE)
    }
  )
  if (!inherits(envelope, "PopgenVCFRuntimeEnvelope")) {
    stop(
      "legacy unwrapped execution ledger requires explicit migration",
      call. = FALSE
    )
  }
  if (!identical(envelope$kind, "execution_ledger")) {
    stop("runtime integrity envelope is not an execution ledger", call. = FALSE)
  }
  ledger <- runtime_integrity_payload(envelope)
  validate_execution_ledger(ledger)
  ledger
}
