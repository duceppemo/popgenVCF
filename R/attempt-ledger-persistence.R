attempt_ledger_required_columns <- function() {
  c("module", "status", "attempt")
}

#' Create a canonical attempt ledger
#'
#' @param ledger A data frame containing one row per module attempt.
#' @return A validated `PopgenVCFAttemptLedger` data table.
#' @export
new_attempt_ledger <- function(ledger) {
  if (!is.data.frame(ledger)) {
    stop("ledger must be a data frame", call. = FALSE)
  }
  ledger <- data.table::as.data.table(data.table::copy(ledger))
  data.table::setattr(
    ledger,
    "class",
    unique(c("PopgenVCFAttemptLedger", class(ledger)))
  )
  validate_attempt_ledger(ledger)
  ledger
}

#' Validate an attempt ledger and its retry chains
#'
#' @param ledger An attempt ledger.
#' @return `ledger`, invisibly.
#' @export
validate_attempt_ledger <- function(ledger) {
  if (!inherits(ledger, "PopgenVCFAttemptLedger") ||
      !data.table::is.data.table(ledger)) {
    stop("ledger must be a PopgenVCFAttemptLedger data table", call. = FALSE)
  }
  missing <- setdiff(attempt_ledger_required_columns(), names(ledger))
  if (length(missing)) {
    stop(
      "attempt ledger is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  module <- as.character(ledger$module)
  if (anyNA(module) || any(!nzchar(module))) {
    stop("attempt ledger module identities must be non-empty", call. = FALSE)
  }
  status <- as.character(ledger$status)
  allowed_status <- c(
    "pending", "running", "success", "failed", "blocked",
    "cancelled", "skipped"
  )
  if (anyNA(status) || !all(status %in% allowed_status)) {
    stop("attempt ledger contains an unsupported status", call. = FALSE)
  }
  attempt <- suppressWarnings(as.integer(ledger$attempt))
  if (anyNA(attempt) || any(attempt < 1L) ||
      !identical(attempt, as.integer(ledger$attempt))) {
    stop("attempt ledger attempts must be positive integers", call. = FALSE)
  }
  keys <- paste(module, attempt, sep = "\r")
  if (anyDuplicated(keys)) {
    stop("attempt ledger module-attempt pairs must be unique", call. = FALSE)
  }

  chains <- split(attempt, module)
  for (name in names(chains)) {
    observed <- sort(unique(chains[[name]]))
    expected <- seq_len(max(observed))
    if (!identical(observed, expected)) {
      stop(
        "attempt ledger retry chain is not contiguous for module: ", name,
        call. = FALSE
      )
    }
    rows <- ledger[module == name][order(attempt)]
    terminal <- which(as.character(rows$status) %in% c("success", "cancelled", "skipped"))
    if (length(terminal) && terminal[[1]] < nrow(rows)) {
      stop(
        "attempt ledger contains attempts after a terminal state for module: ",
        name,
        call. = FALSE
      )
    }
  }

  global_attempts <- sort(unique(attempt))
  if (!identical(global_attempts, seq_len(max(global_attempts)))) {
    stop("attempt ledger global attempt sequence must be contiguous", call. = FALSE)
  }
  invisible(ledger)
}

attempt_ledger_sidecar_digest <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

read_attempt_ledger_sidecar <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) != 1L) {
    stop("attempt ledger SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields <- strsplit(lines, "[[:space:]]+")[[1]]
  if (length(fields) < 1L || !grepl("^[0-9a-f]{64}$", fields[[1]])) {
    stop("attempt ledger SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields[[1]]
}

#' Write an attempt ledger
#'
#' @param ledger A validated attempt ledger or compatible data frame.
#' @param path Destination `.rds` path.
#' @param overwrite Whether an existing ledger may be replaced.
#' @return The normalized ledger path, invisibly.
#' @export
write_attempt_ledger <- function(ledger, path, overwrite = FALSE) {
  if (!inherits(ledger, "PopgenVCFAttemptLedger")) {
    ledger <- new_attempt_ledger(ledger)
  }
  validate_attempt_ledger(ledger)
  path <- normalizePath(path, mustWork = FALSE)
  checksum_path <- paste0(path, ".sha256")
  if (!overwrite && (file.exists(path) || file.exists(checksum_path))) {
    stop("attempt ledger already exists", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  envelope <- new_runtime_integrity_envelope("attempt_ledger", ledger)
  tmp <- tempfile("attempt-ledger-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(envelope, tmp, version = 3, compress = "xz")
  checksum <- attempt_ledger_sidecar_digest(tmp)
  if (!file.rename(tmp, path)) {
    stop("unable to install attempt ledger", call. = FALSE)
  }
  writeLines(paste(checksum, basename(path)), checksum_path, useBytes = TRUE)
  invisible(path)
}

#' Read and verify an attempt ledger
#'
#' @param path Attempt-ledger `.rds` path.
#' @return A validated `PopgenVCFAttemptLedger` data table.
#' @export
read_attempt_ledger <- function(path) {
  checksum_path <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(checksum_path)) {
    stop("attempt ledger and SHA-256 sidecar are required", call. = FALSE)
  }
  expected <- read_attempt_ledger_sidecar(checksum_path)
  observed <- attempt_ledger_sidecar_digest(path)
  if (!identical(expected, observed)) {
    stop("attempt ledger file checksum mismatch", call. = FALSE)
  }
  envelope <- tryCatch(
    readRDS(path),
    error = function(error) {
      stop("attempt ledger is unreadable or truncated", call. = FALSE)
    }
  )
  if (!inherits(envelope, "PopgenVCFRuntimeEnvelope")) {
    stop(
      "legacy unwrapped attempt ledger requires explicit migration",
      call. = FALSE
    )
  }
  if (!identical(envelope$kind, "attempt_ledger")) {
    stop("runtime integrity envelope is not an attempt ledger", call. = FALSE)
  }
  ledger <- runtime_integrity_payload(envelope)
  validate_attempt_ledger(ledger)
  ledger
}
