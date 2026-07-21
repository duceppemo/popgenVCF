# Release-state and public-API reconciliation helpers.
#
# These helpers are intentionally internal. They audit repository metadata and
# generated package interfaces without creating a second source of truth for
# exports or documentation.

release_reconciliation_root <- function(root = ".") {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  required <- c("DESCRIPTION", "NAMESPACE", "NEWS.md", "README.md", "docs/ROADMAP.md")
  missing <- required[!file.exists(file.path(root, required))]
  if (length(missing) > 0L) {
    stop("Release reconciliation requires repository files: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  root
}

release_reconciliation_read <- function(root, path) {
  readLines(file.path(root, path), warn = FALSE, encoding = "UTF-8")
}

release_reconciliation_export_declarations <- function(namespace_lines) {
  export_lines <- grep("^export\\(", namespace_lines, value = TRUE)
  exports <- sub("^export\\((.*)\\)$", "\\1", export_lines)
  exports[nzchar(exports)]
}

release_reconciliation_exports <- function(namespace_lines) {
  sort(unique(release_reconciliation_export_declarations(namespace_lines)))
}

release_reconciliation_s3_methods <- function(namespace_lines) {
  method_lines <- grep("^S3method\\(", namespace_lines, value = TRUE)
  if (length(method_lines) == 0L) {
    return(data.frame(generic = character(), class = character(), stringsAsFactors = FALSE))
  }
  values <- sub("^S3method\\((.*)\\)$", "\\1", method_lines)
  parts <- strsplit(values, ",", fixed = TRUE)
  out <- data.frame(
    generic = trimws(vapply(parts, `[[`, character(1), 1L)),
    class = trimws(vapply(parts, `[[`, character(1), 2L)),
    stringsAsFactors = FALSE
  )
  out[order(out$generic, out$class), , drop = FALSE]
}

release_reconciliation_rd_files <- function(root) {
  sort(list.files(file.path(root, "man"), pattern = "\\.Rd$", full.names = TRUE))
}

release_reconciliation_rd_aliases <- function(root) {
  files <- release_reconciliation_rd_files(root)
  if (length(files) == 0L) {
    return(data.frame(alias = character(), topic = character(), stringsAsFactors = FALSE))
  }
  records <- lapply(files, function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    alias_lines <- grep("^\\\\alias\\{", lines, value = TRUE)
    aliases <- sub("^\\\\alias\\{(.*)\\}$", "\\1", alias_lines)
    data.frame(alias = aliases, topic = basename(path), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, records)
  out <- out[nzchar(out$alias), , drop = FALSE]
  out[order(out$alias, out$topic), , drop = FALSE]
}

release_reconciliation_rd_documented_classes <- function(root, classes) {
  classes <- sort(unique(as.character(classes)))
  if (length(classes) == 0L) return(character())
  files <- release_reconciliation_rd_files(root)
  if (length(files) == 0L) return(character())
  rd_text <- paste(vapply(files, function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1)), collapse = "\n")
  classes[vapply(classes, function(class) {
    grepl(class, rd_text, fixed = TRUE)
  }, logical(1))]
}

release_reconciliation_version <- function(root) {
  description <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")
  as.character(description[1L, "Version"])
}

release_reconciliation_version_signals <- function(root, version) {
  files <- c("DESCRIPTION", "NEWS.md", "README.md", "docs/ROADMAP.md", "inst/doc/ROADMAP.md")
  files <- files[file.exists(file.path(root, files))]
  patterns <- c(
    DESCRIPTION = paste0("Version: ", version),
    NEWS.md = paste0("# popgenVCF ", version, " development"),
    README.md = paste0("Development series: **", version, "**"),
    `docs/ROADMAP.md` = paste0("**", version, "**"),
    `inst/doc/ROADMAP.md` = paste0("**", version, "**")
  )
  data.frame(
    file = files,
    expected = unname(patterns[files]),
    present = vapply(files, function(path) {
      any(grepl(patterns[[path]], release_reconciliation_read(root, path), fixed = TRUE))
    }, logical(1)),
    stringsAsFactors = FALSE
  )
}

release_reconciliation_finding <- function(severity, category, items, detail) {
  items <- as.character(items)
  if (length(items) == 0L) {
    return(data.frame(
      severity = character(),
      category = character(),
      item = character(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    severity = rep(severity, length(items)),
    category = rep(category, length(items)),
    item = items,
    detail = rep(detail, length(items)),
    stringsAsFactors = FALSE
  )
}

release_api_reconciliation <- function(root = ".") {
  root <- release_reconciliation_root(root)
  namespace_lines <- release_reconciliation_read(root, "NAMESPACE")
  export_declarations <- release_reconciliation_export_declarations(namespace_lines)
  exports <- sort(unique(export_declarations))
  s3_methods <- release_reconciliation_s3_methods(namespace_lines)
  aliases <- release_reconciliation_rd_aliases(root)
  documented_classes <- release_reconciliation_rd_documented_classes(root, s3_methods$class)
  version <- release_reconciliation_version(root)
  version_signals <- release_reconciliation_version_signals(root, version)

  missing_export_docs <- setdiff(exports, aliases$alias)
  duplicate_exports <- sort(unique(export_declarations[duplicated(export_declarations)]))
  duplicate_aliases <- sort(unique(aliases$alias[duplicated(aliases$alias)]))
  missing_s3_docs <- if (nrow(s3_methods) == 0L) character() else {
    method_names <- paste0(s3_methods$generic, ".", s3_methods$class)
    documented <- method_names %in% aliases$alias |
      s3_methods$generic %in% aliases$alias |
      s3_methods$class %in% documented_classes
    method_names[!documented]
  }

  findings <- do.call(rbind, list(
    release_reconciliation_finding(
      "blocking", "export-documentation", missing_export_docs,
      "Exported symbol has no matching Rd alias."
    ),
    release_reconciliation_finding(
      "blocking", "namespace", duplicate_exports,
      "Duplicate export declaration."
    ),
    release_reconciliation_finding(
      "blocking", "s3-documentation", missing_s3_docs,
      "Registered S3 method is not documented by method, generic, or class topic."
    ),
    release_reconciliation_finding(
      "blocking", "release-version", version_signals$file[!version_signals$present],
      paste0("Release-facing file does not identify development version ", version, ".")
    ),
    release_reconciliation_finding(
      "advisory", "documentation-alias", duplicate_aliases,
      "Rd alias is declared by more than one topic; verify that the overlap is intentional."
    )
  ))
  findings <- findings[order(findings$severity, findings$category, findings$item), , drop = FALSE]
  rownames(findings) <- NULL

  structure(
    list(
      version = version,
      exports = exports,
      s3_methods = s3_methods,
      aliases = aliases,
      version_signals = version_signals,
      findings = findings,
      passed = !any(findings$severity == "blocking")
    ),
    class = "PopgenVCFReleaseReconciliation"
  )
}

write_release_api_reconciliation <- function(root = ".", output_dir = file.path(root, "artifacts", "release-reconciliation")) {
  audit <- release_api_reconciliation(root)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  summary <- data.frame(
    package_version = audit$version,
    exports = length(audit$exports),
    s3_methods = nrow(audit$s3_methods),
    rd_aliases = nrow(audit$aliases),
    blocking_findings = sum(audit$findings$severity == "blocking"),
    advisory_findings = sum(audit$findings$severity == "advisory"),
    passed = audit$passed,
    stringsAsFactors = FALSE
  )
  utils::write.table(summary, file.path(output_dir, "summary.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  utils::write.table(audit$findings, file.path(output_dir, "findings.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  utils::write.table(
    data.frame(symbol = audit$exports, documented = audit$exports %in% audit$aliases$alias, stringsAsFactors = FALSE),
    file.path(output_dir, "exports.tsv"), sep = "\t", quote = FALSE, row.names = FALSE
  )
  utils::write.table(audit$s3_methods, file.path(output_dir, "s3-methods.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  utils::write.table(audit$version_signals, file.path(output_dir, "version-signals.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)

  if (!audit$passed) {
    stop("Release/API reconciliation failed with ", sum(audit$findings$severity == "blocking"), " blocking finding(s).", call. = FALSE)
  }
  invisible(audit)
}
