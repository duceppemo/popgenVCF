# Deterministic public API contract snapshots and compatibility checks.

public_api_contract_signature <- function(fun) {
  f <- formals(fun)
  if (is.null(f)) return(NA_character_)
  paste(vapply(names(f), function(name) {
    formal <- f[name]
    missing_template <- alist(value = )
    names(missing_template) <- name
    required <- identical(formal, missing_template)
    default <- if (required) {
      "<required>"
    } else {
      paste(deparse(f[[name]], width.cutoff = 500L), collapse = " ")
    }
    paste(name, default, sep = "=")
  }, character(1)), collapse = ";")
}

public_api_contract_snapshot <- function(namespace = "popgenVCF", namespace_file = NULL) {
  ns <- asNamespace(namespace)
  exports <- sort(getNamespaceExports(ns))
  exported <- data.frame(
    kind = "export",
    generic = NA_character_,
    class = NA_character_,
    symbol = exports,
    signature = vapply(exports, function(symbol) {
      object <- getExportedValue(namespace, symbol)
      if (is.function(object)) public_api_contract_signature(object) else NA_character_
    }, character(1)),
    stringsAsFactors = FALSE
  )

  if (is.null(namespace_file)) {
    namespace_file <- system.file("NAMESPACE", package = namespace)
  }
  lines <- if (nzchar(namespace_file) && file.exists(namespace_file)) readLines(namespace_file, warn = FALSE) else character()
  s3_lines <- grep("^S3method\\(", lines, value = TRUE)
  s3 <- if (length(s3_lines) == 0L) {
    exported[0, , drop = FALSE]
  } else {
    values <- sub("^S3method\\((.*)\\)$", "\\1", s3_lines)
    parts <- strsplit(values, ",", fixed = TRUE)
    generic <- trimws(vapply(parts, `[[`, character(1), 1L))
    class <- trimws(vapply(parts, `[[`, character(1), 2L))
    data.frame(
      kind = "s3",
      generic = generic,
      class = class,
      symbol = paste0(generic, ".", class),
      signature = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(exported, s3)
  out[order(out$kind, out$symbol), , drop = FALSE]
}

public_api_contract_arguments <- function(signature) {
  if (is.na(signature) || !nzchar(signature)) return(data.frame(name = character(), default = character(), required = logical()))
  fields <- strsplit(signature, ";", fixed = TRUE)[[1L]]
  pos <- regexpr("=", fields, fixed = TRUE)
  data.frame(
    name = substring(fields, 1L, pos - 1L),
    default = substring(fields, pos + 1L),
    required = substring(fields, pos + 1L) == "<required>",
    stringsAsFactors = FALSE
  )
}

compare_public_api_contract <- function(baseline, current) {
  key <- function(x) paste(x$kind, x$symbol, sep = ":")
  baseline_key <- key(baseline)
  current_key <- key(current)
  findings <- list()
  add <- function(severity, category, item, detail) {
    findings[[length(findings) + 1L]] <<- data.frame(severity, category, item, detail, stringsAsFactors = FALSE)
  }

  for (item in setdiff(baseline_key, current_key)) {
    add("blocking", "removed-api", item, "Public API entry was removed from the current contract.")
  }
  for (item in setdiff(current_key, baseline_key)) {
    add("advisory", "added-api", item, "Public API entry was added; review and accept it before refreshing the baseline.")
  }

  shared <- intersect(baseline_key, current_key)
  for (item in shared) {
    old <- baseline[match(item, baseline_key), , drop = FALSE]
    new <- current[match(item, current_key), , drop = FALSE]
    if (old$kind != "export" || is.na(old$signature) || is.na(new$signature)) next
    old_args <- public_api_contract_arguments(old$signature)
    new_args <- public_api_contract_arguments(new$signature)
    removed <- setdiff(old_args$name, new_args$name)
    for (arg in removed) add("blocking", "removed-argument", paste0(old$symbol, "::", arg), "Public function argument was removed.")
    old_required <- old_args$name[old_args$required]
    new_required <- new_args$name[new_args$required]
    if (!identical(old_required, new_required[seq_len(min(length(old_required), length(new_required)))])) {
      add("blocking", "required-argument-order", old$symbol, "Required arguments were removed, reordered, or inserted before existing required arguments.")
    }
    shared_args <- intersect(old_args$name, new_args$name)
    for (arg in shared_args) {
      old_default <- old_args$default[match(arg, old_args$name)]
      new_default <- new_args$default[match(arg, new_args$name)]
      if (!identical(old_default, new_default)) add("blocking", "changed-default", paste0(old$symbol, "::", arg), "Public function argument default changed.")
    }
    added <- setdiff(new_args$name, old_args$name)
    for (arg in added) {
      severity <- if (new_args$required[match(arg, new_args$name)]) "blocking" else "advisory"
      category <- if (severity == "blocking") "added-required-argument" else "added-optional-argument"
      add(severity, category, paste0(old$symbol, "::", arg), "Public function argument was added.")
    }
  }

  if (length(findings) == 0L) {
    return(data.frame(severity = character(), category = character(), item = character(), detail = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, findings)
  out[order(out$severity, out$category, out$item), , drop = FALSE]
}

write_public_api_contract <- function(output_dir, baseline_file = NULL, namespace = "popgenVCF") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  current <- public_api_contract_snapshot(namespace)
  utils::write.table(current, file.path(output_dir, "public-api-current.tsv"), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  if (is.null(baseline_file)) return(invisible(current))
  baseline <- utils::read.delim(
    baseline_file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "",
    quote = ""
  )
  findings <- compare_public_api_contract(baseline, current)
  utils::write.table(findings, file.path(output_dir, "public-api-findings.tsv"), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  summary <- data.frame(entries = nrow(current), blocking_findings = sum(findings$severity == "blocking"), advisory_findings = sum(findings$severity == "advisory"), passed = !any(findings$severity == "blocking"))
  utils::write.table(summary, file.path(output_dir, "public-api-summary.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  if (!summary$passed) stop("Public API contract check failed with ", summary$blocking_findings, " blocking finding(s).", call. = FALSE)
  invisible(list(current = current, findings = findings, summary = summary))
}
