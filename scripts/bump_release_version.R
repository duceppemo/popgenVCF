#!/usr/bin/env Rscript
#
# Consolidates the multi-file version-bump/release-cut choreography this
# repository has otherwise re-derived by hand for every one of v1.0.0
# through v1.0.6 (see the GitHub issue this was filed from): editing ~10
# independent files that each declare "the current version" for a
# different consumer, with the public-api-baseline and
# release-candidate-policy re-pins each missed at least once and only
# caught by a subsequent CI failure.
#
# Usage:
#   Rscript scripts/bump_release_version.R X.Y.Z[.9NNN]
#
# A version WITHOUT a .9NNN suffix is a release cut: the existing top
# NEWS.md heading is renamed in place (its version number updated, " development"
# kept -- it does not become a real release until the separate, later DOI
# reconciliation step flips inst/metadata/software-identity.json's own
# release_status). A version WITH a .9NNN suffix is a development-cycle
# bump: a brand-new top NEWS.md heading is inserted above the (unchanged)
# heading of the release it follows. Either way this script updates:
#
#   - DESCRIPTION (Version)
#   - inst/metadata/software-identity.json (version; release_status/
#     date_released/doi are unconditionally reset to development/null/null,
#     since the new version has definitionally not been released yet --
#     DOI reconciliation is a separate, later, DOI-dependent step this
#     script does not attempt)
#   - CITATION.cff (version; date-released and the identifiers: DOI block
#     are removed outright, for the same not-yet-released reason)
#   - codemeta.json (version; datePublished/doi/sameAs removed outright)
#   - .zenodo.json (version; publication_date/doi/conceptdoi and any of
#     conceptrecid/recid/record_id/date_released, if ever present, removed
#     outright -- these are exactly scripts/validate_zenodo_metadata.R's own
#     "prohibited when development" field list)
#   - inst/metadata/release-candidate-policy.json (package_version,
#     target_release -- always "v" + the version with any .9NNN suffix
#     stripped, matching real precedent across every prior cut *and* bump)
#   - NEWS.md (top heading, per the rule above)
#   - README.md's version blockquote (both bolded version numbers; the
#     "past the current stable release" clause is rebuilt from
#     software-identity.json's PRE-bump version/release_status/
#     date_released/doi)
#   - inst/api-contract/public-api-baseline.{dcf,tsv.gz.b64}, via the
#     (already-fixed) tools/update-public-api-baseline.R, IF the installed
#     popgenVCF package version already matches the just-bumped
#     DESCRIPTION -- printed as a manual follow-up otherwise, since a
#     stale install cannot produce a real current signature snapshot
#
# Deliberately NOT touched, and not idempotent to try to automate: docs/ROADMAP.md
# and inst/doc/ROADMAP.md's dated bullet list and the running summary
# paragraph's own prose. Every real entry there describes *what changed
# and why* -- genuine narrative judgment, not a template substitution --
# and every real CI break this tooling gap ever caused was a missed
# structured field (the baseline/policy re-pins), never ROADMAP prose. Add
# your own dated bullet(s) by hand after running this script.
#
# Idempotent: re-running with the same target version is a no-op for any
# field already at that value.

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required", call. = FALSE)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(\\.9[0-9]{3})?$", args[[1L]])) {
  stop(
    "Usage: Rscript scripts/bump_release_version.R X.Y.Z[.9NNN]\n",
    "  e.g. Rscript scripts/bump_release_version.R 1.0.7\n",
    "       Rscript scripts/bump_release_version.R 1.0.7.9000",
    call. = FALSE
  )
}
target_version <- args[[1L]]
is_dev <- grepl("\\.9[0-9]{3}$", target_version)
stripped_version <- sub("\\.9[0-9]{3}$", "", target_version)
target_release <- paste0("v", stripped_version)

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
root <- if (length(script_path)) dirname(dirname(normalizePath(script_path[[1L]], mustWork = TRUE))) else normalizePath(".")
at <- function(...) file.path(root, ...)

changed <- character()
note <- function(...) cat(sprintf(...), "\n", sep = "")
record <- function(path, did_change) {
  if (did_change) changed <<- c(changed, path)
  note("%-55s %s", path, if (did_change) "updated" else "already current")
}

# --- Small, format-preserving helpers -------------------------------------
# Every one of these edits exactly one line via a targeted regex, not a
# full parse/reserialize round-trip: every real precedent commit this
# script's design is based on touched only the changed line, and a JSON/YAML
# library's own re-serialization would reformat the whole file (key order,
# quoting, indentation) into a noisy, unreviewable diff.
replace_line <- function(path, pattern, replacement) {
  lines <- readLines(path, warn = FALSE)
  hit <- grep(pattern, lines)
  if (!length(hit)) stop("Pattern not found in ", path, ": ", pattern, call. = FALSE)
  new_line <- sub(pattern, replacement, lines[[hit[1L]]])
  did_change <- !identical(new_line, lines[[hit[1L]]])
  lines[[hit[1L]]] <- new_line
  if (did_change) writeLines(lines, path, useBytes = TRUE)
  record(path, did_change)
  invisible(did_change)
}

# Removes whole JSON key/value lines matching any of `patterns` (e.g. a
# `"doi": "..."` line), then repairs a now-dangling trailing comma if the
# removed line(s) happened to be the last key before a closing `}`/`]`.
# Idempotent: no matches means no-op. Used only to reset released-metadata
# fields back to a not-yet-released state; never to remove arbitrary content.
remove_json_key_lines <- function(path, patterns) {
  lines <- readLines(path, warn = FALSE)
  hit <- Reduce(function(acc, p) acc | grepl(p, lines), patterns, rep(FALSE, length(lines)))
  if (!any(hit)) { record(path, FALSE); return(invisible(FALSE)) }
  keep <- lines[!hit]
  for (i in seq_along(keep)) {
    if (i < length(keep) && grepl(",\\s*$", keep[[i]]) && grepl("^[}\\]]", trimws(keep[[i + 1L]]))) {
      keep[[i]] <- sub(",\\s*$", "", keep[[i]])
    }
  }
  writeLines(keep, path, useBytes = TRUE)
  record(path, TRUE)
  invisible(TRUE)
}

# CITATION.cff-specific: removes the `date-released:` line and the entire
# `identifiers:` YAML block (from the `identifiers:` line through the last
# line before the next top-level, non-indented key). Idempotent.
remove_cff_release_fields <- function(path) {
  lines <- readLines(path, warn = FALSE)
  date_hit <- grep("^date-released:", lines)
  ids_hit <- which(lines == "identifiers:")
  if (!length(date_hit) && !length(ids_hit)) { record(path, FALSE); return(invisible(FALSE)) }
  remove_idx <- integer(0)
  if (length(date_hit)) remove_idx <- c(remove_idx, date_hit[1L])
  if (length(ids_hit)) {
    start <- ids_hit[1L]
    rest <- lines[(start + 1L):length(lines)]
    next_top <- which(grepl("^[A-Za-z]", rest))[1L]
    end <- if (is.na(next_top)) length(lines) else start + next_top - 1L
    remove_idx <- c(remove_idx, start:end)
  }
  writeLines(lines[-unique(remove_idx)], path, useBytes = TRUE)
  record(path, TRUE)
  invisible(TRUE)
}

# --- DESCRIPTION ------------------------------------------------------------
replace_line(at("DESCRIPTION"), "^Version: .*$", paste0("Version: ", target_version))

# --- inst/metadata/software-identity.json -----------------------------------
identity_path <- at("inst", "metadata", "software-identity.json")
identity_before <- jsonlite::fromJSON(identity_path, simplifyVector = TRUE)
replace_line(identity_path, '"version": *"[^"]*"', sprintf('"version": "%s"', stripped_version))
# Unconditional reset: the new version has definitionally not been released
# yet, regardless of what the prior version's own release_status was (every
# prior cut in this repo's history happened to inherit an already-pending
# development/null state from the version before it -- but the *next* cut
# after a version whose DOI *has* already been reconciled would otherwise
# silently claim the new version shares the old one's release date and DOI,
# a real correctness bug this reset exists specifically to prevent).
replace_line(identity_path, '"release_status": *"[^"]*"', '"release_status": "development"')
replace_line(identity_path, '"date_released": *(null|"[^"]*")', '"date_released": null')
replace_line(identity_path, '"doi": *(null|"[^"]*")', '"doi": null')

# --- CITATION.cff -------------------------------------------------------
replace_line(at("CITATION.cff"), "^version: .*$", paste0("version: ", stripped_version))
remove_cff_release_fields(at("CITATION.cff"))

# --- codemeta.json --------------------------------------------------------
replace_line(at("codemeta.json"), '"version": *"[^"]*"', sprintf('"version": "%s"', stripped_version))
remove_json_key_lines(at("codemeta.json"), c(
  '^\\s*"datePublished":', '^\\s*"doi":', '^\\s*"sameAs":'
))

# --- .zenodo.json ----------------------------------------------------------
replace_line(at(".zenodo.json"), '"version": *"[^"]*"', sprintf('"version": "%s"', stripped_version))
remove_json_key_lines(at(".zenodo.json"), c(
  '^\\s*"doi":', '^\\s*"conceptdoi":', '^\\s*"publication_date":',
  '^\\s*"conceptrecid":', '^\\s*"recid":', '^\\s*"record_id":', '^\\s*"date_released":'
))

# --- inst/metadata/release-candidate-policy.json ----------------------------
policy_path <- at("inst", "metadata", "release-candidate-policy.json")
replace_line(policy_path, '"target_release": *"[^"]*"', sprintf('"target_release": "%s"', target_release))
replace_line(policy_path, '"package_version": *"[^"]*"', sprintf('"package_version": "%s"', target_version))

# --- NEWS.md -----------------------------------------------------------
news_path <- at("NEWS.md")
news_lines <- readLines(news_path, warn = FALSE)
top_heading_idx <- which(grepl("^# popgenVCF ", news_lines))[1L]
if (is.na(top_heading_idx)) stop("No top-level '# popgenVCF ...' heading found in NEWS.md", call. = FALSE)
top_heading <- news_lines[[top_heading_idx]]
already_current <- identical(top_heading, sprintf("# popgenVCF %s development", target_version))
if (already_current) {
  record(news_path, FALSE)
} else if (is_dev) {
  news_lines <- append(
    news_lines, c(sprintf("# popgenVCF %s development", target_version), ""),
    after = top_heading_idx - 1L
  )
  writeLines(news_lines, news_path, useBytes = TRUE)
  record(news_path, TRUE)
} else {
  news_lines[[top_heading_idx]] <- sprintf("# popgenVCF %s development", stripped_version)
  writeLines(news_lines, news_path, useBytes = TRUE)
  record(news_path, TRUE)
}

# --- README.md's version blockquote -----------------------------------------
readme_path <- at("README.md")
readme_lines <- readLines(readme_path, warn = FALSE)
blockquote_idx <- grep("^> Development series: \\*\\*", readme_lines)
if (!length(blockquote_idx)) {
  note(
    "%-55s %s", readme_path,
    "SKIPPED (no '> Development series: **...**' blockquote found -- update it by hand)"
  )
} else {
  old_version <- identity_before$version
  old_status <- identity_before$release_status
  old_date <- identity_before$date_released
  old_doi <- identity_before$doi
  # A vector of separate physical lines, not a single element with an
  # embedded "\n" -- writeLines() would split that into two file lines
  # while an in-memory identical() comparison against readLines() output
  # (which is always one element per line) would never match, making the
  # "already current" check spuriously report a change on every re-run.
  parenthetical_lines <- if (identical(old_status, "released") && !is.null(old_date) && !is.null(old_doi)) {
    c("(released", sprintf("> %s; [DOI %s](https://doi.org/%s)).", old_date, old_doi, old_doi))
  } else {
    c("(released;", "> Zenodo DOI reconciliation for this release is still pending).")
  }
  new_block <- c(
    sprintf(
      "> Development series: **%s** — active development on `main` toward",
      target_version
    ),
    sprintf(
      "> the next release, past the current stable release **%s** %s",
      old_version, parenthetical_lines[[1L]]
    ),
    parenthetical_lines[-1L],
    "> No development build should be treated as release-approved unless its own",
    "> production dossier reports `READY`."
  )
  # The old blockquote is exactly 4 or 5 lines (its own line-wrapping
  # varies with content length); replace through the first line matching
  # the fixed closing sentence.
  close_idx <- blockquote_idx[1L] - 1L + which(grepl(
    "production dossier reports", readme_lines[blockquote_idx[1L]:length(readme_lines)]
  ))[1L]
  did_change <- !identical(readme_lines[blockquote_idx[1L]:close_idx], new_block)
  if (did_change) {
    readme_lines <- append(
      readme_lines[-(blockquote_idx[1L]:close_idx)], new_block, after = blockquote_idx[1L] - 1L
    )
    writeLines(readme_lines, readme_path, useBytes = TRUE)
    n_lines <- length(readme_lines)
    if (n_lines > 120L) {
      note(
        "WARNING: README.md is now %d lines, over its enforced 120-line cap (test-user-documentation.R) -- condense the blockquote by hand.",
        n_lines
      )
    }
  }
  record(readme_path, did_change)
}

# --- Public API baseline ----------------------------------------------------
installed_version <- tryCatch(
  as.character(utils::packageVersion("popgenVCF")), error = function(e) NA_character_
)
if (!is.na(installed_version) && identical(installed_version, target_version)) {
  baseline_script <- at("tools", "update-public-api-baseline.R")
  status <- system2("Rscript", c(shQuote(baseline_script), shQuote(root)))
  if (!identical(status, 0L)) stop("tools/update-public-api-baseline.R failed", call. = FALSE)
} else {
  note(
    paste0(
      "public API baseline                                    ",
      "SKIPPED (installed popgenVCF is %s, not the just-bumped %s -- reinstall, then run:\n",
      "  Rscript tools/update-public-api-baseline.R)"
    ),
    if (is.na(installed_version)) "not installed" else installed_version, target_version
  )
}

# --- Self-check --------------------------------------------------------
note("\nRunning scripts/validate_release_metadata.R as a self-check...")
validate_status <- system2(
  "Rscript", c(shQuote(at("scripts", "validate_release_metadata.R"))),
  stdout = "", stderr = ""
)

note("\n%d file(s) actually changed: %s", length(changed), if (length(changed)) paste(changed, collapse = ", ") else "(none -- already at target)")
note("NOT touched (update by hand): docs/ROADMAP.md, inst/doc/ROADMAP.md -- add a dated bullet describing what this release/cycle actually contains.")
if (!identical(validate_status, 0L)) {
  stop("Self-check failed (scripts/validate_release_metadata.R exited ", validate_status, "); review the output above before committing.", call. = FALSE)
}
note("Self-check passed.")
