#!/usr/bin/env Rscript
#
# Re-pins packaging/bioconda/r-popgenvcf/meta.yaml's version/sha256 to a
# real, already-published GitHub Release. Consolidates the multi-step dance
# that was previously done by hand for v1.0.13's submission: fetch the
# release's own release-SHA256SUMS.txt, extract the source tarball's
# checksum, independently re-verify it by downloading the actual tarball
# and hashing it directly (never trust a manifest alone), then rewrite only
# the two `{% set %}` lines in meta.yaml -- the rest of the recipe (Jinja
# comments, dependency list, test commands) is untouched.
#
# Usage:
#   Rscript scripts/update_bioconda_recipe.R X.Y.Z
#
# Requires the `gh` CLI, authenticated, with access to duceppemo/popgenVCF.
#
# Deliberately does NOT push to the bioconda-recipes fork or touch any PR --
# submitting/updating a PR against an external, third-party repository is a
# real, externally-visible action that should stay a deliberate, reviewed
# step each time, not something a version-bump script does unattended. This
# script only updates this repository's own copy of the recipe and prints
# the exact follow-up commands.

suppressPackageStartupMessages({
  if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required", call. = FALSE)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", args[[1L]])) {
  stop(
    "Usage: Rscript scripts/update_bioconda_recipe.R X.Y.Z\n",
    "  e.g. Rscript scripts/update_bioconda_recipe.R 1.0.14\n",
    "  (a released version only -- no .9NNN dev suffix; the release's real\n",
    "  GitHub Release assets must already exist)",
    call. = FALSE
  )
}
version <- args[[1L]]
tag <- paste0("v", version)
tarball_name <- sprintf("popgenVCF_%s.tar.gz", version)

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
root <- if (length(script_path)) dirname(dirname(normalizePath(script_path[[1L]], mustWork = TRUE))) else normalizePath(".")
recipe_path <- file.path(root, "packaging", "bioconda", "r-popgenvcf", "meta.yaml")
if (!file.exists(recipe_path)) stop("Recipe not found: ", recipe_path, call. = FALSE)

note <- function(...) cat(sprintf(...), "\n", sep = "")

# --- Confirm the release actually exists and has the source tarball --------
note("Checking that %s exists with a source tarball asset...", tag)
assets <- system2(
  "gh", c("release", "view", shQuote(tag), "--repo", "duceppemo/popgenVCF", "--json", "assets", "--jq", ".assets[].name"),
  stdout = TRUE, stderr = TRUE
)
if (!is.null(attr(assets, "status")) && attr(assets, "status") != 0L) {
  stop("Could not find release ", tag, " on duceppemo/popgenVCF:\n", paste(assets, collapse = "\n"), call. = FALSE)
}
if (!tarball_name %in% assets) {
  stop(
    "Release ", tag, " exists but has no '", tarball_name, "' asset yet -- ",
    "the tagged-source-release.yml workflow may still be running. Wait for it ",
    "to finish, then re-run this script.",
    call. = FALSE
  )
}

# --- Fetch the manifest sha256 ----------------------------------------------
tmpdir <- tempfile("bioconda-recipe-update-")
dir.create(tmpdir)
on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

note("Downloading %s's release-SHA256SUMS.txt...", tag)
dl_status <- system2(
  "gh", c(
    "release", "download", shQuote(tag), "--repo", "duceppemo/popgenVCF",
    "--pattern", "release-SHA256SUMS.txt", "--dir", shQuote(tmpdir)
  ),
  stdout = TRUE, stderr = TRUE
)
manifest_path <- file.path(tmpdir, "release-SHA256SUMS.txt")
if (!file.exists(manifest_path)) stop("Failed to download release-SHA256SUMS.txt:\n", paste(dl_status, collapse = "\n"), call. = FALSE)

manifest_lines <- readLines(manifest_path, warn = FALSE)
manifest_hit <- grep(paste0("  ", tarball_name, "$"), manifest_lines, value = TRUE)
if (length(manifest_hit) != 1L) {
  stop("Expected exactly one manifest line for ", tarball_name, ", found ", length(manifest_hit), call. = FALSE)
}
manifest_sha256 <- trimws(sub("\\s+.*$", "", manifest_hit))

# --- Independently re-verify by hashing the actual tarball ------------------
note("Downloading %s itself and re-hashing it independently (never trust the manifest alone)...", tarball_name)
dl2_status <- system2(
  "gh", c(
    "release", "download", shQuote(tag), "--repo", "duceppemo/popgenVCF",
    "--pattern", tarball_name, "--dir", shQuote(tmpdir)
  ),
  stdout = TRUE, stderr = TRUE
)
tarball_path <- file.path(tmpdir, tarball_name)
if (!file.exists(tarball_path)) stop("Failed to download ", tarball_name, ":\n", paste(dl2_status, collapse = "\n"), call. = FALSE)

actual_sha256 <- digest::digest(file = tarball_path, algo = "sha256")
if (!identical(actual_sha256, manifest_sha256)) {
  stop(
    "sha256 MISMATCH between release-SHA256SUMS.txt (", manifest_sha256, ") ",
    "and the actual downloaded tarball (", actual_sha256, "). Do not proceed -- ",
    "this indicates a real integrity problem with the release assets.",
    call. = FALSE
  )
}
note("Verified: manifest and independently-hashed tarball agree (%s).", actual_sha256)

# --- Rewrite meta.yaml's two {% set %} lines --------------------------------
replace_line <- function(path, pattern, replacement) {
  lines <- readLines(path, warn = FALSE)
  hit <- grep(pattern, lines)
  if (!length(hit)) stop("Pattern not found in ", path, ": ", pattern, call. = FALSE)
  new_line <- sub(pattern, replacement, lines[[hit[1L]]])
  did_change <- !identical(new_line, lines[[hit[1L]]])
  lines[[hit[1L]]] <- new_line
  if (did_change) writeLines(lines, path, useBytes = TRUE)
  note("%-55s %s", path, if (did_change) "updated" else "already current")
  invisible(did_change)
}

replace_line(recipe_path, '^\\{% set version = "[^"]*" %\\}$', sprintf('{%% set version = "%s" %%}', version))
replace_line(recipe_path, '^\\{% set sha256 = "[^"]*" %\\}$', sprintf('{%% set sha256 = "%s" %%}', actual_sha256))

note("\nDone. Next steps (deliberately not automated by this script):")
note("  1. Review the diff: git diff %s", recipe_path)
note("  2. If bioconda-utils is installed, re-lint before pushing:")
note("       bioconda-utils lint packaging/bioconda/ --packages r-popgenvcf")
note("     (note: bioconda-utils expects the recipe under recipes/<name>/,")
note("     so lint from a checkout of the fork/PR branch, not this path directly)")
note("  3. Commit this repo's copy: git add %s && git commit", recipe_path)
note("  4. Copy the updated file into your bioconda-recipes fork's PR branch")
note("     (or a fresh 'update r-popgenvcf' branch if the prior PR already")
note("     merged), push, and open/update the PR yourself -- this script")
note("     deliberately does not touch that external repository.")
