#!/usr/bin/env Rscript
#
# Regenerates the tracked, compressed public API baseline
# (inst/api-contract/public-api-baseline.tsv.gz.b64 + .dcf) from the
# installed package's real current exported-function signatures.
#
# This must produce exactly what .github/workflows/public-api-contract.yml
# decodes and verifies:
#   base64 --decode public-api-baseline.tsv.gz.b64 | gzip --decompress > public-api-baseline.tsv
#   sha256sum public-api-baseline.tsv  # must equal the .dcf's Snapshot-SHA256
# -- so this shells out to the real `gzip`/`base64` command-line tools
# rather than R's memCompress()/base64 helpers: on this platform,
# memCompress(type = "gzip") was confirmed to emit a zlib stream (magic
# bytes 78 9c), not a true gzip stream (1f 8b) -- `gzip --decompress`
# rejects it. The CLI tools are already a hard requirement for this
# workflow (it decodes/decompresses with them itself), so this adds no new
# tooling dependency.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."
encoded <- if (length(args) >= 2L) args[[2L]] else file.path(root, "inst", "api-contract", "public-api-baseline.tsv.gz.b64")
metadata <- if (length(args) >= 3L) args[[3L]] else file.path(root, "inst", "api-contract", "public-api-baseline.dcf")

root <- normalizePath(root, winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "public_api_contract.R"), local = .GlobalEnv)

if (!requireNamespace("popgenVCF", quietly = TRUE)) {
  stop("Install popgenVCF before refreshing the public API baseline.", call. = FALSE)
}
gzip_bin <- Sys.which("gzip")
base64_bin <- Sys.which("base64")
if (!nzchar(gzip_bin) || !nzchar(base64_bin)) {
  stop("gzip and base64 must both be on PATH to write the compressed baseline.", call. = FALSE)
}

# public-api-contract.yml reconciles a .9NNN-suffixed dev version against
# its released X.Y.Z prefix (see its own comment: "the baseline stays
# pinned to the last release the public API was actually audited against,
# and is re-pinned to a new exact version only when the API itself
# changes") -- writing the raw dev version here would desync from what
# that check actually compares against on every future run until the next
# release, not just this one.
installed_version <- as.character(utils::packageVersion("popgenVCF"))
version <- sub("^([0-9]+\\.[0-9]+\\.[0-9]+)\\.9[0-9]{3}$", "\\1", installed_version)
dir.create(dirname(encoded), recursive = TRUE, showWarnings = FALSE)
snapshot <- public_api_contract_snapshot("popgenVCF")

tsv_tmp <- tempfile(fileext = ".tsv")
on.exit(unlink(tsv_tmp), add = TRUE)
utils::write.table(snapshot, tsv_tmp, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
snapshot_sha256 <- digest::digest(file = tsv_tmp, algo = "sha256")

gz_tmp <- tempfile(fileext = ".gz")
on.exit(unlink(gz_tmp), add = TRUE)
gz_status <- system2(gzip_bin, c("-9", "-c", shQuote(tsv_tmp)), stdout = gz_tmp)
if (!identical(gz_status, 0L) || !file.exists(gz_tmp)) {
  stop("gzip compression of the baseline snapshot failed.", call. = FALSE)
}
b64_status <- system2(base64_bin, shQuote(gz_tmp), stdout = encoded)
if (!identical(b64_status, 0L) || !file.exists(encoded)) {
  stop("base64 encoding of the compressed baseline snapshot failed.", call. = FALSE)
}

writeLines(c(
  paste0("Package: popgenVCF"),
  paste0("Version: ", version),
  "Contract-Format: 1",
  paste0("Entries: ", nrow(snapshot)),
  "Snapshot-Encoding: gzip+base64",
  paste0("Snapshot-SHA256: ", snapshot_sha256)
), metadata, useBytes = TRUE)

cat(
  "Updated public API baseline for popgenVCF ", version, " (", nrow(snapshot), " entries) at ",
  encoded, "\n", sep = ""
)
