# Loaded after publication_report_rendering.R so class attributes never affect
# canonical record fingerprints.
.publication_report_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}
