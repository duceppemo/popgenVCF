# Loaded after projects.R so project construction never advances R's analysis RNG.
project_uuid <- function() {
  seed <- list(
    time = format(Sys.time(), tz = "UTC", usetz = TRUE),
    pid = Sys.getpid(),
    temporary_identity = tempfile("popgenvcf-project-id-"),
    host = unname(Sys.info()[["nodename"]])
  )
  hash <- digest::digest(seed, algo = "sha256", serialize = TRUE)
  paste(substr(hash, 1L, 8L), substr(hash, 9L, 12L), substr(hash, 13L, 16L),
        substr(hash, 17L, 20L), substr(hash, 21L, 32L), sep = "-")
}
