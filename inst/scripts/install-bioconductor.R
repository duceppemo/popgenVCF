#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

# See NEWS.md / ROADMAP.md: a 2026-08-18 apptainer.yml build failed
# compiling gdsfmt's vendored liblzma with a silent Error 1, four times in
# a row, deterministically, with disk exhaustion, a toolchain/C-standard
# mismatch, and make's own -j parallelism all ruled out by direct
# evidence. install.packages()/BiocManager::install() can install
# multiple packages concurrently (separate R subprocesses) when
# getOption("Ncpus") > 1 -- a different axis of parallelism than make -j,
# and the one difference left untested between the failing CI build
# (which installs gdsfmt/SNPRelate/BiocVersion together in one call) and
# an earlier local repro (a single, standalone gdsfmt install). Ncpus
# defaults to 1L in stock R, but conda-forge's r-base recipe is known to
# raise it via a site profile on some builds; setting it explicitly here
# is a safe no-op if the default was already 1.
options(Ncpus = 1)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

required <- c("gdsfmt", "SNPRelate")
optional <- c("LEA")

message("Bioconductor version selected for this R installation: ",
        as.character(BiocManager::version()))

BiocManager::install(required, ask = FALSE, update = FALSE)

install_optional <- identical(
  tolower(Sys.getenv("POPGENVCF_INSTALL_LEA", "true")),
  "true"
)

if (install_optional) {
  BiocManager::install(optional, ask = FALSE, update = FALSE)
}

pkgs <- c(required, if (install_optional) optional else character())
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Failed to install required Bioconductor package(s): ",
       paste(missing, collapse = ", "))
}

message("Bioconductor packages installed successfully: ",
        paste(pkgs, collapse = ", "))
message("BiocManager::valid() result:")
print(BiocManager::valid())
