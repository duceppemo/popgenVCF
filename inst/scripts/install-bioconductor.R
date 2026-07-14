#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

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
