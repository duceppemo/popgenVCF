#!/usr/bin/env Rscript
cran <- c("data.table", "ggplot2", "ggrepel", "scales", "viridisLite", "ape",
          "adegenet", "poppr", "vegan", "rmarkdown", "knitr",
          "yaml", "digest", "ade4", "testthat", "remotes")
missing <- cran[!vapply(cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager", repos = "https://cloud.r-project.org")
bioc <- c("SNPRelate", "gdsfmt")
missing_bioc <- bioc[!vapply(bioc, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_bioc)) BiocManager::install(missing_bioc, ask = FALSE, update = FALSE)
root <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
remotes::install_local(root, upgrade = "never")
