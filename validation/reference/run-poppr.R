#!/usr/bin/env Rscript
if (!requireNamespace("poppr", quietly = TRUE)) stop("Install poppr")
cat("poppr=", as.character(packageVersion("poppr")), "\n", sep = "")
cat("AMOVA reference validation is scheduled for the hierarchical-validation milestone.\n")
