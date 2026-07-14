#!/usr/bin/env Rscript
files <- list.files("validation/reports", pattern = "\\.tsv$", full.names = TRUE)
if (!length(files)) stop("No validation reports found")
out <- data.table::rbindlist(lapply(files, function(path) {
  x <- data.table::fread(path)
  x[, source_file := basename(path)]
  x
}), fill = TRUE)
data.table::fwrite(out, "validation/reports/validation_summary.tsv", sep = "\t")
print(out)
