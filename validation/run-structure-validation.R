#!/usr/bin/env Rscript
x <- popgenVCF::run_population_structure_validation(integration = TRUE)
dir.create("validation/reports", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(x$checks, "validation/reports/population_structure.tsv", sep = "\t")
print(x$checks)
if (!x$passed) quit(status = 1L)
