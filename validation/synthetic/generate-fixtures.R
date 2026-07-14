#!/usr/bin/env Rscript
# The canonical fixture is checked into inst/extdata/validation.
# This script verifies that its hand-calculated tables remain reproducible.
x <- popgenVCF::run_scientific_validation(integration = FALSE)
print(x$checks)
if (!x$passed) quit(status = 1L)
