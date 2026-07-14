#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(popgenVCF))
tryCatch(popgenVCF::cli_main(), error = function(e) {
  cat(sprintf("[ERROR] %s\n", conditionMessage(e)), file = stderr())
  quit(save = "no", status = 1L)
})
