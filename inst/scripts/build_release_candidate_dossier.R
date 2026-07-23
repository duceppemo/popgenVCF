#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
module_dir <- dirname(script_path)
for (module in c(
  "release_candidate_utils.R",
  "release_candidate_policy.R",
  "release_candidate_evaluate.R",
  "release_candidate_write.R",
  "release_candidate_main.R"
)) {
  sys.source(file.path(module_dir, module), envir = globalenv())
}

main(commandArgs(trailingOnly = TRUE))
