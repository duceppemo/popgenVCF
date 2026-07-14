#' Benchmark a popgenVCF expression
#'
#' @param label Benchmark label.
#' @param expr Expression to evaluate.
#' @return One-row data table with elapsed time and garbage-collection change.
#' @export
benchmark_stage <- function(label, expr) {
  before <- gc(reset = TRUE)
  t0 <- proc.time()
  value <- force(expr)
  elapsed <- unname((proc.time() - t0)[["elapsed"]])
  after <- gc()
  bytes_per_cell <- c(Ncells = 56, Vcells = 8)
  mem_mb <- sum(after[, "max used"] * bytes_per_cell[rownames(after)]) / 1024^2
  list(value = value, metrics = data.table::data.table(
    label = as.character(label), elapsed_seconds = elapsed,
    approximate_peak_mb = mem_mb, timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ))
}

#' Generate a deterministic synthetic diploid genotype matrix
#'
#' @param samples Number of individuals.
#' @param snps Number of SNPs.
#' @param seed Random seed.
#' @return Sample-by-SNP integer matrix with values 0, 1, and 2.
#' @export
synthetic_genotypes <- function(samples = 100L, snps = 10000L, seed = 1L) {
  stopifnot(samples >= 2L, snps >= 2L)
  set.seed(seed)
  p <- stats::rbeta(snps, 0.8, 0.8)
  matrix(stats::rbinom(samples * snps, 2L, rep(p, each = samples)), nrow = samples,
         dimnames = list(paste0("sample", seq_len(samples)), paste0("snp", seq_len(snps))))
}
