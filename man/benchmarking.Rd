\name{benchmarking}
\alias{benchmarking}
\alias{benchmark_stage}
\alias{synthetic_genotypes}
\title{Deterministic performance benchmarking helpers}
\description{Generate synthetic diploid genotypes and record elapsed time and approximate R memory usage for a benchmark expression.}
\usage{
benchmark_stage(label, expr)

synthetic_genotypes(samples = 100L, snps = 10000L, seed = 1L)
}
\arguments{
  \item{label}{Benchmark label.}
  \item{expr}{Expression to evaluate.}
  \item{samples}{Number of individuals.}
  \item{snps}{Number of SNPs.}
  \item{seed}{Random seed.}
}
\value{\code{benchmark_stage()} returns the value and a metrics table. \code{synthetic_genotypes()} returns a sample-by-SNP integer matrix.}
