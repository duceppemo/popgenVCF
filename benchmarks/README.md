# Performance benchmarks

Benchmarks are release diagnostics, not unit tests. They track elapsed time and
approximate peak R memory for deterministic synthetic genotype matrices.

```bash
R CMD INSTALL popgenVCF_0.5.0.tar.gz
POPGENVCF_BENCHMARK_QUICK=true Rscript benchmarks/run-benchmarks.R
Rscript benchmarks/run-benchmarks.R
```

Standard release sizes are 100 x 10,000, 500 x 100,000, and 1,000 x 1,000,000.
The largest case should run only on a machine with sufficient RAM. Results are
compared between releases manually until a dedicated benchmark runner is added.
