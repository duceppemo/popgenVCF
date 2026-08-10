# Example output

`chr22-quickstart-report.pdf` is the real, complete PDF report produced by running
`popgenVCF::run_pipeline()` against the bundled quickstart dataset
(`inst/extdata/quickstart/`, 160 real 1000 Genomes chromosome 22 and X samples
across 8 populations -- see that directory's `README.md` for full provenance). It
is committed here so a prospective user can see exactly what popgenVCF produces
before installing anything.

Curated figures from this same run are embedded in
[Results and Interpretation](https://github.com/duceppemo/popgenVCF/wiki/Results-and-Interpretation)
(`wiki/figures/`) and the
[interpreting-results vignette](https://duceppemo.github.io/popgenVCF/articles/interpreting-results.html)
(`vignettes/figures/`).

Running `vignette("quickstart", package = "popgenVCF")` yourself uses pure
`default_config()` and will **not** reproduce this exact file -- it omits
the ADMIXTURE, fastStructure, and sNMF sections described below, since all
three ancestry backends are off by default (see [Installing and configuring
ancestry
backends](https://github.com/duceppemo/popgenVCF/blob/main/docs/user/ancestry-backends.md)).
Use the exact snippet under "Regenerating" to reproduce this committed PDF
precisely.

## Regenerating

Source dataset SHA-256 (`inst/extdata/quickstart/chr22_quickstart.vcf.gz`):
`5cfd9364158e85384a61cf16df39668e1f4e4cb50249960760331d68c0d627c4`
(a real content change this time, not just a timestamp artifact: the file now
also includes a bounded, non-PAR chromosome X region for the same 160
samples, giving the sex-check module real data to demonstrate -- see
`inst/extdata/quickstart/README.md` for full provenance, including a real
data-preparation finding about this 1000 Genomes release's haploid male
chromosome X genotype representation)

```r
paths <- popgenVCF::quickstart_dataset_paths()
cfg <- popgenVCF::default_config()
cfg$input$vcf <- paths$vcf
cfg$input$metadata <- paths$metadata
cfg$output$directory <- tempfile("popgenvcf-quickstart-")
cfg$compute$threads <- max(1L, parallel::detectCores() - 1L)
cfg$report$enabled <- TRUE
cfg$report$title <- "popgenVCF quickstart example: chromosome 22 subset (160 samples, 8 populations)"
cfg$report$author <- "popgenVCF"

# One-off enablement for this reference run only -- all three stay disabled
# in default_config() (external, optional dependencies; see
# docs/user/ancestry-backends.md). Enabled here purely to source the real
# cluster-number-selection and Q-matrix figures shown in the wiki and
# interpreting-results vignette, which otherwise have no ancestry figures
# at all since none of the three backends run by default.
cfg$analyses$admixture$enabled <- TRUE
cfg$analyses$admixture$k <- "2:9"
cfg$analyses$admixture$cv_folds <- 5L
cfg$analyses$admixture$threads <- "auto"
cfg$analyses$faststructure$enabled <- TRUE
cfg$analyses$faststructure$k <- "2:9"
cfg$analyses$snmf$enabled <- TRUE
cfg$analyses$snmf$k <- "2:9"
cfg$analyses$snmf$repetitions <- 5L
cfg$analyses$snmf$entropy <- TRUE
cfg$analyses$snmf$threads <- "auto"

analysis <- popgenVCF::run_pipeline(cfg)
# report/population_genomics_report.pdf and .html land in cfg$output$directory
```

Every other module used its documented default configuration -- the three
ancestry backends above are the deliberate exception, added for this
reference run only. `dapc_k`'s default `"2:10"` range with cross-validation
is the slowest default-on step (a few minutes on a multi-core machine);
ADMIXTURE, fastStructure, and sNMF's `K = 2:9` sweeps each add under a
minute on this small (357-SNP) marker set; everything else completes in
well under a minute.
