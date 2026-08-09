# Example output

`chr22-quickstart-report.pdf` is the real, complete PDF report produced by running
`popgenVCF::run_pipeline()` against the bundled quickstart dataset
(`inst/extdata/quickstart/`, 160 real 1000 Genomes chromosome 22 samples across
8 populations -- see that directory's `README.md` for full provenance). It is
committed here so a prospective user can see exactly what popgenVCF produces
before installing anything.

Curated figures from this same run are embedded in
[Results and Interpretation](https://github.com/duceppemo/popgenVCF/wiki/Results-and-Interpretation)
(`wiki/figures/`) and the
[interpreting-results vignette](https://duceppemo.github.io/popgenVCF/articles/interpreting-results.html)
(`vignettes/figures/`). Run it yourself with
`vignette("quickstart", package = "popgenVCF")`.

## Regenerating

Source dataset SHA-256 (`inst/extdata/quickstart/chr22_quickstart.vcf.gz`):
`a71db6c3dbcbb85c717bcb246cf9e05f4d6ebdb08b3de0763b9213ade5a2b196`
(unchanged genotype content/sample selection; the hash moved only because
`bcftools view`'s embedded gzip header records the derivation run's
timestamp -- see `inst/extdata/quickstart/README.md` for the coordinate
addition that motivated regenerating the metadata TSV this hash pairs with)

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

analysis <- popgenVCF::run_pipeline(cfg)
# report/population_genomics_report.pdf and .html land in cfg$output$directory
```

Every module used its documented default configuration -- no non-standard
settings were applied for this run. `dapc_k`'s default `"2:10"` range with
cross-validation is the slowest step (a few minutes on a multi-core machine);
everything else completes in well under a minute.
