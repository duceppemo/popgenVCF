# Quickstart example dataset

A real, publicly available subset used to give new users a fast, local, no-network
preview of popgenVCF's output before running their own (larger) data. It is not a
scientific validation fixture -- for that, see `inst/extdata/validation/`.

## Provenance

- Source: `popgenVCF::canonical_1000g_chr22_source()` -- the 1000 Genomes Project
  Phase 3 chromosome 22 callset (GRCh37), the same already-approved source used by
  this package's `production_baseline` scientific gate and continuous-benchmark
  `canonical` tier.
- Region: `22:20000000-21000000` (the same bounded 1Mb interval those gates use),
  restricted to biallelic SNPs.
- Samples: 160 real individuals, 20 each from 8 populations spanning continental
  diversity -- GBR (British, EUR), YRI (Yoruba, AFR), LWK (Luhya, AFR), CHB (Han
  Chinese, EAS), ITU (Indian Telugu, SAS), STU (Sri Lankan Tamil, SAS), PUR (Puerto
  Rican, AMR), PEL (Peruvian, AMR). No further QC/LD filtering has been applied --
  the pipeline's own QC and LD pruning run on this exactly as they would on a real
  user-supplied VCF.
- Real, verifiable relatedness signal: deliberately includes two known real
  duplicate/MZ-twin pairs confirmed against this exact source and region --
  `HG03873`/`HG03998` (a genuine **cross-population** duplicate, ITU/STU) and
  `NA19331`/`NA19334` (same-population, LWK) -- so the bundled kinship demo has
  real, interesting signal rather than an arbitrary sample selection.
- Citation: The 1000 Genomes Project Consortium (2015). A global reference for
  human genetic variation. Nature 526:68-74. doi:10.1038/nature15393
- License: Zenodo open dataset; use subject to record rights (see
  `canonical_1000g_chr22_source()`).

## Regenerating

```bash
Rscript scripts/derive-quickstart-dataset.R <path-to-downloaded-canonical-chr22-source>
```

Deterministic (`set.seed(42)`); re-running against the same source reproduces this
exact sample selection.

## Accessing from R

```r
paths <- popgenVCF::quickstart_dataset_paths()
paths$vcf
paths$metadata
```

See `vignettes/quickstart.Rmd` for a complete walkthrough.
