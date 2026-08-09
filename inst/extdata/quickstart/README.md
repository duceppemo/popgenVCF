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
- Geographic coordinates: the metadata TSV includes `latitude`/`longitude`
  columns with real, documented population collection-site coordinates, source:
  `igsr/1000Genomes_data_indexes` `README_populations.md`
  (https://github.com/igsr/1000Genomes_data_indexes). GBR (England and
  Scotland), ITU (Indian Telugu, collected in the UK), and STU (Sri Lankan
  Tamil, collected in the UK) all share one representative London point
  (51.5074, -0.1278) since the source gives no more specific city for any of
  the three -- this is expected, not an error: ITU and STU are genuinely
  UK-collected diaspora cohorts, not India/Sri Lanka fieldwork. YRI is Ibadan,
  Nigeria (7.3776, 3.9059); LWK is Webuye, Kenya (0.6075, 34.7697); CHB is
  Beijing, China (39.9042, 116.4074); PUR is Puerto Rico, represented by San
  Juan (18.4655, -66.1057); PEL is Lima, Peru (-12.0464, -77.0428). These are
  population-level representative coordinates, not per-sample GPS --
  individual sample locations are never published for de-identified 1000
  Genomes samples, and population-level points are the standard granularity
  for this kind of isolation-by-distance analysis. This is what makes
  `run_pipeline()`'s Mantel/isolation-by-distance module actually execute
  (rather than skip) against this dataset.
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
