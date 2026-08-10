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
- Chromosome X: also includes a bounded, non-PAR 1Mb region
  (`X:70000000-71000000`; PAR1 ends at 2,699,520 and PAR2 starts at
  154,931,044 in GRCh37, so this region is safely hemizygous-only in males),
  restricted to biallelic SNPs, from the same already-approved Zenodo record
  (`10.5281/zenodo.3359882`) the chr22 source uses --
  `ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz`,
  MD5-verified against the record's declared checksum before use. This gives
  the sex-check module (`analyses.sex_check`) real data to demonstrate,
  since it needs chromosome X SNPs and the chr22-only dataset had none.
  **A real, load-bearing data-preparation finding**: this release represents
  males' non-PAR chromosome X genotypes as genuinely haploid VCF `GT` fields
  (a single allele, e.g. `0` or `1`, not `0/0`/`1/1`) -- confirmed by direct
  inspection, and `SNPRelate::snpgdsVCF2GDS()` does not parse a haploid `GT`
  field as "duplicate the observed allele": empirically, it pads the missing
  second allele with the ALT allele index, silently turning every male
  REF-hemizygous call into a false heterozygous dosage and every male
  ALT-hemizygous call into a homozygous-ALT dosage. Left unfixed, this would
  corrupt genotypes at these sites for every module, not just sex-check.
  Fixed at derivation time with `bcftools +fixploidy -- -f 2` (correct here
  specifically because the extracted region is entirely non-PAR; already
  diploid female calls are left untouched, verified before shipping).
- Chromosome Y: also includes the whole callable non-PAR region this 1000
  Genomes release ships (~60,505 biallelic SNPs, already a small curated
  set -- no further bounding needed), from `canonical_1000g_chrY_source()`'s
  already-approved, checksummed source. This release is genuinely
  male-only (chromosome Y has no biological meaning for females): the
  source VCF's sample list is a strict subset containing only males. The
  83 selected female samples are added at derivation time as explicit,
  correctly-missing genotype columns (`bcftools merge` with a header-only
  placeholder VCF) at the same real sites -- not fabricated data, just an
  honest representation of "no chromosome Y", exactly what a real chrY VCF
  that included females would show. Gives the sex-check module a second,
  much cleaner corroborating signal (Y-chromosome call rate) alongside
  chromosome X heterozygosity.
  **Two further real, load-bearing findings, both more severe than the
  chromosome X ploidy issue above, found while adding this**: (1) sample-
  level missingness QC (`harmonize_samples()`) computed missingness across
  *all* SNPs including chromosome Y; since chromosome Y is ~100% "missing"
  for one whole sex by biology, not by data-quality problem, this silently
  dropped every sample of the unaffected sex out of the *entire pipeline*
  before this was fixed. (2) Per-SNP missingness QC (`variant_qc()`)
  likewise failed every chromosome Y SNP out of QC for the same reason,
  leaving sex-check with zero chromosome Y markers. Both are now fixed:
  sample-level QC restricts its missingness calculation to autosomal
  markers by default, and variant-level QC exempts configured sex-limited
  chromosomes (`analyses.sex_check_y_chromosome_names`) from the
  missingness threshold specifically (MAF filtering is unaffected). See
  `NEWS.md` for the full story and real before/after numbers.
- Samples: 160 real individuals, 20 each from 8 populations spanning continental
  diversity -- GBR (British, EUR), YRI (Yoruba, AFR), LWK (Luhya, AFR), CHB (Han
  Chinese, EAS), ITU (Indian Telugu, SAS), STU (Sri Lankan Tamil, SAS), PUR (Puerto
  Rican, AMR), PEL (Peruvian, AMR). No further QC/LD filtering has been applied --
  the pipeline's own QC and LD pruning run on this exactly as they would on a real
  user-supplied VCF.
- Real, verifiable relatedness signal: deliberately includes a known real
  duplicate/MZ-twin pair, `NA19331`/`NA19334` (same-population, LWK) -- kinship
  ~0.446, confirmed consistent on both chromosome X (real hemizygous
  genotypes) and chromosome Y (real, matching, high call rate) too, both
  genuinely male -- so the bundled kinship demo has real, interesting signal
  rather than an arbitrary sample selection.
  Also includes `HG03873`/`HG03998` (labelled as two *different* populations,
  ITU/STU), a real, high chr22-only kinship pair (~0.453, correctly classified
  `duplicate/MZ twin` by KING's threshold) that chromosome X data added later
  revealed is *not* actually a duplicate: their real chromosome X genotypes
  show genuinely different sexes, and MZ twins/duplicates share genetic sex by
  definition. A deliberately kept, real cautionary example -- the autosomal
  kinship signal is real, not a computation error, but a single small
  autosomal window is not sufficient evidence for "duplicate" on its own; see
  the sex-check section of
  [Results and Interpretation](https://github.com/duceppemo/popgenVCF/wiki/Results-and-Interpretation).
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
- Sex: the metadata TSV includes a `sex` column with real, self-reported
  values (`male`/`female`) from the same authoritative panel file already
  used for population assignment
  (`integrated_call_samples_v3.20130502.ALL.panel`'s `gender` column) -- not
  a separate lookup, an inference, or a fabricated value. 77 male / 83
  female across the 160 samples. This is what makes any `sex`-based
  per-metadata-column PCA panel (`07b_PCA_PC1_PC2_by_sex`) actually appear
  in the rendered report, since it satisfies the default minimum-group-size
  threshold in every one of the 8 populations.
- Citation: The 1000 Genomes Project Consortium (2015). A global reference for
  human genetic variation. Nature 526:68-74. doi:10.1038/nature15393
- License: Zenodo open dataset; use subject to record rights (see
  `canonical_1000g_chr22_source()`).

## Regenerating

```bash
Rscript scripts/derive-quickstart-dataset.R <source-data-dir>
```

`<source-data-dir>` must contain the canonical chr22 VCF/index/panel, the
chrX VCF/index (`ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz`
and its `.tbi`), and the chrY VCF/index (`ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz`
and its `.tbi`), all from the same Zenodo record, co-located together.
Deterministic (`set.seed(42)`); re-running against the same sources reproduces
this exact sample selection.

## Accessing from R

```r
paths <- popgenVCF::quickstart_dataset_paths()
paths$vcf
paths$metadata
```

See `vignettes/quickstart.Rmd` for a complete walkthrough.
