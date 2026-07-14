# Scientific validation

The validation suite has three tiers.

1. **Deterministic checks** use the bundled designed VCF and hand-calculated expected values. These run during `R CMD check`.
2. **Numerical equivalence** compares PCA after sign alignment and estimates within explicit tolerances.
3. **External integration** compares output with PLINK 2, hierfstat, adegenet, poppr, vegan, and ADMIXTURE. These are manual or CI jobs because they require external tools and are not appropriate for ordinary package checks.

Run the bundled suite after installation:

```r
popgenVCF::run_scientific_validation()
popgenVCF::run_scientific_validation(integration = TRUE)
```

Run all reference scripts from the repository root with `validation/run-validation.sh`. Each output includes tool versions, input hashes, tolerances, and pass/fail status. Private project data are never bundled.


## v0.7 core numerical equivalence

The integration suite additionally validates:

- SNPRelate IBS against direct diploid genotype-sharing arithmetic;
- MDS against `cmdscale()` applied to the independently calculated distance;
- SNPRelate PCA against a standardized-dosage SVD by canonical subspace correlation;
- population Ho, unbiased He, and FIS against direct allele-count calculations;
- global and pairwise Weir-Cockerham FST against explicit Weir-Cockerham 1984 variance-component calculations; `hierfstat` is retained as a cross-tool diagnostic.

PCA is intentionally compared as an eigenspace rather than by raw axis signs.
