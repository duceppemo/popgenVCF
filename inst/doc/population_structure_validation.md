# Population-structure validation

popgenVCF 0.8.3 validates population-structure outputs at three levels.

1. **Matrix validity**: membership values are finite, non-negative, and rows sum to one.
2. **Label-switching invariance**: replicate matrices are aligned through an assignment problem before differences are calculated.
3. **Reproducibility**: aligned replicate RMSE and minimum per-cluster correlations are recorded.

The deterministic suite is available with:

```r
popgenVCF::run_population_structure_validation()
```

The optional integration suite also fits DAPC to a strongly separated synthetic genotype matrix:

```r
popgenVCF::run_population_structure_validation(integration = TRUE)
```

External ADMIXTURE, fastStructure, and sNMF analyses are optional. They require explicit input sample order; popgenVCF never infers sample order from numeric Q-matrix columns.
