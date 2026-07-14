# Scientific validation policy

A method is not considered stable until it has deterministic fixture coverage, a trusted independent reference implementation, an explicit tolerance, and a recorded tool version. The bundled synthetic VCF is designed to exercise missingness, monomorphic loci, exact LD duplicates, chromosome boundaries, differentiation, and spatial metadata.

External real datasets are downloaded or supplied locally and are never silently redistributed. Dataset manifests must include a license and SHA-256 checksum.


## Core numerical validation (v0.7.0)

Core ordination, relatedness, diversity, and differentiation are now compared
against hand calculations, covariance eigendecomposition, and optional external-tool diagnostics. The complete installed
validation command is:

```r
x <- popgenVCF::run_scientific_validation(integration = TRUE)
stopifnot(x$passed)
```
