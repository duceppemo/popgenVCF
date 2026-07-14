# Contributing to popgenVCF

Contributions should preserve scientific semantics, reproducibility, and backward compatibility.

1. Create a focused branch.
2. Add or update tests for every behavioral change.
3. Run `R CMD build` and `R CMD check --as-cran` on the source tarball.
4. Document statistical changes in `NEWS.md` and the relevant help page.
5. Avoid introducing automatic package installation inside analysis functions.
6. Keep canonical outputs machine-readable; formatted reports are derived products.
7. Include numerical comparison against an independent implementation for new estimators when feasible.

All pull requests should explain the scientific rationale, expected numerical behavior, computational cost, and compatibility impact.
