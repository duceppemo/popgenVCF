# Numerical validation

Validation has three layers:

1. **Structural validation** checks required fields, dimensions, sample IDs, and
   finite values.
2. **Scientific validation** checks bounded statistics, symmetric matrices,
   zero diagonals, variance proportions, convergence, and sample-size rules.
3. **Reference validation** compares deterministic fixtures with trusted
   implementations or hand-calculated expectations.

Each statistical module must add a reference fixture before its API is declared
stable. Tolerances must be explicit and justified rather than inferred from
printed output.
