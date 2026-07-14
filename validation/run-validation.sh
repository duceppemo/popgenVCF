#!/usr/bin/env bash
set -euo pipefail
mkdir -p validation/reports
Rscript -e 'x <- popgenVCF::run_scientific_validation(integration=TRUE); data.table::fwrite(x$checks, "validation/reports/core.tsv", sep="\t"); if (!x$passed) quit(status=1)'
for script in run-hierfstat.R run-adegenet.R run-vegan.R; do
  Rscript "validation/reference/${script}" || echo "Reference ${script} unavailable or failed"
done
if command -v plink2 >/dev/null 2>&1; then validation/reference/run-plink2.sh; else echo "plink2 unavailable; skipping PLINK2 validation"; fi
Rscript validation/report-validation.R
