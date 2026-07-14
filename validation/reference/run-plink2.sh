#!/usr/bin/env bash
set -euo pipefail
PLINK2=${PLINK2:-plink2}
VCF=${1:-inst/extdata/validation/core_validation.vcf}
OUT=${2:-validation/reports/plink2_core}
mkdir -p "$(dirname "$OUT")"
"$PLINK2" --version > "${OUT}.version.txt"
"$PLINK2" --vcf "$VCF" --allow-extra-chr --make-pgen --out "$OUT"
"$PLINK2" --pfile "$OUT" --freq --missing sample-only --missing variant-only --out "$OUT"
"$PLINK2" --pfile "$OUT" --indep-pairwise 50 1 0.2 --out "$OUT"
sha256sum "$VCF" > "${OUT}.input.sha256"
