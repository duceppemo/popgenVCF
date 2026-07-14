#!/usr/bin/env bash
set -euo pipefail

MANAGER="${POPGENVCF_CONDA_MANAGER:-mamba}"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENV_FILE="$ROOT_DIR/inst/conda/environment.yml"

if ! command -v "$MANAGER" >/dev/null 2>&1; then
  echo "Conda-compatible manager not found: $MANAGER" >&2
  echo "Set POPGENVCF_CONDA_MANAGER to mamba, micromamba, or conda." >&2
  exit 1
fi

"$MANAGER" env create --file "$ENV_FILE"

cat <<'MSG'
Core environment created.

Next commands:
  conda activate popgenvcf
  Rscript popgenVCF/inst/scripts/install-bioconductor.R
  R CMD build popgenVCF
  R CMD INSTALL popgenVCF_0.8.3.tar.gz
MSG
