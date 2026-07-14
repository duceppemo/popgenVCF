#!/usr/bin/env bash
set -euo pipefail

fail=0
check_cmd() {
  local cmd="$1"
  local required="${2:-required}"
  if command -v "$cmd" >/dev/null 2>&1; then
    printf '[OK]   %-16s %s\n' "$cmd" "$(command -v "$cmd")"
  else
    printf '[%s] %-16s not found\n' "${required^^}" "$cmd" >&2
    [[ "$required" == "required" ]] && fail=1
  fi
}

echo 'External executables'
check_cmd R
check_cmd Rscript
check_cmd plink optional
check_cmd plink2 optional
check_cmd admixture optional
check_cmd structure.py optional
check_cmd chooseK.py optional
check_cmd bcftools optional
check_cmd tabix optional
check_cmd vcftools optional
check_cmd pandoc optional
check_cmd qpdf optional
check_cmd tidy optional
check_cmd pdflatex optional

echo
echo 'R package namespaces'
Rscript --vanilla - <<'RS'
required <- c(
  "ade4", "adegenet", "ape", "data.table", "digest", "gdsfmt",
  "ggplot2", "ggrepel", "poppr", "rmarkdown", "scales",
  "SNPRelate", "vegan", "viridisLite", "yaml"
)
optional <- c(
  "clue", "covr", "hierfstat", "LEA", "knitr", "patchwork",
  "pkgdown", "svglite", "testthat"
)
check <- function(packages, type) {
  for (package in packages) {
    available <- requireNamespace(package, quietly = TRUE)
    version <- if (available) as.character(utils::packageVersion(package)) else "missing"
    cat(sprintf("[%-8s] %-18s %s\n", if (available) "OK" else toupper(type), package, version))
    if (!available && type == "required") quit(status = 1L)
  }
}
check(required, "required")
check(optional, "optional")
cat(sprintf("R version: %s\n", R.version.string))
RS

if [[ "$fail" -ne 0 ]]; then
  echo 'Environment verification failed.' >&2
  exit 1
fi

echo
echo 'Environment verification completed.'
