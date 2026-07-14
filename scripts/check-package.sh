#!/usr/bin/env bash
set -euo pipefail
pkg_dir="${1:-popgenVCF}"
version=$(awk -F': ' '$1 == "Version" {print $2}' "$pkg_dir/DESCRIPTION")
R CMD build "$pkg_dir"
R CMD check --as-cran "popgenVCF_${version}.tar.gz"
