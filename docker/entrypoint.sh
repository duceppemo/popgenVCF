#!/usr/bin/env bash
set -euo pipefail

if [[ $# -eq 0 ]]; then
  set -- --help
fi

case "$1" in
  bash|sh|R|Rscript)
    exec "$@"
    ;;
  popgenVCF)
    shift
    ;;
esac

exec Rscript /opt/popgenVCF/popgenVCF.R "$@"
