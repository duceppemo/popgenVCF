#!/usr/bin/env bash
set -euo pipefail

PREFIX="${1:-$HOME/.local/opt/fastStructure3}"
REPOSITORY="${POPGENVCF_FASTSTRUCTURE_REPOSITORY:-https://github.com/stevemussmann/fastStructure3.git}"

if [[ -e "$PREFIX" ]]; then
  echo "Destination already exists: $PREFIX" >&2
  echo "Remove it or pass a different destination." >&2
  exit 2
fi

git clone --depth 1 "$REPOSITORY" "$PREFIX"
cd "$PREFIX"

# The Python-3 port provides the original structure.py/chooseK.py interface.
# Build commands can vary between upstream revisions; use setup.py when
# present and otherwise compile Cython extensions in place.
if [[ -f setup.py ]]; then
  python setup.py build_ext --inplace
elif [[ -f vars/Makefile ]]; then
  make -C vars
else
  echo "Unable to identify the upstream fastStructure build layout." >&2
  exit 3
fi

cat <<MSG
fastStructure source installed under:
  $PREFIX

Configure popgenVCF with:
  structure_executable: $PREFIX/structure.py
  choosek_executable:   $PREFIX/chooseK.py
MSG
