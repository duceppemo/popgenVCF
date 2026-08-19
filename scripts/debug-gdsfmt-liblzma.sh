#!/usr/bin/env bash
# Temporary diagnostic script for the apptainer.yml gdsfmt/liblzma build
# failure under investigation -- see NEWS.md/ROADMAP.md. Not part of the
# package; removed once the investigation concludes. Run from inside the
# repo checkout on the failing runner (e.g. via the tmate debug session):
#   bash scripts/debug-gdsfmt-liblzma.sh > /tmp/diag-output.txt 2>&1
# then cat /tmp/diag-output.txt.
set -u

echo "=== dmesg (OOM check) ==="
sudo dmesg 2>/dev/null | tail -80

echo "=== ulimit -a ==="
ulimit -a

echo "=== disk space ==="
df -h /

echo "=== locating leftover build dir ==="
BUILDDIR=$(find /tmp -maxdepth 2 -iname "R.INSTALL*" 2>/dev/null | head -1)
echo "BUILDDIR=$BUILDDIR"

source /opt/conda/etc/profile.d/conda.sh
conda activate popgenvcf

if [ -z "$BUILDDIR" ]; then
  echo "=== no leftover dir; re-extracting gdsfmt fresh ==="
  cd /tmp
  rm -rf gdsfmt gdsfmt_1.46.0.tar.gz
  curl -sL -o gdsfmt_1.46.0.tar.gz "https://bioconductor.org/packages/3.22/bioc/src/contrib/gdsfmt_1.46.0.tar.gz"
  tar -xzf gdsfmt_1.46.0.tar.gz
  cd gdsfmt/src/XZ
  tar -xzf xz-5.2.9.tar.gz
  cd xz-5.2.9
  ./configure CC="x86_64-conda-linux-gnu-cc -std=gnu23" \
    CPP="x86_64-conda-linux-gnu-cc -std=gnu23 -E" \
    CXX="x86_64-conda-linux-gnu-c++ -std=gnu++17" CXXCPP="" --build="" \
    --with-pic --disable-xz --disable-shared > /tmp/xz-configure.log 2>&1
  LZMADIR="$(pwd)/src/liblzma"
else
  LZMADIR=$(find "$BUILDDIR" -type d -path "*xz-5.2.9/src/liblzma" | head -1)
fi
echo "LZMADIR=$LZMADIR"

echo "=== re-running the build, fully verbose (no silent rules) ==="
cd "$LZMADIR" 2>/dev/null && make clean >/dev/null 2>&1
cd "$LZMADIR" 2>/dev/null && make V=1
echo "make exit code: $?"

echo "=== manually invoking the final link command directly ==="
cd "$LZMADIR" 2>/dev/null || exit 0
LINK_CMD=$(make V=1 -n liblzma.la 2>/dev/null | grep -E "^/bin/bash ./libtool|^\./libtool" | tail -1)
echo "LINK_CMD=$LINK_CMD"
if [ -n "$LINK_CMD" ]; then
  eval "$LINK_CMD"
  echo "direct link exit code: $?"
fi
