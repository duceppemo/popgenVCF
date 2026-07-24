#!/usr/bin/env bash
set -euo pipefail

mode="dry-run"
wiki_url="https://github.com/duceppemo/popgenVCF.wiki.git"

while (($#)); do
  case "$1" in
    --push)
      mode="push"
      ;;
    --wiki-url)
      shift
      [[ $# -gt 0 ]] || {
        echo "--wiki-url requires a value" >&2
        exit 2
      }
      wiki_url="$1"
      ;;
    --help|-h)
      cat <<'EOF'
Usage: scripts/publish-wiki.sh [--push] [--wiki-url URL]

Copies maintained Markdown pages from wiki/ into a temporary clone of the
GitHub Wiki. The default is a dry run. --push commits and publishes changes.
Unmanaged Wiki pages are preserved.
EOF
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
  shift
done

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source_root="$(cd "$script_dir/.." && pwd)"
wiki_source="$source_root/wiki"

[[ -d "$wiki_source" ]] || {
  echo "Wiki source directory is missing: $wiki_source" >&2
  exit 1
}

publish_work="$(mktemp -d /tmp/popgenvcf-wiki-publish.XXXXXX)"
cleanup() {
  rm -rf -- "$publish_work"
}
trap cleanup EXIT

git clone "$wiki_url" "$publish_work/wiki"

for page in "$wiki_source"/*.md; do
  [[ "$(basename "$page")" == "README.md" ]] && continue
  install -m 0644 "$page" "$publish_work/wiki/$(basename "$page")"
done

install -m 0644 \
  "$source_root/man/figures/popgenVCF-logo.svg" \
  "$publish_work/wiki/popgenVCF-logo.svg"

git -C "$publish_work/wiki" add --intent-to-add .
git -C "$publish_work/wiki" diff --check

if git -C "$publish_work/wiki" diff --quiet; then
  echo "Wiki is already synchronized."
  exit 0
fi

git -C "$publish_work/wiki" diff --stat

if [[ "$mode" == "dry-run" ]]; then
  echo "Dry run only. Re-run with --push to publish these pages."
  exit 0
fi

author_name="$(git -C "$source_root" config user.name || true)"
author_email="$(git -C "$source_root" config user.email || true)"
[[ -n "$author_name" ]] || author_name="popgenVCF documentation"
[[ -n "$author_email" ]] || author_email="duceppemo@users.noreply.github.com"

git -C "$publish_work/wiki" config user.name "$author_name"
git -C "$publish_work/wiki" config user.email "$author_email"
git -C "$publish_work/wiki" add .
git -C "$publish_work/wiki" commit -m "docs: publish structured popgenVCF wiki"
git -C "$publish_work/wiki" push origin HEAD

echo "Published Wiki pages from $wiki_source"
