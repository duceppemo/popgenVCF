#!/usr/bin/env python3
"""Reconcile superseded roxygen export owners deterministically.

Later compatibility files contain the active implementations for a small set of
public functions. Earlier definitions remain load-order fallbacks, but must not
also claim generated documentation or namespace ownership.
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

TARGETS: dict[str, tuple[str, ...]] = {
    "R/benchmark_datasets.R": ("list_benchmark_datasets",),
    "R/execution-checkpoint.R": ("resume_analysis_execution",),
    "R/execution-engine.R": (
        "new_execution_engine",
        "print.PopgenVCFExecutionEngine",
        "execute_analysis_plan",
    ),
    "R/manuscript_revision.R": ("compare_manuscript_revisions",),
    "R/module_registry.R": ("default_analysis_registry",),
    "R/phase10_public_api.R": ("write_public_api_record", "read_public_api_record"),
    "R/publication_companion.R": ("new_publication_bundle", "generate_publication_bundle"),
    "R/zzzzzzzz_manuscript_regeneration_plans.R": (
        "new_manuscript_regeneration_plan",
        "validate_manuscript_regeneration_plan",
    ),
}


def reconcile_file(path: Path, symbols: tuple[str, ...]) -> tuple[list[str], list[str]]:
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed: list[str] = []
    errors: list[str] = []

    for symbol in symbols:
        definition = re.compile(
            rf"^\s*{re.escape(symbol)}\s*<-\s*function\b"
        )
        matches = [index for index, line in enumerate(lines) if definition.search(line)]
        if len(matches) != 1:
            errors.append(f"{path}: expected one definition of {symbol}, found {len(matches)}")
            continue

        index = matches[0] - 1
        block: list[int] = []
        while index >= 0 and lines[index].lstrip().startswith("#'"):
            block.append(index)
            index -= 1
        block.reverse()

        export_lines = [i for i in block if lines[i].strip() == "#' @export"]
        nord_lines = [i for i in block if lines[i].strip() == "#' @noRd"]
        if nord_lines and not export_lines:
            continue
        if len(export_lines) != 1 or nord_lines:
            errors.append(
                f"{path}: {symbol} must have exactly one @export and no @noRd "
                f"(exports={len(export_lines)}, noRd={len(nord_lines)})"
            )
            continue

        newline = "\n" if lines[export_lines[0]].endswith("\n") else ""
        lines[export_lines[0]] = "#' @noRd" + newline
        changed.append(symbol)

    if not errors and changed:
        path.write_text("".join(lines), encoding="utf-8")
    return changed, errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".", help="Repository root")
    parser.add_argument("--write", action="store_true", help="Write reconciled files")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    all_changed: list[str] = []
    all_errors: list[str] = []

    for relative, symbols in TARGETS.items():
        path = root / relative
        if not path.is_file():
            all_errors.append(f"missing source file: {relative}")
            continue
        original = path.read_text(encoding="utf-8")
        changed, errors = reconcile_file(path, symbols)
        all_changed.extend(f"{relative}:{symbol}" for symbol in changed)
        all_errors.extend(errors)
        if not args.write and changed:
            path.write_text(original, encoding="utf-8")

    if all_errors:
        print("Roxygen owner reconciliation errors:")
        print("\n".join(f"- {error}" for error in all_errors))
        return 2

    expected = sum(len(symbols) for symbols in TARGETS.values())
    if args.write:
        print(f"Reconciled {len(all_changed)} superseded owner(s); expected {expected}.")
        if len(all_changed) not in (0, expected):
            print("Partial reconciliation is prohibited.")
            return 2
        return 0

    if all_changed:
        print("Superseded roxygen export owners require reconciliation:")
        print("\n".join(f"- {item}" for item in all_changed))
        return 1

    print(f"Roxygen ownership is reconciled across {expected} superseded definitions.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
