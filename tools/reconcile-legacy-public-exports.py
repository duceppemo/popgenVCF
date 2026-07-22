#!/usr/bin/env python3
"""Restore roxygen ownership for legacy functions in the stable public API."""

from __future__ import annotations

import argparse
import re
from pathlib import Path

BLOCKS: dict[tuple[str, str], tuple[str, ...]] = {
    ("R/cli.R", "cli_main"): (
        "#' Run the popgenVCF command-line interface",
        "#'",
        "#' @param args Character vector of command-line arguments.",
        "#' @return The pipeline result, or `NULL` invisibly for informational commands.",
        "#' @export",
    ),
    ("R/config.R", "default_config"): (
        "#' Create the default popgenVCF configuration",
        "#'",
        "#' @return A nested configuration list using the supported schema.",
        "#' @export",
    ),
    ("R/config.R", "read_config"): (
        "#' Read and merge a popgenVCF configuration",
        "#'",
        "#' @param path YAML configuration file.",
        "#' @return The user configuration merged with current defaults.",
        "#' @export",
    ),
    ("R/report.R", "render_report"): (
        "#' Render a population-genomics report",
        "#'",
        "#' @param results_rds Serialized analysis results path.",
        "#' @param output_dir Report output directory.",
        "#' @param title Report title.",
        "#' @param author Report author.",
        "#' @return The rendered report path, invisibly.",
        "#' @export",
    ),
    ("R/admixture.R", "run_admixture_cv"): (
        "#' Run ADMIXTURE cross-validation across K values",
        "#'",
        "#' @param executable ADMIXTURE executable name or path.",
        "#' @param plink_prefix Prefix of the PLINK BED dataset.",
        "#' @param k_values Integer ancestry-cluster values to evaluate.",
        "#' @param threads Number of ADMIXTURE worker threads.",
        "#' @param cv_folds Number of cross-validation folds.",
        "#' @param output_dir Directory for ADMIXTURE logs and outputs.",
        "#' @param seed Deterministic ADMIXTURE seed.",
        "#' @return A data table of K values and cross-validation errors.",
        "#' @export",
    ),
    ("R/pipeline.R", "run_pipeline"): (
        "#' Run the complete popgenVCF analysis pipeline",
        "#'",
        "#' @param config Configuration list or YAML configuration path.",
        "#' @param registry Analysis module registry.",
        "#' @param selected Optional module identifiers to execute.",
        "#' @return The completed `PopgenVCFAnalysis` object.",
        "#' @export",
    ),
}


def reconcile(path: Path, symbol: str, block: tuple[str, ...], write: bool) -> bool:
    lines = path.read_text(encoding="utf-8").splitlines()
    pattern = re.compile(rf"^\s*{re.escape(symbol)}\s*<-\s*function\b")
    matches = [index for index, line in enumerate(lines) if pattern.search(line)]
    if len(matches) != 1:
        raise RuntimeError(f"{path}: expected one definition of {symbol}, found {len(matches)}")

    index = matches[0]
    cursor = index - 1
    while cursor >= 0 and not lines[cursor].strip():
        cursor -= 1
    block_end = cursor
    existing: list[str] = []
    while cursor >= 0 and lines[cursor].lstrip().startswith("#'"):
        existing.append(lines[cursor].strip())
        cursor -= 1

    if "#' @export" in existing:
        if block_end + 1 < index:
            if write:
                del lines[block_end + 1:index]
                path.write_text("\n".join(lines) + "\n", encoding="utf-8")
            return True
        return False

    if existing:
        raise RuntimeError(f"{path}: {symbol} has an unexpected existing roxygen block")

    lines[index:index] = list(block)
    if write:
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    root = Path(args.root).resolve()

    changed: list[str] = []
    for (relative, symbol), block in BLOCKS.items():
        path = root / relative
        if not path.is_file():
            raise RuntimeError(f"missing source file: {relative}")
        if reconcile(path, symbol, block, args.write):
            changed.append(f"{relative}:{symbol}")

    if changed and not args.write:
        print("Legacy public exports require roxygen ownership:")
        print("\n".join(f"- {item}" for item in changed))
        return 1

    if args.write:
        print(f"Reconciled {len(changed)} legacy public export(s).")
    else:
        print(f"Legacy roxygen ownership is complete for {len(BLOCKS)} public exports.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
