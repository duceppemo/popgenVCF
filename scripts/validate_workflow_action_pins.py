#!/usr/bin/env python3
"""Fail closed when GitHub Actions workflows use mutable external references."""

from __future__ import annotations

import json
import os
import re
import sys
from pathlib import Path

USES_RE = re.compile(r"^\s*uses:\s*([^\s#]+)(?:\s+#\s*(.+?))?\s*$")
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
VERSION_RE = re.compile(r"\bv[0-9]+(?:\.[0-9]+){0,2}\b")
WORKFLOW_SUFFIXES = {".yml", ".yaml"}


def repository_root() -> Path:
    configured = os.environ.get("GITHUB_WORKSPACE") or os.environ.get(
        "POPGENVCF_ACTION_PIN_ROOT"
    )
    if configured:
        return Path(configured).resolve(strict=True)
    return Path(__file__).resolve().parents[1]


def workflow_paths(root: Path) -> list[Path]:
    workflow_dir = root / ".github" / "workflows"
    if not workflow_dir.is_dir():
        raise RuntimeError(f"Missing workflow directory: {workflow_dir}")
    return sorted(
        path
        for path in workflow_dir.iterdir()
        if path.is_file() and path.suffix.lower() in WORKFLOW_SUFFIXES
    )


def inspect_workflows(root: Path) -> tuple[list[dict[str, object]], list[str]]:
    inventory: list[dict[str, object]] = []
    findings: list[str] = []

    for workflow in workflow_paths(root):
        relative = workflow.relative_to(root).as_posix()
        for line_number, line in enumerate(
            workflow.read_text(encoding="utf-8").splitlines(), start=1
        ):
            match = USES_RE.match(line)
            if not match:
                continue

            reference = match.group(1).strip("\"'")
            comment = (match.group(2) or "").strip()
            if reference.startswith("./") or reference.startswith("docker://"):
                inventory.append(
                    {
                        "workflow": relative,
                        "line": line_number,
                        "reference": reference,
                        "kind": "local",
                    }
                )
                continue

            if "@" not in reference:
                findings.append(
                    f"{relative}:{line_number}: external action lacks @ref: {reference}"
                )
                continue

            action, ref = reference.rsplit("@", 1)
            action_parts = action.split("/")
            if len(action_parts) < 2 or not all(action_parts[:2]):
                findings.append(
                    f"{relative}:{line_number}: malformed external action: {reference}"
                )
                continue

            if not SHA_RE.fullmatch(ref):
                findings.append(
                    f"{relative}:{line_number}: mutable action ref {ref!r}; "
                    "expected a 40-character lowercase commit SHA"
                )
            if not VERSION_RE.search(comment):
                findings.append(
                    f"{relative}:{line_number}: pinned action must retain a version "
                    "comment such as '# v4'"
                )

            inventory.append(
                {
                    "workflow": relative,
                    "line": line_number,
                    "action": action,
                    "commit": ref,
                    "version_comment": comment,
                    "kind": "external",
                }
            )

    inventory.sort(
        key=lambda item: (
            str(item["workflow"]),
            int(item["line"]),
            str(item.get("reference", item.get("action", ""))),
        )
    )
    return inventory, sorted(findings)


def main() -> int:
    root = repository_root()
    inventory, findings = inspect_workflows(root)
    external = [item for item in inventory if item["kind"] == "external"]
    if not external:
        findings.append("No external GitHub Action references were discovered")

    evidence = {
        "schema_version": "1.0",
        "record_type": "popgenvcf_github_action_pin_audit",
        "passed": not findings,
        "workflow_count": len(workflow_paths(root)),
        "external_reference_count": len(external),
        "local_reference_count": len(inventory) - len(external),
        "findings": findings,
        "inventory": inventory,
    }

    evidence_path = os.environ.get("POPGENVCF_ACTION_PIN_EVIDENCE", "")
    if evidence_path:
        destination = Path(evidence_path)
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(
            json.dumps(evidence, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )

    if findings:
        print("GitHub Action pin validation failed:", file=sys.stderr)
        for finding in findings:
            print(f"- {finding}", file=sys.stderr)
        return 1

    print(
        "GitHub Action pins are immutable: "
        f"{len(external)} external references across "
        f"{evidence['workflow_count']} workflows."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
