"""Diff symbolic ground truth against C++ evaluation; emit reports.

For each theta in the eval grid this compares:
- ``nll_at_theta`` — negative log-likelihood
- ``grad_at_theta`` — gradient (per parameter)
- ``hess_at_theta`` — Hessian (per parameter pair)

Tolerance is *relative*, scaled by the larger of the two values (with a
floor of 1.0 to avoid divide-by-near-zero amplifying noise around true
zero entries — e.g. score at the MLE is by construction ~0). The 1e-12
ceiling reflects the rounding floor between mpmath-50 truth and C++
double evaluation; if the C++ side carries an algebra mistake even by a
single derivative term, the relative diff jumps several orders of
magnitude above this floor.

Phase 2 emits:
- ``gradient_report.json``
- ``hessian_report.json``
- ``quality_report.md`` (minimal — the ladder claim is wired in Phase 6)

Phase 3+ extends this with simulation recovery, identifiability, dpqr.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Dict, List

NLL_TOL = 1e-12
GRAD_TOL = 1e-12
HESS_TOL = 1e-12


def _diff_pair(truth: float, observed: float) -> Dict[str, float]:
    abs_diff = abs(truth - observed)
    scale = max(abs(truth), abs(observed), 1.0)
    return {"abs": abs_diff, "rel": abs_diff / scale}


def _summarize_1d(truth: List[float], observed: List[float]) -> Dict:
    diffs = [_diff_pair(t, o) for t, o in zip(truth, observed)]
    return {
        "n": len(diffs),
        "max_abs": max((d["abs"] for d in diffs), default=0.0),
        "max_rel": max((d["rel"] for d in diffs), default=0.0),
    }


def _summarize_2d(truth: List[List[float]], observed: List[List[float]]) -> Dict:
    diffs = [
        _diff_pair(t, o)
        for t_row, o_row in zip(truth, observed)
        for t, o in zip(t_row, o_row)
    ]
    return {
        "n": len(diffs),
        "max_abs": max((d["abs"] for d in diffs), default=0.0),
        "max_rel": max((d["rel"] for d in diffs), default=0.0),
    }


def _summarize_3d(
    truth: List[List[List[float]]], observed: List[List[List[float]]]
) -> Dict:
    diffs = [
        _diff_pair(t, o)
        for t_th, o_th in zip(truth, observed)
        for t_row, o_row in zip(t_th, o_th)
        for t, o in zip(t_row, o_row)
    ]
    return {
        "n": len(diffs),
        "max_abs": max((d["abs"] for d in diffs), default=0.0),
        "max_rel": max((d["rel"] for d in diffs), default=0.0),
    }


def _write_quality_report(
    out_path: Path,
    spec_id: str,
    spec_hash: str,
    mpmath_dps: int,
    nll: Dict,
    grad: Dict,
    hess: Dict,
    nll_pass: bool,
    grad_pass: bool,
    hess_pass: bool,
) -> None:
    all_pass = nll_pass and grad_pass and hess_pass
    claimed = "validated" if all_pass else "experimental"

    def fmt(check_name: str, summary: Dict, tol: float, ok: bool) -> str:
        flag = "✓" if ok else "✗"
        return (
            f"- {flag} {check_name}: max_rel = {summary['max_rel']:.3e}"
            f" (tol {tol:.0e}, n={summary['n']})"
        )

    lines = [
        f"---",
        f"id: {spec_id}",
        f"spec_hash: {spec_hash}",
        f"mpmath_dps: {mpmath_dps}",
        f"claimed_level: {claimed}",
        f"---",
        f"",
        f"# {spec_id} — quality report",
        f"",
        f"## Symbolic vs C++ round-trip",
        f"",
        fmt("nll_at_theta", nll, NLL_TOL, nll_pass),
        fmt("grad_at_theta", grad, GRAD_TOL, grad_pass),
        fmt("hess_at_theta", hess, HESS_TOL, hess_pass),
        f"",
        f"## Provenance",
        f"",
        f"`spec_hash` is the SHA-256 (16 hex) of the canonical sympy "
        f"`srepr(log_density)`. It identifies the math, not the implementation:",
        f"a re-spelling of the same expression that simplifies to itself "
        f"(e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different",
        f"hash, which is the desired behaviour — it forces an explicit "
        f"reroll of the derived artifacts when the math changes.",
        f"",
    ]
    out_path.write_text("\n".join(lines) + "\n")


def main(distribution_id: str, repo_root: Path) -> int:
    derived = repo_root / "quality" / "derived" / distribution_id
    sym_path = derived / "symbolic_grid.json"
    cpp_path = derived / "cpp_eval.json"

    if not sym_path.exists():
        print(f"ERROR: {sym_path} missing. Run `just derive {distribution_id}`.")
        return 2
    if not cpp_path.exists():
        print(
            f"ERROR: {cpp_path} missing. Run `just cpp-test` to populate it "
            f"(the validation_tests ctest target writes it)."
        )
        return 2

    with sym_path.open() as fh:
        sym = json.load(fh)
    with cpp_path.open() as fh:
        cpp = json.load(fh)

    if sym["spec_hash"] != cpp["spec_hash"]:
        print(
            f"ERROR: spec_hash mismatch — "
            f"symbolic={sym['spec_hash']!r}, cpp={cpp['spec_hash']!r}. "
            f"Re-run `just derive {distribution_id}` then `just cpp-test`."
        )
        return 2

    nll = _summarize_1d(sym["nll_at_theta"], cpp["nll_at_theta"])
    grad = _summarize_2d(sym["grad_at_theta"], cpp["grad_at_theta"])
    hess = _summarize_3d(sym["hess_at_theta"], cpp["hess_at_theta"])

    nll_pass = nll["max_rel"] < NLL_TOL
    grad_pass = grad["max_rel"] < GRAD_TOL
    hess_pass = hess["max_rel"] < HESS_TOL

    grad_payload = {
        "id": distribution_id,
        "spec_hash": sym["spec_hash"],
        "tolerance_rel": GRAD_TOL,
        "summary": grad,
        "passed": grad_pass,
    }
    hess_payload = {
        "id": distribution_id,
        "spec_hash": sym["spec_hash"],
        "tolerance_rel": HESS_TOL,
        "summary": hess,
        "passed": hess_pass,
    }
    (derived / "gradient_report.json").write_text(
        json.dumps(grad_payload, indent=2) + "\n"
    )
    (derived / "hessian_report.json").write_text(
        json.dumps(hess_payload, indent=2) + "\n"
    )

    _write_quality_report(
        derived / "quality_report.md",
        distribution_id,
        sym["spec_hash"],
        sym["mpmath_dps"],
        nll,
        grad,
        hess,
        nll_pass,
        grad_pass,
        hess_pass,
    )

    print(f"[{distribution_id}] nll  max_rel = {nll['max_rel']:.3e}  "
          f"({'OK' if nll_pass else 'FAIL'})")
    print(f"[{distribution_id}] grad max_rel = {grad['max_rel']:.3e}  "
          f"({'OK' if grad_pass else 'FAIL'})")
    print(f"[{distribution_id}] hess max_rel = {hess['max_rel']:.3e}  "
          f"({'OK' if hess_pass else 'FAIL'})")

    return 0 if (nll_pass and grad_pass and hess_pass) else 1
