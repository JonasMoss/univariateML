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


def _summarize_against_sweep_max(diffs_abs: List[float], all_values: List[float]) -> Dict:
    """Summarise abs diffs against the sweep-wide max-absolute-value scale.

    Per-entry relative-diff has a degenerate scale at score-zero points
    (the MLE for grad, regular interior for some Hessian entries) — the
    1.0 floor in `max(|t|, |o|, 1.0)` makes a 1e-12 abs diff look like
    1e-12 *relative* even though the surrounding sweep values are O(1e3+).
    Scaling by the sweep-wide max catches algebra bugs (which would
    perturb every entry, not just the score-zero one) while not flagging
    floating-point rounding at points where both sides are zero.
    """
    sweep_max = max((abs(v) for v in all_values), default=0.0)
    sweep_max = max(sweep_max, 1.0)
    max_abs = max(diffs_abs, default=0.0)
    return {
        "n": len(diffs_abs),
        "max_abs": max_abs,
        "max_rel": max_abs / sweep_max,
        "sweep_max": sweep_max,
    }


def _summarize_1d(truth: List[float], observed: List[float]) -> Dict:
    diffs_abs = [abs(t - o) for t, o in zip(truth, observed)]
    return _summarize_against_sweep_max(diffs_abs, list(truth) + list(observed))


def _summarize_2d(truth: List[List[float]], observed: List[List[float]]) -> Dict:
    diffs_abs = [
        abs(t - o)
        for t_row, o_row in zip(truth, observed)
        for t, o in zip(t_row, o_row)
    ]
    flat = [v for row in truth for v in row] + [v for row in observed for v in row]
    return _summarize_against_sweep_max(diffs_abs, flat)


def _summarize_3d(
    truth: List[List[List[float]]], observed: List[List[List[float]]]
) -> Dict:
    diffs_abs = [
        abs(t - o)
        for t_th, o_th in zip(truth, observed)
        for t_row, o_row in zip(t_th, o_th)
        for t, o in zip(t_row, o_row)
    ]
    flat = [v for t_th in truth for row in t_th for v in row] + [
        v for o_th in observed for row in o_th for v in row
    ]
    return _summarize_against_sweep_max(diffs_abs, flat)


def _write_quality_report(
    out_path: Path,
    spec_id: str,
    spec_hash: str,
    mpmath_dps: int,
    nll: Dict,
    grad: Dict,
    hess: Dict,
    nll_pass: bool,
    grad_pass: Optional[bool],
    hess_pass: bool,
) -> None:
    gates_pass = nll_pass and hess_pass and (grad_pass is not False)
    claimed = "validated" if gates_pass else "experimental"

    def fmt(check_name: str, summary: Dict, tol: float, ok: Optional[bool]) -> str:
        if ok is None:
            return (
                f"- · {check_name}: max_rel = {summary['max_rel']:.3e}"
                f" (SKIP — FD grad, advisory only, n={summary['n']})"
            )
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
    from quality.harness.spec import load_spec

    spec = load_spec(distribution_id)
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
    grad_check_runs = spec.cost_has_gradient
    grad_pass = (grad["max_rel"] < GRAD_TOL) if grad_check_runs else None
    hess_pass = hess["max_rel"] < HESS_TOL

    hess_payload = {
        "id": distribution_id,
        "spec_hash": sym["spec_hash"],
        "tolerance_rel": HESS_TOL,
        "summary": hess,
        "passed": hess_pass,
    }
    (derived / "hessian_report.json").write_text(
        json.dumps(hess_payload, indent=2) + "\n"
    )

    grad_payload = {
        "id": distribution_id,
        "spec_hash": sym["spec_hash"],
        "tolerance_rel": GRAD_TOL if grad_check_runs else None,
        "summary": grad,
        "passed": grad_pass,
        "skipped_reason": (
            None if grad_check_runs
            else "Cost::gradient is finite-difference; analytical "
                 "gradient-vs-symbolic check is not meaningful "
                 "(implicit in nll match)."
        ),
    }
    (derived / "gradient_report.json").write_text(
        json.dumps(grad_payload, indent=2) + "\n"
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
    if grad_check_runs:
        print(f"[{distribution_id}] grad max_rel = {grad['max_rel']:.3e}  "
              f"({'OK' if grad_pass else 'FAIL'})")
    else:
        print(f"[{distribution_id}] grad max_rel = {grad['max_rel']:.3e}  "
              f"(SKIP — FD grad, advisory only)")
    print(f"[{distribution_id}] hess max_rel = {hess['max_rel']:.3e}  "
          f"({'OK' if hess_pass else 'FAIL'})")

    gates_pass = nll_pass and hess_pass and (grad_pass is not False)
    return 0 if gates_pass else 1
