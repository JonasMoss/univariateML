"""Simulation-recovery harness driver.

For a given distribution, drives `cppunivariate/tests/sim_driver` to fit
M replicates at each of several sample sizes N. Compares the recovered
estimates against the *true* parameters (the golden's `sampler_args`,
since that is what the data was actually generated under).

Reports per-N summaries:
- bias        = mean(θ̂) − θ_true
- N · Var(θ̂) = N times the sample covariance (should approach
                I(θ_true)⁻¹ as N → ∞ under regularity)
- empirical SD of θ̂

Phase 3b emits these as `simulation_recovery.json` plus a section in
`quality_report.md`. Phase 6 will tighten this with KS tests and the
explicit Fisher-info comparison (numerical when no closed form).
"""

from __future__ import annotations

import json
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Tuple

import numpy as np

from quality.harness.spec import Spec, load_spec

DEFAULT_NS = (200, 1000, 5000)
DEFAULT_REPS = 200
SIM_DRIVER_REL = "cppunivariate/build/tests/sim_driver"


def _golden_true_params(repo_root: Path, spec: Spec) -> Dict:
    """Recover the natural-space sampler args from quality/golden/<id>.json."""
    path = repo_root / "quality" / "golden" / f"{spec.id}.json"
    with path.open() as fh:
        g = json.load(fh)
    args = g.get("sampler_args")
    if not args:
        raise RuntimeError(
            f"golden/{spec.id}.json has no sampler_args — cannot run "
            f"simulation recovery without ground-truth parameters."
        )
    return args


def _natural_param_order(spec: Spec) -> List[str]:
    """Return the natural-space parameter names in the order *_mle returns them.

    For now we read this off `quality/golden/<id>.json::estimates`, which is
    canonical (codegen produces it). The Spec doesn't carry natural names
    yet (added in a future Spec extension when more distros need it).
    """
    return None  # caller dereferences from golden directly


def _build_request(
    spec_id: str,
    true_params_natural: List[float],
    Ns: List[int],
    reps: int,
    base_seed: int,
) -> Dict:
    # Seeds are passed to sim_driver as JSON numbers, which our hand-rolled
    # parser reads as doubles. Doubles exactly represent integers only up
    # to 2^53 (~9e15), so seeds need to stay below that — otherwise neighbours
    # collide after the round-trip and replicates duplicate. A sequential
    # counter offset by base_seed keeps every (N, rep) globally unique
    # without any mixing arithmetic. base_seed is the only knob.
    out_Ns: List[int] = []
    out_seeds: List[int] = []
    counter = 0
    for N in Ns:
        for r in range(reps):
            out_Ns.append(N)
            out_seeds.append(base_seed + counter)
            counter += 1
    return {
        "id": spec_id,
        "true_params": [float(v) for v in true_params_natural],
        "Ns": out_Ns,
        "seeds": out_seeds,
        "tol": 1e-4,
        "maxit": 300,
    }


def _run_sim_driver(
    repo_root: Path, request: Dict, work_dir: Path
) -> Dict:
    work_dir.mkdir(parents=True, exist_ok=True)
    in_path = work_dir / "_sim_request.json"
    out_path = work_dir / "_sim_response.json"
    in_path.write_text(json.dumps(request, indent=2) + "\n")
    driver = repo_root / SIM_DRIVER_REL
    if not driver.exists():
        raise RuntimeError(
            f"{driver} not built; run `just cpp-test` (or any cmake build "
            f"of cppunivariate/) first."
        )
    proc = subprocess.run(
        [str(driver), str(in_path), str(out_path)],
        capture_output=True,
        text=True,
    )
    if proc.returncode != 0:
        sys.stderr.write(proc.stderr)
        raise RuntimeError(f"sim_driver exited {proc.returncode}")
    with out_path.open() as fh:
        return json.load(fh)


def _summarise(
    Ns: List[int],
    estimates: List[List[float]],
    response_Ns: List[int],
    true_params: List[float],
    param_names: List[str],
) -> List[Dict]:
    """Group estimates by N; compute bias, sample SD, N·Var, n_oks."""
    n_params = len(true_params)
    arr = np.asarray(estimates, dtype=float)
    response_arr = np.asarray(response_Ns, dtype=int)
    summary = []
    for N in Ns:
        rows = arr[response_arr == N]
        if rows.size == 0:
            continue
        mean = rows.mean(axis=0)
        median = np.median(rows, axis=0)
        sd = rows.std(axis=0, ddof=1)
        bias = mean - np.asarray(true_params)
        median_bias = median - np.asarray(true_params)
        n_var_diag = float(N) * np.var(rows, axis=0, ddof=1)
        summary.append(
            {
                "N": int(N),
                "n_reps": int(rows.shape[0]),
                "params": param_names,
                "true": [float(v) for v in true_params],
                "mean": [float(v) for v in mean],
                "median": [float(v) for v in median],
                "bias": [float(v) for v in bias],
                "median_bias": [float(v) for v in median_bias],
                "sd": [float(v) for v in sd],
                "n_var_diag": [float(v) for v in n_var_diag],
            }
        )
    return summary


def main(distribution_id: str, repo_root: Path, *, N=None, reps: int = DEFAULT_REPS) -> int:
    spec = load_spec(distribution_id)
    derived = repo_root / "quality" / "derived" / distribution_id
    derived.mkdir(parents=True, exist_ok=True)

    true_args = _golden_true_params(repo_root, spec)
    # The order of estimates returned by sim_driver is the natural-space
    # *_mle Result order. We discover it from the golden's `estimates` keys.
    golden_estimates = json.load(
        (repo_root / "quality" / "golden" / f"{distribution_id}.json").open()
    )["estimates"]
    natural_names = list(golden_estimates.keys())
    # Match each natural name to a sampler_args key (which uses the same
    # canonical names as `estimates` for every distro shipped today).
    true_params = [float(true_args[name]) for name in natural_names]

    if N is None:
        Ns: List[int] = list(DEFAULT_NS)
    elif isinstance(N, int):
        Ns = [N]
    else:
        Ns = list(N)
    request = _build_request(
        spec_id=distribution_id,
        true_params_natural=true_params,
        Ns=Ns,
        reps=reps,
        base_seed=0xC0FFEE,
    )
    response = _run_sim_driver(repo_root, request, derived)

    # Filter out non-Ok fits before summarising — a status != 0 means BFGS
    # iteration limit etc., and including those skews bias/SD.
    statuses = response["statuses"]
    Ns_resp = response["Ns"]
    estimates = response["estimates"]
    ok_idx = [i for i, s in enumerate(statuses) if int(s) == 0]
    n_failed = len(estimates) - len(ok_idx)
    estimates_ok = [estimates[i] for i in ok_idx]
    Ns_ok = [Ns_resp[i] for i in ok_idx]

    summary = _summarise(
        Ns, estimates_ok, Ns_ok, true_params, natural_names
    )

    payload = {
        "id": distribution_id,
        "spec_hash": spec.spec_hash(),
        "true_params": dict(zip(natural_names, true_params)),
        "Ns": Ns,
        "reps": reps,
        "n_failed_fits": int(n_failed),
        "summary": summary,
    }
    (derived / "simulation_recovery.json").write_text(
        json.dumps(payload, indent=2) + "\n"
    )

    # Tidy up the request/response scratch files so derived/ stays small.
    for tmp_name in ("_sim_request.json", "_sim_response.json"):
        tmp = derived / tmp_name
        if tmp.exists():
            tmp.unlink()

    print(f"[{distribution_id}] simulation recovery (reps={reps}, "
          f"failures={n_failed}):")
    for row in summary:
        mean_str = " ".join(
            f"{name}={b:+.4f}" for name, b in zip(row["params"], row["bias"])
        )
        med_str = " ".join(
            f"{name}={b:+.4f}"
            for name, b in zip(row["params"], row["median_bias"])
        )
        print(f"  N={row['N']:>5}:  mean bias = {mean_str}")
        print(f"            median bias = {med_str}")
    return 0
