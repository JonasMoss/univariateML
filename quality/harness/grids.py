"""Generate the (theta, x) evaluation grid for one distribution.

The grid combines:
- A fixed reference dataset (taken from distributions/<id>/golden.json so the
  same data is used by every layer of the pipeline, and it does not need
  regeneration when the spec changes).
- A theta grid: the recovered MLE plus a Sobol-low-discrepancy sweep
  around it in Cost-space. Sweep half-widths are chosen per parameter
  domain so that grid points stay strictly inside the parameter support.

Output: quality/derived/<id>/eval_request.json. This file is checked in
so the C++ side and the Python side agree bit-for-bit on which points to
evaluate at.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Dict, List

import numpy as np
from scipy.stats import qmc

from quality.harness.spec import Spec

DEFAULT_N_THETAS = 16


def _sweep_halfwidth(param_domain: str, start: float) -> float:
    """Pick a sensible half-width for the per-parameter Sobol sweep.

    The sweep is multiplicative (geometric) for half-line positive params
    (e.g. (0, oo), (2, oo)) and additive for real-line and bounded
    intervals like (0, 1) — additive with clamping keeps the sweep
    inside the support without per-distribution tuning. Multiplicative
    factor 1.5 means we sweep [start/1.5, start*1.5]; additive 0.5
    means [start−0.5, start+0.5].
    """
    # Bounded (0, 1) must be checked *before* the half-line (0, …) prefix
    # match, otherwise theta close to 1 gets swept past the boundary.
    if "(0, 1)" in param_domain:
        return 0.3  # additive, clamped inside (0, 1)
    if "(0," in param_domain or "(2," in param_domain or "(0, oo)" in param_domain:
        return 1.5  # multiplicative
    return 0.5  # additive


def _shift_inside(spec: Spec, mle: List[float]) -> List[float]:
    """Nudge boundary params 1% into the smooth interior, leave others alone.

    Used as the first theta for distros with boundary MLEs (pareto/unif/etc.)
    where the *actual* MLE sits AT a feasibility limit — the gradient/
    Hessian formulas the C++ Cost implements aren't defined there.
    """
    out = list(mle)
    for j, p in enumerate(spec.params):
        if p.boundary == "upper":
            out[j] = mle[j] * 0.99
        elif p.boundary == "lower":
            out[j] = mle[j] * 1.01
    return out


def _theta_sweep(spec: Spec, mle: List[float], n_thetas: int, seed: int) -> List[List[float]]:
    """Sobol-low-discrepancy sweep in [0, 1)^n, mapped to per-param sweeps."""
    sobol = qmc.Sobol(d=spec.n_params, scramble=True, seed=seed)
    u = sobol.random(n_thetas)
    has_boundary = any(p.boundary for p in spec.params)
    first = _shift_inside(spec, mle) if has_boundary else list(mle)
    thetas: List[List[float]] = [first]  # MLE (or boundary-shifted MLE)
    for i in range(n_thetas):
        theta = []
        for j, p in enumerate(spec.params):
            hw = _sweep_halfwidth(p.domain, mle[j])
            uij = u[i, j]  # uniform in [0, 1)
            # Boundary parameters: MLE sits AT the feasibility limit;
            # the gradient/Hessian formulas the C++ Cost implements are
            # only defined in the smooth interior. Sweep stays inside.
            if p.boundary == "upper":
                # MLE is upper feasibility limit; smooth interior is below.
                # Log-uniform on [mle * 0.5, mle * 0.99].
                log_lo, log_hi = np.log(mle[j] * 0.5), np.log(mle[j] * 0.99)
                theta.append(float(np.exp(log_lo + uij * (log_hi - log_lo))))
            elif p.boundary == "lower":
                # MLE is lower feasibility limit; smooth interior is above.
                # Log-uniform on [mle * 1.01, mle * 2.0].
                log_lo, log_hi = np.log(mle[j] * 1.01), np.log(mle[j] * 2.0)
                theta.append(float(np.exp(log_lo + uij * (log_hi - log_lo))))
            # Order matters below: (0, 1) is a *prefix* of (0, …), so it has
            # to be tested first or bounded-interval params get the
            # multiplicative branch (and theta sweeps past the boundary).
            elif "(0, 1)" in p.domain:
                lo = max(1e-3, mle[j] - hw)
                hi = min(1 - 1e-3, mle[j] + hw)
                theta.append(float(lo + uij * (hi - lo)))
            elif "(0," in p.domain or "(2," in p.domain:
                # Multiplicative: x in [mle/hw, mle*hw]
                log_lo, log_hi = np.log(mle[j] / hw), np.log(mle[j] * hw)
                theta.append(float(np.exp(log_lo + uij * (log_hi - log_lo))))
            else:
                lo, hi = mle[j] - hw, mle[j] + hw
                theta.append(float(lo + uij * (hi - lo)))
        thetas.append(theta)
    return thetas


def build_eval_request(
    spec: Spec,
    repo_root: Path,
    n_thetas: int = DEFAULT_N_THETAS,
    seed: int = 0xC0FFEE,
) -> Dict:
    """Build the eval-request payload (data + theta grid + metadata).

    The data is taken from distributions/<id>/golden.json. The MLE used as the
    sweep centre is the `estimates` field of the same file.
    """
    golden_path = (repo_root / "cppunivariate" / "include" / "cppunivariate"
                   / "distributions" / spec.id / "golden.json")
    with golden_path.open() as fh:
        golden = json.load(fh)
    data = list(golden["data"])
    estimates = golden["estimates"]
    if spec.golden_to_cost is not None:
        mle = list(spec.golden_to_cost(estimates))
    else:
        mle = [float(estimates[p.name]) for p in spec.params]
    thetas = _theta_sweep(spec, mle, n_thetas=n_thetas, seed=seed)
    return {
        "id": spec.id,
        "spec_hash": spec.spec_hash(),
        "n_params": spec.n_params,
        "param_names": [p.name for p in spec.params],
        "n_data": len(data),
        "data": data,
        "mle": mle,
        "n_thetas": len(thetas),
        "thetas": thetas,
    }


def write_eval_request(spec: Spec, repo_root: Path, **kwargs) -> Path:
    payload = build_eval_request(spec, repo_root, **kwargs)
    out_dir = repo_root / "quality" / "derived" / spec.id
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "eval_request.json"
    with out_path.open("w") as fh:
        json.dump(payload, fh, indent=2)
        fh.write("\n")
    return out_path
