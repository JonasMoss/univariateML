"""Symbolic derivation: spec → gradient/Hessian/Fisher at the eval grid.

Reads quality/math/<id>.py and quality/derived/<id>/eval_request.json.
Symbolically differentiates `log_density` with respect to each declared
parameter, lambdifies the resulting expressions through `mpmath`
(working precision 50 decimal digits), evaluates at each (theta, x_i)
in the grid, and writes quality/derived/<id>/symbolic_grid.json.

The output schema mirrors what the C++ `validation_tests` doctest target
will write to `cpp_eval.json`, so `validate.py` can diff them entry-wise
in a uniform way.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Dict, List

import mpmath
import sympy as sp

from quality.harness.spec import Spec, load_spec

MPMATH_DPS = 50


def _symbolic_negloglik_derivs(spec: Spec):
    """Return (negloglik_per_obs, grad_per_obs, hess_per_obs).

    The estimator side optimises the *negative* log-likelihood, so we
    negate up front. All three are functions of (theta..., x).
    """
    syms = spec.param_symbols()
    nll = -spec.log_density
    grad = sp.Matrix([sp.diff(nll, s) for s in syms])
    hess = sp.Matrix(
        [[sp.diff(nll, si, sj) for sj in syms] for si in syms]
    )
    return nll, grad, hess


def _lambdify(expr, syms_in_order, x_symbol):
    """Lambdify an expression to mpmath with positional args (theta..., x)."""
    args = list(syms_in_order) + [x_symbol]
    return sp.lambdify(args, expr, modules="mpmath")


def _eval_at_theta(
    nll_fn, grad_fns, hess_fns, theta, data
):
    """Sum per-obs values over the dataset; return (nll, grad, hess)."""
    n_params = len(theta)
    mp_theta = [mpmath.mpf(v) for v in theta]
    nll_total = mpmath.mpf(0)
    grad_total = [mpmath.mpf(0) for _ in range(n_params)]
    hess_total = [
        [mpmath.mpf(0) for _ in range(n_params)] for _ in range(n_params)
    ]
    for x_val in data:
        x_mp = mpmath.mpf(x_val)
        nll_total += nll_fn(*mp_theta, x_mp)
        for i in range(n_params):
            grad_total[i] += grad_fns[i](*mp_theta, x_mp)
            for j in range(n_params):
                hess_total[i][j] += hess_fns[i][j](*mp_theta, x_mp)
    return nll_total, grad_total, hess_total


def derive_grid(spec: Spec, repo_root: Path) -> Path:
    """Read eval_request.json, evaluate symbolically, write symbolic_grid.json."""
    derived_dir = repo_root / "quality" / "derived" / spec.id
    eval_path = derived_dir / "eval_request.json"
    if not eval_path.exists():
        raise FileNotFoundError(
            f"Missing {eval_path}. Run grids.write_eval_request first."
        )
    with eval_path.open() as fh:
        req = json.load(fh)

    if req["spec_hash"] != spec.spec_hash():
        raise RuntimeError(
            f"eval_request.json was generated against spec_hash="
            f"{req['spec_hash']!r}; current spec hashes to "
            f"{spec.spec_hash()!r}. Regenerate the request."
        )

    nll_expr, grad_expr, hess_expr = _symbolic_negloglik_derivs(spec)
    syms = spec.param_symbols()
    x_sym = spec.x_symbol

    nll_fn = _lambdify(nll_expr, syms, x_sym)
    grad_fns = [_lambdify(grad_expr[i], syms, x_sym) for i in range(spec.n_params)]
    hess_fns = [
        [_lambdify(hess_expr[i, j], syms, x_sym) for j in range(spec.n_params)]
        for i in range(spec.n_params)
    ]

    mpmath.mp.dps = MPMATH_DPS

    data = req["data"]
    nll_at_theta: List[float] = []
    grad_at_theta: List[List[float]] = []
    hess_at_theta: List[List[List[float]]] = []
    for theta in req["thetas"]:
        nll, g, H = _eval_at_theta(nll_fn, grad_fns, hess_fns, theta, data)
        nll_at_theta.append(float(nll))
        grad_at_theta.append([float(gi) for gi in g])
        hess_at_theta.append([[float(H[i][j]) for j in range(len(theta))] for i in range(len(theta))])

    payload: Dict = {
        "id": spec.id,
        "spec_hash": spec.spec_hash(),
        "mpmath_dps": MPMATH_DPS,
        "n_thetas": req["n_thetas"],
        "n_params": req["n_params"],
        "param_names": req["param_names"],
        "thetas": req["thetas"],
        "nll_at_theta": nll_at_theta,
        "grad_at_theta": grad_at_theta,
        "hess_at_theta": hess_at_theta,
    }

    out_path = derived_dir / "symbolic_grid.json"
    with out_path.open("w") as fh:
        json.dump(payload, fh, indent=2)
        fh.write("\n")
    return out_path


def print_derivatives(spec: Spec) -> None:
    """Pretty-print the symbolic derivatives so the user can sanity-check.

    Called from the CLI as a side-effect of `derive` so the human running
    the harness sees what was differentiated, in human-readable sympy form.
    """
    nll, grad, hess = _symbolic_negloglik_derivs(spec)
    syms = spec.param_symbols()
    print(f"\n[{spec.id}] negative log-likelihood per obs:")
    print(f"  -log f = {sp.simplify(nll)}")
    print(f"\n[{spec.id}] gradient per obs (∂(-log f)/∂param):")
    for s, g in zip(syms, grad):
        print(f"  ∂/∂{s.name}: {sp.simplify(g)}")
    print(f"\n[{spec.id}] Hessian per obs:")
    for i, si in enumerate(syms):
        for j, sj in enumerate(syms):
            if j < i:
                continue
            print(f"  ∂²/∂{si.name}∂{sj.name}: {sp.simplify(hess[i, j])}")
    if spec.fisher_info_per_obs is not None:
        print(f"\n[{spec.id}] Fisher information per obs (declared):")
        print(f"  I(θ) = {spec.fisher_info_per_obs}")
    print()


def main(distribution_id: str, repo_root: Path) -> None:
    spec = load_spec(distribution_id)
    print_derivatives(spec)
    from quality.harness import grids

    eval_path = grids.write_eval_request(spec, repo_root)
    sym_path = derive_grid(spec, repo_root)
    print(f"Wrote {eval_path.relative_to(repo_root)}")
    print(f"Wrote {sym_path.relative_to(repo_root)}")
