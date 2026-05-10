"""Symbolic specification of a univariate distribution.

A `Spec` declares the math of one distribution in machine-readable form.
The harness reads it to derive gradients, Hessians, and Fisher information
symbolically (sympy), evaluates them at high precision (mpmath), and
compares against the C++ implementation's `Cost` evaluation at the same
points.

Pilot rule (v1): a Spec's `params` are written in **the same order and
the same parameter space** that `Cost::operator()(theta)` expects. For
distributions whose Cost is defined in natural parameters (lomax, exp,
norm), `log_density` is in natural params. For distributions whose Cost
is defined in a transformed (phi) space (std uses `(mu, log sigma,
log(nu - 2))`), the spec writes `log_density` in those same phi params.
This keeps symbolic derivatives directly comparable with `Cost::gradient`
and avoids a Jacobian transform layer in v1. A future extension may
decouple natural and phi spaces — see roadmap.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional, Tuple

import sympy as sp


@dataclass(frozen=True)
class Param:
    name: str
    start: float
    domain: str = "real"
    # Set when the MLE sits AT a feasibility boundary (pareto's b = min(x),
    # power's alpha = max(x), unif's a = min(x), unif's b = max(x), …).
    # "upper" → MLE is the upper feasibility limit, smooth interior is
    # below MLE. "lower" → MLE is the lower limit, smooth interior is
    # above. The harness's Sobol sweep strays into the smooth interior
    # rather than centring on the boundary itself, where the gradient
    # formula isn't defined.
    boundary: Optional[str] = None  # one of {"upper", "lower", None}


@dataclass(frozen=True)
class Spec:
    id: str
    params: Tuple[Param, ...]
    x_symbol: sp.Symbol
    log_density: sp.Expr
    support: str = "real"
    cdf: Optional[sp.Expr] = None
    quantile: Optional[sp.Expr] = None
    fisher_info_per_obs: Optional[sp.Matrix] = None
    references: Dict[str, str] = field(default_factory=dict)
    notes: str = ""
    # True when Cost::gradient is implemented analytically (most distros).
    # Set to False for distros where BFGS calls finite_diff_cost<Cost> —
    # std, snorm, sstd, sged, ged, paralogis. The harness then skips the
    # 1e-12 gradient-vs-symbolic check (the FD gradient is already
    # implicitly verified by checking nll matches symbolic, since the FD
    # stencil reads from the same operator()).
    cost_has_gradient: bool = True
    # Map golden's natural-space `estimates` dict to a Cost-space theta
    # vector matching `params` order. None ⇒ identity (golden names match
    # param names; used by exp/lomax/etc.). Set explicitly for distros
    # whose Cost takes a phi reparam (std, etc.).
    golden_to_cost: Optional[Callable[[Dict[str, float]], List[float]]] = None

    def __post_init__(self) -> None:
        free = {s.name for s in self.log_density.free_symbols}
        declared = {self.x_symbol.name, *(p.name for p in self.params)}
        extra = free - declared
        missing = {p.name for p in self.params} - free
        if extra:
            raise ValueError(
                f"Spec[{self.id}]: log_density references undeclared symbols: "
                f"{sorted(extra)}"
            )
        if missing:
            raise ValueError(
                f"Spec[{self.id}]: declared params absent from log_density: "
                f"{sorted(missing)}"
            )

    @property
    def n_params(self) -> int:
        return len(self.params)

    def param_symbols(self) -> Tuple[sp.Symbol, ...]:
        by_name = {s.name: s for s in self.log_density.free_symbols}
        return tuple(by_name[p.name] for p in self.params)

    def spec_hash(self) -> str:
        canonical = sp.srepr(self.log_density)
        return hashlib.sha256(canonical.encode()).hexdigest()[:16]


def load_spec(distribution_id: str) -> Spec:
    """Import quality.math.<id> and return its module-level SPEC."""
    import importlib

    mod = importlib.import_module(f"quality.math.{distribution_id}")
    spec = getattr(mod, "SPEC", None)
    if not isinstance(spec, Spec):
        raise ValueError(
            f"quality.math.{distribution_id} must export a module-level "
            f"`SPEC` of type Spec; got {type(spec).__name__}"
        )
    if spec.id != distribution_id:
        raise ValueError(
            f"Spec.id ({spec.id!r}) must match module name ({distribution_id!r})"
        )
    return spec
