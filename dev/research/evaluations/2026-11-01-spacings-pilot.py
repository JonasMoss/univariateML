"""Small independent MPS checks; not a production fitter or calibration study.

Run with OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 <this file>.
Prints JSON to stdout; deterministic data, no random seed required.
"""
import json
import platform
import subprocess

import numpy as np
import scipy
from scipy.optimize import minimize


def log_product(u):
    d = np.diff(np.r_[0.0, u, 1.0])
    return float(np.log(d).sum()) if np.all(d > 0) else -np.inf


x = np.array([0.1, 0.2, 0.4, 0.7, 1.0])
n = len(x)
span = float(x[-1] - x[0])
margin = span / (n - 1)
exact = np.array([x[0] - margin, x[-1] + margin])


def objective(z):
    a, b = x[0] - np.exp(z[0]), x[-1] + np.exp(z[1])
    return -log_product((x - a) / (b - a))


fits = []
for start in [[-3.0, -3.0], [0.0, 0.0], [-2.0, 1.0]]:
    fit = minimize(objective, start, method="Nelder-Mead",
                   options={"xatol": 1e-11, "fatol": 1e-12, "maxiter": 2000})
    endpoints = np.array([x[0] - np.exp(fit.x[0]), x[-1] + np.exp(fit.x[1])])
    error = float(np.max(np.abs(endpoints - exact)))
    assert fit.success and error < 1e-7
    fits.append({"start": start, "success": bool(fit.success),
                 "endpoints": endpoints.tolist(), "max_abs_error": error})

# GPD xi=-1.5, threshold=0, upper endpoint e=1+epsilon.
# Moving e down to max(x)=1 creates an infinite-density spike.
path = []
xi = -1.5
for epsilon in [1e-2, 1e-4, 1e-8, 1e-12]:
    endpoint = 1 + epsilon
    sigma = -xi * endpoint
    log_t = np.log1p(-x / endpoint)
    log_sf = -log_t / xi
    u = -np.expm1(log_sf)
    # Use survival differences: avoids subtraction of two CDFs near 1.
    sf = np.exp(log_sf)
    d = np.r_[u[0], sf[:-1] - sf[1:], sf[-1]]
    log_mps = float(np.log(d).sum())
    ll = float((-np.log(sigma) - (1 + 1 / xi) * log_t).sum())
    path.append({"epsilon": epsilon, "log_likelihood": ll, "log_product_spacings": log_mps})
assert all(b["log_likelihood"] > a["log_likelihood"] for a, b in zip(path, path[1:]))
assert all(b["log_product_spacings"] < a["log_product_spacings"] for a, b in zip(path, path[1:]))
assert np.isneginf(log_product([0.2, 0.2, 0.8]))

print(json.dumps({
    "source_revision": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
    "python": platform.python_version(), "numpy": np.__version__, "scipy": scipy.__version__,
    "data": x.tolist(), "uniform_exact_endpoints": exact.tolist(), "uniform_fits": fits,
    "gpd_shape": xi, "gpd_endpoint_path": path,
    "ties": "ordinary MPS is zero (log objective minus infinity)",
    "scope": "Analytic identity and numerical path illustration; no global GPD fit, production CDF test, simulation or inference calibration."
}, indent=2))
