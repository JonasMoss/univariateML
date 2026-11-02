"""Numerical illustration of the truncated-CMP entropy upper bound.

Run: python3 dev/research/evaluations/2026-11-02-cmb-free-2-data/entropy_certificate.py
This is not an outward-rounded floating-point certificate.
"""
import numpy as np
from scipy.optimize import minimize
from scipy.special import gammaln, logsumexp

x = np.array([0, 1, 2, 2, 3, 4], dtype=int)
k = np.arange(9)
stats = np.column_stack((k, -gammaln(k + 1)))
target = stats[x].mean(axis=0)


def value(theta):
    logw = stats @ theta
    logz = logsumexp(logw)
    q = np.exp(logw - logz)
    return logz - target @ theta, q @ stats - target


fit = minimize(value, [0.0, 1.0], jac=True, method="BFGS", tol=1e-12)
logw = stats @ fit.x
q = np.exp(logw - logsumexp(logw))
matrix = np.vstack((np.ones(3), stats[:3].T))
residual = np.r_[1 - q.sum(), target - q @ stats]
q[:3] += np.linalg.solve(matrix, residual)
if not np.all(q >= 0):
    raise RuntimeError("moment correction produced a negative weight")
moment_error = np.max(np.abs(np.r_[q.sum() - 1, q @ stats - target]))
upper = len(x) * np.sum(np.where(q > 0, q * np.log(q), 0))
fitted = -len(x) * value(fit.x)[0]
assert moment_error < 1e-13
assert fitted <= upper + 1e-12
for a in np.linspace(-3, 3, 31):
    for nu in np.linspace(-1, 4, 26):
        assert -len(x) * value([a, nu])[0] <= upper + 1e-12
print(f"K=8 n={len(x)} minimum_q={q.min():.9g} moment_error={moment_error:.3g}")
print(f"fitted={fitted:.12g} entropy_upper={upper:.12g} gap={upper-fitted:.3g}")
