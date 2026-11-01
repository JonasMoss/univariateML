"""Numerical spot checks of the dossier; these do not certify global extrema."""
import numpy as np
from scipy.special import stdtr
from scipy.stats import t


def loglik(x, xi, omega, alpha, nu):
    z = (x - xi) / omega
    a = alpha * z * np.sqrt((nu + 1) / (nu + z * z))
    return np.sum(np.log(2) - np.log(omega) + t.logpdf(z, nu) + np.log(stdtr(nu + 1, a)))


x = np.array([0.0, 0.0, 1.0, 2.0, 3.0])
nu, alpha = 0.3, 2.0
scales = np.array([1e-5, 1e-6, 1e-7])
values = np.array([loglik(x, 0, s, alpha, nu) for s in scales])
slope = np.diff(values) / np.diff(np.log(scales))
target = nu * 3 - 2
assert np.max(np.abs(slope - target)) < 1e-4, (slope, target)
print('collapse log-log slopes:', slope, 'target:', target)

nu, c = 3.0, 1.0
expected = 2 * stdtr(nu + 1, c * np.sqrt((nu + 1) / nu))
alpha = 1e6
z = c / alpha
actual = 2 * stdtr(nu + 1, alpha * z * np.sqrt((nu + 1) / (nu + z*z)))
assert abs(actual - expected) < 1e-12
print('endpoint-layer skew factor:', actual, 'limit:', expected)
