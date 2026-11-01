"""Check the skew-t positive-wedge identity against density integration.

Reproduce with /tmp/umvl-cpu/run python3 .../cdf_wedge.py.  This validates
finite examples only; it is not a bound on quadrature or floating-point error.
"""
import numpy as np
from scipy.integrate import quad
from scipy.special import stdtr
from scipy.stats import t


def wedge(x, a, nu):
    theta = np.arctan2(1., a)
    def integrand(r):
        s = np.sin(r)
        if s == 0:
            return 0.
        return np.exp(-0.5 * nu * np.log1p((x / s) ** 2 / nu))
    return quad(integrand, 0., theta, epsabs=1e-100, epsrel=1e-12)[0] / np.pi


def density_integral(x, a, nu):
    def f(z):
        return 2 * t.pdf(z, nu) * stdtr(nu+1, a*z*np.sqrt((nu+1)/(nu+z*z)))
    return quad(f, -np.inf, -x, epsabs=1e-100, epsrel=1e-11)[0]


def main():
    rng = np.random.default_rng(20261101)
    worst = (0., None)
    for _ in range(300):
        x = np.exp(rng.uniform(np.log(.01), np.log(8)))
        a = np.exp(rng.uniform(np.log(.05), np.log(12)))
        nu = np.exp(rng.uniform(np.log(.5), np.log(50)))
        w, d = wedge(x,a,nu), density_integral(x,a,nu)
        rel = abs(w-d)/d
        if rel > worst[0]:
            worst = (rel, (x,a,nu,w,d))
    print('cases=300 seed=20261101 worst relative error=', worst)
    assert worst[0] < 1e-8

if __name__ == '__main__':
    main()
