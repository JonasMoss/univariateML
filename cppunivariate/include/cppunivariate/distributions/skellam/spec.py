"""Bessel density with analytic derivatives for the independent quality oracle.

The harness's scalar math backend does not supply besseli. Dispatch scalar
mpmath arguments to mpmath, and numpy/scalar-double arguments to scipy.
"""
import sympy as sp
import mpmath as mp
from scipy.special import iv


def _bessel(order, argument):
    if isinstance(order, mp.mpf) or isinstance(argument, mp.mpf):
        return mp.besseli(order, argument)
    return iv(order, argument)


class BesselI(sp.Function):
    nargs = 2
    _imp_ = staticmethod(_bessel)

    def fdiff(self, argindex=1):
        order, argument = self.args
        if argindex == 2:
            return (BesselI(order - 1, argument) + BesselI(order + 1, argument)) / 2
        raise sp.ArgumentIndexError(self, argindex)


def log_density(symbols, x):
    a, b = symbols['mu1'], symbols['mu2']
    return -a-b+x*sp.log(a/b)/2+sp.log(BesselI(sp.Abs(x), 2*sp.sqrt(a*b)))
