"""Exact sample inputs and high-precision point scouts for U-branch folds."""
from fractions import Fraction
from time import perf_counter

import mpmath as mp


def cases():
    t = Fraction(40**3 - 399**3, 401**3)
    eps = Fraction(1, 10**20)
    return {
        "two contacts": [-1, 1],
        "witness": [-1, 1] + [t] * 6,
        "balanced half tie": [-1, 0, 0, 1],
        "separated half tie": [0, 0, eps, 1],
        "half tie sweep endpoint": [0, 0, Fraction(1, 2), 1],
        "held-scale pair as free-scale U": [0, 10**20],
        "held-scale triple as free-scale U": [0, 0, 10**20],
        "envelope illustration": [-1, 0, 1],
    }


def point_inverse(t, l):
    if not t:
        return mp.mpf(".5")
    d = 1-t
    p = d/2
    for _ in range(100):
        q = 1-p
        step = (p**l - mp.expm1(l*mp.log1p(-p))-d)/(l*(p**(l-1)+q**(l-1)))
        new = p-step
        if abs(step) < 8*mp.eps*p:
            return new
        p = new
    raise ArithmeticError("point inverse")


def terms(t, l):
    p = point_inverse(t, l)
    q = 1-p
    a, b = p**(l-1), q**(l-1)
    d = a+b
    pl = -(p*a*mp.log(p)-q*b*mp.log(q))/(l*d)
    h = -(a*mp.log(p)+b*mp.log(q)+(l-1)*(a/p-b/q)*pl)/d
    return h, mp.log(2/d)


def fold(k, m, c, guesses):
    n = k+m+c
    def score(l, t):
        return -n/l + m*terms(t, l)[0] + c*mp.log(2)
    return mp.findroot(lambda l, t: (score(l,t), mp.diff(lambda z: score(z,t),l)), guesses)


if __name__ == '__main__':
    start = perf_counter()
    mp.mp.dps = 30
    for m,c in [(1,8),(1,16),(2,10),(2,20),(3,9),(4,6),(6,0),(6,18),(12,20)]:
        for guess in [('1.3','.93'), ('1.6','.98')]:
            try:
                l,t = fold(2,m,c,tuple(map(mp.mpf,guess)))
                if 1 < l < 2 and 0<t<1:
                    print(m,c,mp.nstr(l,24),mp.nstr(t,24))
                    break
            except (ValueError, ArithmeticError, TypeError):
                pass
    print(f"elapsed {perf_counter()-start:.3f}s")
