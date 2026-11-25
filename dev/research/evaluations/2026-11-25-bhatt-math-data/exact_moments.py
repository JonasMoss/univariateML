"""Exact rational checks of illustrative samples and proof constants."""
from fractions import Fraction as F
from math import lcm

import sympy as s


def even_moments(counts):
    n = sum(counts.values())
    return tuple(sum(w*F(z)**j for z, w in counts.items())/n for j in (2, 4, 6, 8))


def histogram(weights):
    n = lcm(*(2*w.denominator for r, w in weights.items() if r),
            *(w.denominator for r, w in weights.items() if not r))
    return {r: int(n*w) for r, w in weights.items()}


def exp_bounds(x):
    term = partial = F(1)
    for j in range(1, 65):
        term *= x/j
        partial += term
    return partial, partial + term*(x/65)/(1-x/66)


def main():
    t = s.Symbol('t')
    print('Hermite coefficients', s.series(s.exp(-t*t/6)*s.sinh(t)/t, t, 0, 10))
    for radii in ({0: 4, 1: 2}, {0: 8, 1: 27, 3: 1}):
        v, m4, m6, _ = even_moments(radii)
        assert m4 == 3*v*v
        print('fourth equality', radii, 'standardized sixth Hermite', m6/v**3-15)
    radii = (F(1,2), F(3,4), F(9,4), F(5,2))
    matrix = s.Matrix([[v**j for v in radii] for j in (0, 2, 4, 6)])
    weights = tuple(map(F, matrix.inv()*s.Matrix([1, 1, 3, 15])))
    assert min(weights) > 0
    for count, m8_sign in [
        ({0: 70, 1: 90, 2: 18, 3: 2}, 1),
        (histogram(dict(zip(radii, weights))), -1),
    ]:
        v, m4, m6, m8 = even_moments(count)
        assert (v,m4,m6) == (1,3,15)
        assert (m8-91)*m8_sign > 0
        print('fourth and sixth equality', count, 'eighth moment', m8,
              'coefficient of eta^8', -(m8-91)/37800)
    p = F(1, 2**20)
    assert F(1, 150000)/(16*p) > F(1, 8)
    assert (F(507**2, 2)+1)*p < F(1, 8)
    assert 1/p > 3*101**2
    m = 32
    v = (1+m*F(2,5)**2)/(m+1)
    m4 = (1+m*F(2,5)**4)/(m+1)
    assert m4 < 3*v*v
    assert F(132,7)*v < 4
    p = F(1,10000)
    assert -(1-p)*(F(7,24)-F(1,16)) + p/F(3,100)**2 < 0
    assert -(1-p)/600 + p*(F(332,10)-5-F(1,100)-F(1,200)) > F(1,1000)
    assert 2*(333*p)**2/(1-p) < F(23,10000)
    assert exp_bounds(F(273,50))[1] < F(59600,251)
    assert exp_bounds(F(1371,250))[0] > 240
    assert -F(273,50) + p*(332**2-33**2)/2 < 0
    assert -F(1371,250) + p*333**2/2 - 551*p - F(1,10**6) > F(1,200)
    print('crossing proof constant checks passed')


if __name__ == '__main__':
    main()
