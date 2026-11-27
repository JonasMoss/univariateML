"""Check the repaired skew-t crossing from the df-one density."""
from fractions import Fraction as Q

import sympy as sp


def check_density():
    z, alpha = sp.symbols("z alpha", positive=True)
    arg = alpha * z * sp.sqrt(2 / (1 + z**2))
    # T_2(h) = (1 + h/sqrt(2+h^2))/2, by differentiation and T_2(0)=1/2.
    h = sp.symbols("h", real=True)
    cdf = (1 + h / sp.sqrt(2 + h**2)) / 2
    assert cdf.subs(h, 0) == Q(1, 2)
    assert sp.simplify(sp.diff(cdf, h) - (2 + h**2)**sp.Rational(-3, 2)) == 0
    assert sp.simplify((arg / sp.sqrt(2 + arg**2))**2 -
                       alpha**2 / (1 + alpha**2 + z**-2)) == 0

    w, d, r = sp.symbols("w d r", positive=True)
    p = 100
    # The density factor pi*f(x)/S_x is w/(w^2+x^2).
    group = (w / (w*w+1))**p * (w / (w*w+d*d))**p * w / (w*w+r*r)
    scaled = w * (w*w / (w*w+1))**p * (d*d / (w*w+d*d))**p * r*r / (w*w+r*r)
    assert sp.cancel(group * d**(2*p) * r*r / scaled) == 1


def check_brackets():
    p, alpha, remote = 100, 2**20, 2**50000
    n = 2*p + 1
    low, high = alpha * 2**10, alpha * 2**150
    split = 8*alpha
    early = Q(alpha, 8)
    # The lower bounds use Bernoulli's inequality for the groups and
    # (1+y)^(-1/2) >= 1-y/2 for the two remote skew multipliers.
    assert 1 + alpha**2 + early**2 < Q(100, 81)*alpha**2
    early_factors = (
        1-p/early**2,
        1-p*(early/low)**2,
        1-Q(p, 4*alpha**2)*(1+(early/low)**2),
        1-(early/remote)**2,
        1-Q(1, 4*alpha**2)*(1+(early/remote)**2),
    )
    assert min(early_factors) > Q(3, 4)
    floor = early * Q(19, 10)**p * Q(3, 4)**5
    assert floor > 1
    # On w >= split the remaining scale times the D-group factor is <= D.
    assert split * Q(9, 8)**p < floor
    assert low * Q(9, 8)**p < floor
    late = Q(high, 16)
    assert 1-p/late**2 > Q(1, 2)
    assert 1-p*(late/high)**2 > Q(1, 2)
    assert late < remote
    # The two normalized skew factors are each > 1/2, and S_1 > 1.
    assert late / 32 > split * 2**p

    # Student/Cauchy and Student/t_2 density ratios peak at |z|=1
    # for df above 1 and 2 respectively. The Gaussian mixture bounds
    # t_df(1) by phi(1)<1/2. In particular t_2(1)>1/8.
    assert 3**3 < 8**2
    cutoff = 2**1000
    norm_bound = 4**n * high**(2*p)
    assert norm_bound * 2**n < cutoff**(2*p-1)
    assert norm_bound * 12*cutoff**2 < remote
    # log(2)>1/2 turns this exact tail bound into df-score exclusion.
    assert remote**2 > 2*cutoff**2 * 2**(20*n)


if __name__ == "__main__":
    check_density()
    check_brackets()
    print("Skew-t density, branch signs and free-df exclusion constants passed.")
