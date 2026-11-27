"""Independent rational check of the two held-location skew-t witnesses."""
from fractions import Fraction as Q

import sympy as sp


def main():
    # From 2*t_1(x/w)*T_2(h)/w, with h=A*x*sqrt(2)/sqrt(w²+x²),
    # the factors left after removing S_x are pi*f_x/S_x=w/(w²+x²).
    w, d, r = sp.symbols("w d r", positive=True)
    p = 100
    n = 2 * p + 1
    dossier_product = (w / (w*w + 1))**p * (w / (w*w + d*d))**p * w / (w*w + r*r)
    normalized_core = w * (w*w / (w*w + 1))**p * (d*d / (w*w + d*d))**p * r*r / (w*w + r*r)
    assert sp.powsimp(normalized_core / (d**(2*p) * r*r * dossier_product)) == 1

    alpha = 2**20
    lower, upper, outlier = 2**10 * alpha, 2**150 * alpha, 2**50000
    e = Q(alpha, 512) * Q(19, 10)**p
    scale = alpha // 8
    assert Q(1) + alpha*alpha + scale*scale < Q(100, 81) * alpha*alpha
    for scale, gap, bound in ((scale, lower, Q(3, 4)), (upper // 16, upper, Q(1, 2))):
        factors = (
            1 - Q(p, 1 + scale*scale),
            1 - Q(p*scale*scale, gap*gap),
            1 - Q(p, 4*alpha*alpha) * (1 + Q(scale*scale, gap*gap)),
            1 - Q(scale*scale, outlier*outlier),
            1 - Q(1, 4*alpha*alpha) * (1 + Q(scale*scale, outlier*outlier)),
        )
        assert all(factor > bound for factor in factors)
    assert Q(3, 4)**5 > Q(1, 64)
    assert e > 1
    assert max(8*alpha, lower) * Q(9, 8)**p < e
    # At the separator and above, S_1 <= 1+A/w <= 9/8.
    # The upper-end high-scale witness beats even the uniform low-branch bound.
    assert Q(upper, 16*32) > 8*alpha*2**p

    # Use pi < 4. The global Cauchy-type envelope and the cubic envelope
    # bound the normalized product outside the df monotonicity rectangle.
    max_scale = 2**1000
    normalizer_without_outlier = Q(4**n * upper**(2*p), 2**(p+1))
    assert normalizer_without_outlier * 2**n < max_scale**(2*p-1)
    assert normalizer_without_outlier * 12 * max_scale**2 < outlier
    assert outlier**2 > 2 * max_scale**2 * 2**(20*n)
    print("Skew-t dossier normalization and rational crossing bounds verified.")


if __name__ == "__main__":
    main()
