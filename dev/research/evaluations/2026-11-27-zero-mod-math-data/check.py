"""Exact CMB comparisons and high-precision replays of rare-event limits."""
from fractions import Fraction as F
from math import comb, factorial
import mpmath as mp


def cmb_examples():
    q = F(183, 500)
    p = 1 - q
    empirical_zero = F(1, 8)

    def conditional(size):
        return comb(size, 2) * p**2 * q**(size - 2) / (1 - q**size)

    assert q**3 < empirical_zero < q**2
    assert conditional(2) > conditional(3)
    assert 2 * q < 1  # Upper bound on all successive ratios starting at size 3.
    saturated = empirical_zero * (1 - empirical_zero)**7
    interior = saturated * conditional(3)**7
    boundary = q**2 * p**14
    assert interior > boundary
    print("CMB interior/boundary likelihood ratio:", float(interior / boundary))

    # Coefficients of 2 q^2 + 2 q - 1 in the basis (1, sqrt(3)).
    a, b = F(-1, 2), F(1, 2)
    assert (2 * (a*a + 3*b*b) + 2*a - 1, 4*a*b + 2*b) == (0, 0)
    assert 1 < 3 < 4  # 0 < (sqrt(3) - 1)/2 < 1/2.


def bnbinom_off_diagonal_example():
    tail = F(3, 10**15 + 3)
    mass_pad = F(1, 10**7)
    assert mass_pad > 2 * tail and tail < (mass_pad - 2 * tail)**2
    ratio_pad = F(1, 10**5)
    centers = (F(6, 5), F(2), F(20, 7))
    for i, c in enumerate(centers, 1):
        g = F(3, (i + 2) * (i + 3))
        assert ((i + 1) + c) * mass_pad / (g - mass_pad) < ratio_pad

    dc = 2 * ratio_pad
    ddc = 4 * ratio_pad
    first = centers[1] - centers[0]
    second = centers[2] - 2 * centers[1] + centers[0]
    dlo = 2 * (1 - first - dc) / (second + ddc) - 3
    dhi = 2 * (1 - first + dc) / (second - ddc) - 3
    assert F("3.99") < dlo <= dhi < F("4.01")

    a1lo = (dlo + 1) * (centers[0] - ratio_pad) - 1
    a1hi = (dhi + 1) * (centers[0] + ratio_pad) - 1
    a2lo = (dlo + 2) * (centers[1] - ratio_pad) - 4
    a2hi = (dhi + 2) * (centers[1] + ratio_pad) - 4
    assert F("2.96") < a2lo - a1hi <= a2hi - a1lo < F("3.04")
    assert F("1.95") < 2*a1lo - a2hi <= 2*a1hi - a2lo < F("2.05")
    assert F("2.96")**2 - 4 * F("2.05") > 0


def dgamma(shape, rate, k):
    return mp.gammainc(shape, k * rate, (k + 1) * rate) / mp.gammainc(shape, rate)


def dweibull(t, beta, k):
    return (mp.exp(-t * (mp.power(k, beta) - 1))
            * -mp.expm1(-t * (mp.power(k + 1, beta) - mp.power(k, beta))))


def pig(z, rho, k):
    d = rho * z
    shape = d * (z - d / 2)
    polynomial = sum(mp.mpf(factorial(k - 1 + j)) /
                     (factorial(j) * factorial(k - 1 - j) * (2 * z)**j)
                     for j in range(k))
    return (shape / z)**k * polynomial / (factorial(k) * mp.expm1(d))


def bnbinom(r, a, b, k):
    log_zero = (mp.loggamma(a+r) + mp.loggamma(a+b)
                - mp.loggamma(a+r+b) - mp.loggamma(a))
    return (mp.rf(r, k) * mp.rf(b, k) / (mp.rf(a+r+b, k) * factorial(k))
            / mp.expm1(-log_zero))


def bbinom(size, a, b, k):
    log_zero = sum(mp.log1p(-a / (a + b + j)) for j in range(size))
    odds = comb(size, k) * mp.rf(a, k) * mp.rf(b, size-k) / mp.rf(b, size)
    return odds / mp.expm1(-log_zero)


def replay():
    for digits in (70, 100):
        with mp.workdps(digits):
            largest = [mp.mpf(0)] * 5
            for exponent in (4, 6, 8, 10):
                eps = mp.mpf(10)**(-exponent)
                for k in range(1, 9):
                    for x in map(mp.mpf, ("0.3", "0.7", "1.0")):
                        logarithmic = -x**k / (k * mp.log1p(-x)) if x < 1 else None
                        pairs = [
                            (dgamma(eps, x, k), dgamma(0, x, k)),
                            (dweibull(x / eps, eps, k), k**(-x)-(k+1)**(-x)),
                            (pig(eps, x, k),
                             (-1)**(k+1) * mp.binomial(mp.mpf("0.5"), k)
                             * (x*(2-x))**k / x),
                            (bnbinom(eps, 2, 3, k),
                             mp.rf(3, k) / (k * mp.rf(5, k))
                             / (mp.digamma(5)-mp.digamma(2))),
                            (bbinom(8, eps, x, k),
                             comb(8, k) * factorial(k-1) * mp.rf(x, 8-k)
                             / (mp.rf(x, 8)*sum(1/(x+j) for j in range(8)))),
                        ]
                        for j, (value, limit) in enumerate(pairs):
                            error = abs(value - limit)
                            largest[j] = max(largest[j], error / eps)
                            assert error < 10 * eps
                        if logarithmic is not None:
                            value = bnbinom(eps, (1-x)/(x*eps), 1/eps, k)
                            assert abs(value - logarithmic) < 10 * eps
            mean = mp.findroot(lambda mu: mp.expm1(mu) - 2*mu, (1, 2))
            d_vals = [mean * j / 100 for j in range(1, 100)]
            assert all(d / ((2-d/mean) * mp.expm1(d)) < mp.mpf("0.5")
                       for d in d_vals)
            print(digits, "digits, held-mean PIG tie at", mp.nstr(mean, 25))
            print(digits, "digits, max absolute error / epsilon:",
                  ", ".join(mp.nstr(error, 8) for error in largest))


if __name__ == "__main__":
    cmb_examples()
    bnbinom_off_diagonal_example()
    replay()
