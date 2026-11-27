"""Replay rational hypergeometric ties and the growing-design expansion."""
from fractions import Fraction
from math import prod

import mpmath as mp

from check import hyper_mass


def fixed_population():
    sample = (0, 1, 1, 1, 1)
    heights = {(k, m): prod(hyper_mass(5, k, m, x) for x in sample)
               for k in range(6) for m in range(6)}
    top = max(heights.values())
    assert top == Fraction(256, 3125)
    assert {pair for pair, height in heights.items() if height == top} == {(1, 4), (4, 1)}


def tail():
    mp.mp.dps = 80
    max_scaled_error = mp.mpf(0)
    for sample in ((0, 2), (0, 3), (0, 0, 1, 3)):
        mean = mp.mpf(sum(sample))/len(sample)
        f2 = mp.mpf(sum(x*(x-1) for x in sample))/len(sample)
        f3 = mp.mpf(sum(x*(x-1)*(x-2) for x in sample))/len(sample)
        variance = f2+mean-mean**2
        for k in (100, 1000, 10000):
            for m in (k, 3*k, 13*k):
                u, v = 1/mp.mpf(k), 1/mp.mpf(m)
                h = u+v
                for desired in (mean/2, mean, 2*mean):
                    n = int(mp.nint(k*m/desired))
                    rate = mp.mpf(k*m)/n
                    zero = sum(mp.log1p(-mp.mpf(m)/(n-j)) for j in range(k))
                    correction = sum(
                        sum(mp.log1p(-j*u)+mp.log1p(-j*v)
                            -mp.log1p(-rate*h+(j+1)*rate*u*v) for j in range(x))
                        for x in sample
                    )/len(sample)
                    exact = zero+rate+correction
                    first = -h*((rate-mean)**2+variance-mean)/2
                    second = (
                        rate**2*u*v/2-rate**3*((u*u+v*v)/3+u*v/2)
                        -(u*u+v*v)*(f3/6+f2/4)
                        -rate*u*v*(f2/2+mean)+mean*rate**2*h*h/2
                    )
                    error = abs(exact-first-second)/h**3
                    max_scaled_error = max(max_scaled_error, error)
    assert max_scaled_error < 100
    print("81 growing-design evaluations at 80 digits. Largest error / h^3:",
          mp.nstr(max_scaled_error, 10))


if __name__ == "__main__":
    fixed_population()
    tail()
