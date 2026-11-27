"""Exact finite arithmetic checks for the discrete maximizing-set report."""
import importlib.util
from collections import Counter
from fractions import Fraction as Q
from itertools import combinations_with_replacement
from math import comb, prod
from pathlib import Path

import mpmath as mp
import sympy as sp


def choose(n, k):
    return comb(n, k) if 0 <= k <= n else 0


def hyper_mass(population, marked, draws, count):
    return Q(choose(marked, count) * choose(population-marked, draws-count),
             comb(population, draws))


def hyper_scan():
    witnesses = {}
    for population in range(1, 16):
        for length in range(1, 5):
            for sample in combinations_with_replacement(range(population+1), length):
                if sample[0] == sample[-1]:
                    continue
                histogram = Counter(sample)
                heights = {
                    (marked, draws): prod(
                        hyper_mass(population, marked, draws, x)**frequency
                        for x, frequency in histogram.items()
                    )
                    for marked in range(sample[-1], population+1)
                    for draws in range(marked, population+1)
                    if marked + draws <= population + sample[0]
                }
                if not heights:
                    continue
                height = max(heights.values())
                winners = [point for point, value in heights.items() if value == height]
                if len(winners) >= 2:
                    witnesses.setdefault("distinct canonical", (population, sample, winners, height))
                if winners[-1][0] - winners[0][0] > 1:
                    witnesses.setdefault("nonadjacent canonical marked", (population, sample, winners, height))
                if len(witnesses) == 2:
                    return witnesses
    return witnesses


def moments():
    a, m, f3, c = sp.symbols("a m f3 c", positive=True)
    f2 = m**2*(1+1/a)
    first_bb = -a*(a+1)/2*(1/c-1/(c+1))-f2/2+(a*m+f2/2+m)/(c+1)
    second_bb = (-a*(a+1)*(2*a+1)/12*(1/c**2-1/(c+1)**2)
                 -f3/6-f2/4
                 +(f3/3+(a+sp.Rational(3, 2))*f2+(a+1)**2*m)/(2*(c+1)**2))
    # c=b/N+a/N for BB, and c is the beta mixing mean for BNB.
    info_bb = m/(c*(c+1))
    result_bb = sp.factor((second_bb+sp.diff(first_bb, c)**2/(2*info_bb)).subs(c, a/m))
    first_bnb = a*(a-1)/2*(1/(1-c)-1)+f2/(2*c)-a*m-f2/2
    second_bnb = (
        -a*(a-1)*(2*a-1)/12*(1/(1-c)**2-1)
        -(f3/6+f2/4)/c**2
        +(a*a*m+a*f2+f3/3+f2/2)/2
    )
    info_bnb = a/(1-c)**2+m/c**2
    result_bnb = sp.factor((second_bnb+sp.diff(first_bnb, c)**2/(2*info_bnb)).subs(c, m/(a+m)))
    print("Held alpha BB, profiled second coefficient:", result_bb)
    print("Held beta or size BNB, profiled second coefficient:", result_bnb)


    coefficients = {}
    for length in range(2, 6):
        for sample in combinations_with_replacement(range(9), length):
            mean = Q(sum(sample), length)
            variance = Q(sum((x-mean)**2 for x in sample), length)
            if variance <= mean:
                continue
            hold = mean**2/(variance-mean)
            third = Q(sum(x*(x-1)*(x-2) for x in sample), length)
            value = result_bb.subs({a: hold, m: mean, f3: third})
            coefficients.setdefault(int(sp.sign(value)), (sample, hold, value))
    print("Equidispersion witnesses (sample, hold, BB quadratic):", coefficients)
    assert set(coefficients) == {-1, 0, 1}

    u, v = sp.symbols("u v")
    h = u+v
    hyper_zero = m*m*u*v/2-m**3*((u*u+v*v)/3+u*v/2)
    hyper_ratios = -(u*u+v*v)*(f3/6+m*m/4)-m*u*v*(m*m/2+m)+m**3*h*h/2
    expected = (u*u+v*v)*((m**3-f3)/6-m*m/4)-m*m*u*v/2
    assert sp.expand(hyper_zero+hyper_ratios-expected) == 0

    mp.mp.dps = 80
    max_errors = [mp.mpf(0), mp.mpf(0)]
    for sample, hold, coefficient in coefficients.values():
        a0 = mp.mpf(hold.numerator)/hold.denominator
        mean = mp.mpf(sum(sample))/len(sample)
        for size in (10000, 100000, 1000000):
            def rising(x, n):
                return mp.fprod(x+i for i in range(n))

            def bb(c):
                beta = c*size-a0
                log_p0 = (mp.loggamma(c*size)+mp.loggamma((c+1)*size-a0)
                          -mp.loggamma(beta)-mp.loggamma((c+1)*size))
                return log_p0+sum(
                    mp.log(comb(size,x)*rising(a0,x)/rising(beta+size-x,x))
                    for x in sample
                )/len(sample)

            def bnb(q):
                ra = size*q
                alpha = size-ra
                log_p0 = (mp.loggamma(alpha+a0)-mp.loggamma(alpha)
                          +mp.loggamma(size)-mp.loggamma(size+a0))
                return log_p0+sum(
                    mp.log(rising(ra,x)*rising(a0,x)/(rising(size+a0,x)*mp.factorial(x)))
                    for x in sample
                )/len(sample)

            q0 = mean/(mean+a0)
            face = a0*mp.log1p(-q0)+sum(
                mp.log(rising(a0,x)/mp.factorial(x))+x*mp.log(q0)
                for x in sample
            )/len(sample)
            for i, (fn, start, target) in enumerate(((bb, a0/mean, coefficient),
                     (bnb, q0, coefficient*(a0+mean)**2/mean**2))):
                root = mp.findroot(lambda arg: mp.diff(fn, arg), (start, start*1.0001))
                error = abs(size**2*(fn(root)-face)-mp.mpf(str(sp.N(target, 78))))
                max_errors[i] = max(max_errors[i], error)
                assert error < mp.mpf("0.01")
    print("80-digit quadratic residual maxima, BB and BNB:",
          *(mp.nstr(error, 8) for error in max_errors))


def main():
    data = Path(__file__).resolve().parent.parent / "2026-11-25-binom-ties-math-data"
    spec = importlib.util.spec_from_file_location("size_pairs", data / "size_pairs.py")
    binomial = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(binomial)
    candidates = [(size, candidate) for size in range(1, 501)
                  for candidate in binomial.possible_means(size)]
    print("Binomial integer-filter survivors, sizes 1 through 500:", candidates)
    assert not candidates
    print("Hypergeometric exact canonical witnesses:", hyper_scan())
    assert hyper_mass(3, 2, 2, 1) == hyper_mass(4, 2, 2, 1) == Q(2, 3)
    sample = (7, 8, 13)
    def binomial_height(size):
        p = Q(sum(sample), len(sample)*size)
        return prod(comb(size, x)*p**x*(1-p)**(size-x) for x in sample)
    assert binomial_height(38) > binomial_height(13)
    print("CMB endpoint comparison at nu=1: L38 > L13 (rational)")
    moments()


if __name__ == "__main__":
    main()
