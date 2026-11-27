"""Exact rational checks for the BNB diagonal cover and discrete examples."""

import json
from collections import Counter
from fractions import Fraction as Q
from functools import cache
from math import comb, factorial, prod
from pathlib import Path

UNIT = 1 << 96
TERMS = 32


def floor(value):
    return value.numerator // value.denominator


def ceil(value):
    return -floor(-value)


def mantissa_bounds(value):
    """Log bounds for 1 <= value <= 2, in integer units of 2^-96."""
    assert 1 <= value <= 2
    u = (value - 1) / (value + 1)
    u_low, u_high = floor(UNIT*u), ceil(UNIT*u)
    square_low = u_low*u_low // UNIT
    square_high = (u_high*u_high + UNIT-1) // UNIT
    power_low, power_high = u_low, u_high
    lower, upper = 0, 0
    for j in range(TERMS):
        denominator = 2*j + 1
        lower += 2*power_low // denominator
        upper += (2*power_high + denominator-1) // denominator
        power_low = power_low*square_low // UNIT
        power_high = (power_high*square_high + UNIT-1) // UNIT
    # For u <= 1/3, the entire analytic remainder is below one unit.
    assert Q(2*UNIT, 3**(2*TERMS+1)*(2*TERMS+1)) * Q(9, 8) < 1
    return lower, upper + 1


LOG2 = mantissa_bounds(Q(2))


def log_bounds(value):
    value = Q(value)
    assert value > 0
    shift = value.numerator.bit_length() - value.denominator.bit_length()
    mantissa = value / Q(2)**shift
    if mantissa < 1:
        shift -= 1
        mantissa *= 2
    low, high = mantissa_bounds(mantissa)
    a, b = sorted(shift * endpoint for endpoint in LOG2)
    return low + a, high + b


def loggamma_bounds(value):
    shifts = []
    while value < 16:
        shifts.append(value)
        value += 1
    # Stirling bounds for log Gamma with the common log(2*pi)/2 removed.
    low, high = log_bounds(value)
    main = -value + 1/(12*value)
    lower = floor((value-Q(1, 2))*low + UNIT*(main-1/(360*value**3)))
    upper = ceil((value-Q(1, 2))*high + UNIT*main)
    # The recurrence subtracts the log of the entire rising product.
    low, high = log_bounds(prod(shifts))
    return lower - high, upper - low


@cache
def logzero_bounds(s):
    # 3.14159 < pi < 3.14160 suffices (Machin's alternating series).
    a, b = loggamma_bounds(s + 2)
    c, d = loggamma_bounds(2*s + 2)
    low, high = log_bounds(Q(314159, 50000))[0], log_bounds(Q(3927, 625))[1]
    return 2*a - d + low//2, 2*b - c + (high+1)//2


def rising(value, count):
    return prod(value + j for j in range(count))


def check_bnbinom():
    root = Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json')
    counts = Counter(json.loads(root.read_text())['data'])
    assert all(x >= 0 and x == int(x) for x in counts)
    counts = {int(x): count for x, count in counts.items()}
    n = sum(counts.values())
    witness = sum(count * log_bounds(Q(60, (x+5)*(x+6)*(x+7)))[0]
                  for x, count in counts.items())
    # A deliberately loose separation test, with every endpoint rounded out.
    target = witness - UNIT//100
    lo, hi = Q(1, 10**6), Q(256)
    small = sum(count * log_bounds(rising(lo, x)/factorial(x))[1]
                for x, count in counts.items() if x)
    assert small < target

    @cache
    def ratios(s):
        terms = [Q(1)]
        for x in range(1, max(counts)+1):
            terms.append(terms[-1] * (s+x-1)**2 / ((2*s+x+1)*x))
        return terms

    assert n*(logzero_bounds(hi)[1] + log_bounds(sum(ratios(hi)))[1]) < target

    @cache
    def ratio_sum(s):
        return sum(count * log_bounds(ratios(s)[x])[1]
                   for x, count in counts.items() if x)

    # Relative spacing from octaves, then rational adaptive bisection.
    breaks = [lo]
    while breaks[-1] < hi:
        breaks.append(min(2*breaks[-1], hi))
    pending = list(zip(breaks, breaks[1:]))
    accepted = 0
    while pending:
        left, right = pending.pop()
        upper = n*logzero_bounds(left)[1] + ratio_sum(right)
        if upper < target:
            accepted += 1
            continue
        middle = (left+right)/2
        pending.extend([(left, middle), (middle, right)])
        assert len(pending) + accepted < 20000
    print(f'BNB diagonal excluded on {accepted} rational intervals plus both tails')


def hyper_mass(N, m, k, x):
    return Q(comb(m, x)*comb(N-m, k-x), comb(N, k))


def nhyper_mass(N, m, r, x):
    return Q(comb(x-1, r-1)*comb(N-x, m-r), comb(N, m))


def check_discrete():
    assert hyper_mass(5, 2, 2, 0)*hyper_mass(5, 2, 2, 2) == Q(3, 100)
    assert hyper_mass(5, 3, 2, 0)*hyper_mass(5, 3, 2, 2) == Q(3, 100)
    x = (2, 5)
    likelihood = lambda N: prod(nhyper_mass(N, 5, 2, t) for t in x)
    assert likelihood(8) < likelihood(9) == likelihood(10) > likelihood(11)
    assert (3*2-2)**4-2**2*(2*2-1)**3 > 0
    assert (3*20-2)**4-20**2*(2*20-1)**3 < 0
    def binomial_profile(size):
        prob = Q(28, 3*size)
        return prod(comb(size, x)*prob**x*(1-prob)**(size-x) for x in (7, 8, 13))

    assert binomial_profile(38) > binomial_profile(13)
    print('Exact adjacent products and triangular crossing signs passed')


def check_gamma():
    e_upper = sum(Q(1, factorial(j)) for j in range(13)) + Q(14, 13*factorial(13))
    witness_mean = (10000*Q(5, 3) + Q(41, 9))/10001
    log3_lower = Q(log_bounds(3)[0], UNIT)
    assert log3_lower**2 * 2*Q(314159, 100000) > e_upper*witness_mean**2
    assert Q(10004, 10001)*2*Q(22, 7) < 4*Q(27, 10)
    print('Exact generalized-gamma normal and face comparisons passed')


if __name__ == '__main__':
    check_discrete()
    check_gamma()
    check_bnbinom()
