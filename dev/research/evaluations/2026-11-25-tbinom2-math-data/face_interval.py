"""Certify the derivative inequality on the compact middle-rate strip."""
from fractions import Fraction
from math import factorial

import mpmath as mp

mp.iv.dps = 55
ORDER = 32
EXP_ORDER = 45


def iv(lower, upper=None):
    """Enclose rational interval endpoints."""
    upper = lower if upper is None else upper
    a = mp.iv.mpf(lower.numerator) / lower.denominator
    b = mp.iv.mpf(upper.numerator) / upper.denominator
    return mp.iv.mpf([a.a, b.b])


def moments(rate):
    exponential_terms = [mp.iv.mpf(1)]
    for index in range(1, EXP_ORDER + 1):
        exponential_terms.append(exponential_terms[-1] * rate / index)
    tail = Fraction(3**(EXP_ORDER+1), factorial(EXP_ORDER+1))
    tail /= 1 - Fraction(3, EXP_ORDER+2)
    partition = sum(exponential_terms[1:]) + iv(Fraction(0), tail)
    result = {}
    for power in range(1, ORDER + 3):
        integral = sum(term / (power + index + 1)
                       for index, term in enumerate(exponential_terms))
        integral += iv(Fraction(0), tail / (power + EXP_ORDER + 2))
        result[power] = rate**(power+1) * integral / partition
    return result


def positive_part(value):
    return mp.iv.mpf([max(0, value.a), max(0, value.b)])


def prove(strip_end, min_h):
    width = 500
    minimum = None
    boxes = 0
    divisions = 64 if min_h < 4 else 32
    inverse_sizes = [iv(Fraction(j, divisions*min_h), Fraction(j+1, divisions*min_h))
                     for j in range(divisions)]
    for i in range(6*width//5, strip_end):
        rate = iv(Fraction(i, width), Fraction(i+1, width))
        w = moments(rate)
        assert (w[2]/w[1]).b < 3
        extra3 = positive_part(w[2]-w[1]) / 3 if min_h >= 3 else 0
        extra4 = positive_part(w[2]-2*w[1]) / 12 if min_h >= 4 else 0
        coefficients = [
            (w[2]/2 + extra3*(2**q - 2) + extra4*(1-5*2**q+3*3**q)
             - w[q+1]/(q+1))
            for q in range(2, ORDER+1)
        ]
        for s in inverse_sizes:
            lower = mp.iv.mpf(0)
            for coefficient in reversed(coefficients):
                lower = s*lower + coefficient
            tail = (w[ORDER+2] * s**(ORDER-1)
                    / ((ORDER+2)*(1-s*rate)))
            lower -= tail
            assert lower.a > 0, (i, s, lower)
            minimum = lower.a if minimum is None else min(minimum, lower.a)
            boxes += 1
    print(f"H >= {min_h}: {boxes} boxes; minimum lower endpoint",
          mp.nstr(minimum, 17), flush=True)


def main():
    for rate, maximum in ((Fraction(7, 5), 3), (Fraction(13, 5), 4)):
        w = moments(iv(rate))
        assert (2+w[2]/w[1]).a > maximum
    prove(700, min_h=2)
    prove(1300, min_h=3)
    prove(1500, min_h=4)


if __name__ == "__main__":
    main()
