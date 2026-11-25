"""Exact score brackets and interval likelihood/tail comparisons."""
from fractions import Fraction
from math import comb, factorial
from pathlib import Path
import json

from mpmath import iv

iv.dps = 65
ROOT = Path(__file__).resolve().parents[4]


def interval(value):
    value = Fraction(value)
    return iv.mpf(value.numerator) / value.denominator


def bracket(weights, counts, bits=100):
    """Isolate the unique mean-matching positive root using integer signs."""
    n = sum(counts)
    shifted_sum = sum(j * c for j, c in enumerate(counts))
    coefficients = [(n*j - shifted_sum)*w for j, w in enumerate(weights)]
    def sign(numerator, denominator):
        value = coefficients[-1]
        power = denominator
        for c in reversed(coefficients[:-1]):
            value = value * numerator + c * power
            power *= denominator
        return value
    return bisect_score(sign, bits)


def bisect_score(sign, bits):
    hi = 1
    while sign(hi, 1) < 0:
        hi *= 2
    lo = 0
    denominator = 1
    for _ in range(bits):
        mid = lo + hi
        denominator *= 2
        value = sign(mid, denominator)
        if value == 0:
            return Fraction(mid, denominator), Fraction(mid, denominator)
        lo, hi = (mid, 2*hi) if value < 0 else (2*lo, mid)
    return Fraction(lo, denominator), Fraction(hi, denominator)


def enclosing(bounds):
    return iv.mpf([interval(bounds[0]).a, interval(bounds[1]).b])


def profile(weights, counts):
    left, right = bracket(weights, counts)
    z = enclosing((left, right))
    terms = [iv.mpf(w)*z**j for j, w in enumerate(weights)]
    total = sum(terms)
    loglik = sum(c * iv.log(terms[j] / total)
                 for j, c in enumerate(counts) if c)
    return loglik, z, [w/total for w in terms]


def finite(size, lower, upper, counts):
    if upper == float("inf"):
        return lower_truncated(size, lower, counts)
    return profile([comb(size, k) for k in range(lower, min(size, upper)+1)],
                   counts)


def lower_truncated(size, lower, counts):
    n = sum(counts)
    S = sum(k*c for k, c in enumerate(counts, lower))
    coefficients = [(n*k-S)*comb(size,k) for k in range(lower)]

    def sign(a, b):
        value = (n*size*a-S*(a+b)) * (a+b)**(size-1)
        return value - sum(c*a**k*b**(size-k) for k,c in enumerate(coefficients))

    odds = enclosing(bisect_score(sign, 100))
    normalizer = (1+odds)**size - sum(comb(size,k)*odds**k for k in range(lower))
    loglik = sum(c*iv.log(comb(size,k)*odds**k/normalizer)
                 for k,c in enumerate(counts, lower) if c)
    return loglik, odds, None


def poisson(lower, upper, counts):
    return profile([factorial(upper)//factorial(k) for k in range(lower, upper+1)],
                   counts)


def show(x):
    # Printing may round for display. Assertions use interval endpoints.
    return str(x)


def exclusion(lower, retained, counts, incumbent):
    """Jensen on a retained support, using the signed factorial remainder."""
    face, _, q = poisson(lower, retained, counts)
    gap = incumbent - face
    if not gap.a > 0:
        return None
    f2 = sum(w*k*(k-1) for k, w in enumerate(q, lower))
    f3 = sum(w*k*(k-1)*(k-2) for k, w in enumerate(q, lower))
    empirical = Fraction(sum(c*k*(k-1) for k, c in enumerate(counts, lower)),
                         sum(counts))
    a = (f2 - interval(empirical))/2
    m = f3/3 + f2/2
    end = 2*retained
    while True:
        tail = face + sum(counts)*(max(a.b, iv.mpf(0))/end + m.b/end**2)
        if tail.b < incumbent.a:
            return end, tail, face
        end *= 2


def enumerate_sizes(lower, upper, counts, witness, retained):
    maximum = lower + len(counts) - 1
    inc, _, _ = finite(witness, lower, upper, counts)
    end, tail, face = exclusion(lower, retained, counts, inc)
    values = {size: finite(size, lower, upper, counts)[0]
              for size in range(maximum, end)}
    best = max(values, key=lambda size: values[size].a)
    runner = max(val.b for size, val in values.items() if size != best)
    assert values[best].a > runner and values[best].a > tail.b
    _, odds, _ = finite(best, lower, upper, counts)
    print("histogram", lower, upper, counts)
    print("winner", best, "p", show(odds/(1+odds)), "loglik", show(values[best]))
    print("prefix", maximum, end-1, "runner gap lower", show((values[best]-runner).a))
    print("retained", retained, "Poisson", show(face),
          "tail upper", show(tail.b))


def finite_counterexample(lower, counts):
    upper = 4
    values = [finite(size, lower, upper, counts)[0] for size in range(4, 401)]
    face, _, q = poisson(lower, upper, counts)
    a = (sum(w*k*(k-1) for k, w in enumerate(q, lower))
         - interval(Fraction(sum(c*k*(k-1) for k, c in enumerate(counts, lower)),
                             sum(counts))))/2
    peaks = [size for size in range(5, 400)
             if values[size-4].a > max(values[size-5].b, values[size-3].b)]
    assert values[0].a > values[1].b and peaks and a.a > 0
    print("candidate", lower, upper, counts, "left drop", show(values[0]-values[1]),
          "interior peaks", peaks, "A", show(a), "finite 4 vs face", show(values[0]-face))
    for size in peaks:
        print("peak", size, "left", show(values[size-4]-values[size-5]),
              "right", show(values[size-4]-values[size-3]))



def signed_gap_is_local():
    for n, counts in ((64, [104,0,0,608,112]), (64, [10400,0,1,60798,11201])):
        nobs = sum(counts)
        total = sum(k*c for k,c in enumerate(counts))
        q = [Fraction(4**k, factorial(k)) for k in range(5)]
        q = [w/sum(q) for w in q]
        assert sum(k*w for k,w in enumerate(q)) == Fraction(total, nobs)
        g = sum(k*(k-1)*(Fraction(c,nobs)-w)
                for k,(c,w) in enumerate(zip(counts,q)))
        face = sum(c*iv.log(interval(w)) for c,w in zip(counts,q) if c)
        difference = finite(n, 0, 4, counts)[0] - face
        assert g >= 0 and difference.a > 0
        print("signed gap", counts, g, "finite size", n, "excess", show(difference))

def goldens():
    fixtures = ROOT / "univariateML-cpp/tests/modifiers/fixtures"
    for name, upper, witness, retained in (("tbinom", 10, 12, 10),
                                           ("ztbinom", float("inf"), 8, 14)):
        sample = json.loads((fixtures / name / "golden.json").read_text())["data"]
        lower = 1
        counts = [sample.count(k) for k in range(lower, max(sample)+1)]
        enumerate_sizes(lower, upper, counts, witness, retained)


if __name__ == "__main__":
    finite_counterexample(0, [2358, 0, 2927, 4714, 1])
    finite_counterexample(1, [45145, 4976, 49611, 268])
    signed_gap_is_local()
    goldens()
