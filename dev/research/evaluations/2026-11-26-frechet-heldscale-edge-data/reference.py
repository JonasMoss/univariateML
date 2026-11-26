"""Replay the two held-scale fits and verify the winning shape regions."""
from fractions import Fraction
from pathlib import Path
import sys

import mpmath as mp

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "2026-11-25-frechet-math-data"))
from ties import Family, interval, iv


def reference(r):
    # Use the exact binary64 inputs used by C++ and R.
    gaps = [mp.mpf(0)] * 3 + [mp.mpf(0.05)] * 4 + [mp.mpf(r)]

    def likelihood(shape, gap):
        return sum(mp.log(shape) - (shape + 1) * mp.log(x + gap)
                   - (x + gap)**(-shape) for x in gaps)

    def score(shape, gap):
        return mp.diff(lambda a: likelihood(a, gap), shape), mp.diff(
            lambda e: likelihood(shape, e), gap)

    edge = mp.findroot(lambda e: score(1, e)[1], mp.mpf(".5"))
    shape, gap = mp.findroot(score, (mp.mpf("1.55"), mp.mpf(".7")))
    for label, a, e in (("edge", mp.mpf(1), edge), ("interior", shape, gap)):
        print(r, label, *(mp.nstr(v, 50) for v in (a, -e, likelihood(a, e))))
    assert (likelihood(1, edge) > likelihood(shape, gap)) == (r == 40)


def verify_regions(middle):
    # Repeat the earlier interval exclusions and check the shape derivative
    # near the edge at r=40. Strict value bounds handle the rest of the left region.
    family = Family(False)
    family.xmiddle = middle
    a, split, b, low, high = map(Fraction, ("1", "1.2", "10", "39", "40"))
    assert iv.ln(interval(11)) - 11 * iv.ln(interval(39)) / 8 < family.lower(a, low, high)
    assert 1 / interval(11) - iv.ln(interval(39)) / 8 < 0
    print(middle, "excluded slabs visited",
          family.exclude(a, split, low, Fraction("1.575")),
          family.exclude(split, b, high, a))
    near = Fraction("1.1")
    print(middle, "remaining left slabs visited", family.exclude(near, split, high, a))
    pending = [(a, near, 0)]
    visited = 0
    while pending:
        lo, hi, depth = pending.pop()
        visited += 1
        result = family.upper(lo, hi, high, high)
        if result is not None and result[1] < 0:
            continue
        assert depth < 24, (lo, hi)
        mid = (lo + hi) / 2
        pending.extend([(lo, mid, depth + 1), (mid, hi, depth + 1)])
    print(middle, "shape-derivative slabs visited", visited)


if __name__ == "__main__":
    mp.mp.dps = 80
    iv.dps = 50
    for r in (39, 40):
        reference(r)
    for middle in (Fraction(1, 20), Fraction(0.05)):
        verify_regions(middle)
