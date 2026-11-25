"""Exact dyadic neighbors of the two earlier U-profile events."""
from fractions import Fraction
import math
from pathlib import Path
import sys

import mpmath as mp

# Use the previously checked interval kernel, including its complete root cover.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent
                       / "2026-11-25-tlroot-math-data"))
from roots import describe, rat, tunable

EVENTS = (
    ("fold", 19, 16, "0.7425450262943313027831787956",
     "0.7425450262943313027831787957", ([-1, -1], [])),
    ("junction", 110, 35, "0.7500888321531553476973054941",
     "0.7500888321531553476973054942", ([-1, -1], [-1, 1])),
)


def neighboring_inputs(a, b):
    p = float(a)
    if Fraction(p) >= a:
        p = math.nextafter(p, -math.inf)
    q = math.nextafter(p, math.inf)
    assert Fraction(p) < a < b < Fraction(q)
    return p, q


def main():
    print(f"mpmath {mp.__version__}, interval digits {mp.iv.dps}", flush=True)
    for label, m, c, low, high, expected in EVENTS:
        pair = neighboring_inputs(Fraction(low), Fraction(high))
        for t, heights in zip(pair, expected):
            print(label, t.hex(), str(Fraction(t)), flush=True)
            profile = tunable(m, c, rat(Fraction(t)))
            roots, pending = profile.isolate(goal=mp.mpf("1e-25"))
            assert not pending
            assert describe(profile, roots, pending) == heights
    # Algebraic point witness at shape 3/2 from the earlier report.
    p, q = Fraction(40, 401)**2, Fraction(399, 401)**2
    assert p + q == 1
    d = Fraction(40 + 399, 401)
    # D = 439/401 on this witness.
    assert Fraction(2, 3)**8 * (2/d)**6 > 1
    print("All represented U-event and rational-anchor checks passed.")


if __name__ == "__main__":
    main()
