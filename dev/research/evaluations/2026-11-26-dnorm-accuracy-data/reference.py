"""Independent mpmath references for the compiled probe (all inputs exact)."""
from collections import Counter
import importlib.util
from pathlib import Path
import sys

import mpmath as mp

path = Path(__file__).resolve().parents[1] / "2026-11-25-dnorm-math-data/check.py"
spec = importlib.util.spec_from_file_location("math_check", path)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)
mp.mp.dps = 820  # Retain squared seam distances, including 2^-2148.
tiny = mp.mpf(2)**-1074


def adjacent_scale(delta, left_count=1):
    hist = {-1: left_count, 0: 1}
    # The inverse-scale score changes sign exactly once. On the balanced
    # route a Newton solve in scale can otherwise settle in the flat tail.
    lo, hi = mp.mpf(0), (100 if left_count == 1 else 1/delta)
    for _ in range(1400):
        mid = (lo+hi)/2
        if check.exact_eval(hist, -delta, 1/mid)[2] < 0:
            lo = mid
        else:
            hi = mid
        if hi-lo < mid*mp.mpf("1e-65"):
            return 2/(hi+lo)
    raise AssertionError("scale bracket")


def references():
    for exponent in (52, 60, 100, 1074):
        delta = mp.mpf(2)**-exponent
        scale = mp.sqrt((1+2*delta)/(2*mp.log1p(1/delta)))
        yield f"constant{exponent}", -delta, scale, (False, True)
        yield f"adjacent{exponent}", -delta, adjacent_scale(delta), (False, True)
    delta = mp.mpf(2)**-100
    yield "adjacent_unbalanced", -delta, adjacent_scale(delta, 2), (False, True)
    yield "constant_sd", mp.mpf("0.5"), tiny, (True, False)
    yield "symmetric_sd", mp.mpf(0), tiny, (True, False)
    # For these two scales the omitted outer tails are far below 10^-820.
    q = -mp.sqrt(2)*mp.erfinv(mp.mpf(1)/3)
    yield "unrepresentable_sd", q*tiny, tiny, (True, False)
    yield "adjacent_sd", q*delta, delta, (True, False)
    yield "both", mp.mpf("0.5"), tiny, (False, False)
    yield "both_edge", mp.mpf(0), tiny, (False, False)
    for name, offset in (("joint", -1), ("joint_shifted", 2**51)):
        with mp.workdps(120):
            m, s = check.root(Counter({0: 2, 2: 1}), {}, (1.2, .9))
        yield name, m+offset, s, (True, True)
    # Large-scale limiting scores have corrections of relative order 10^-616.
    yield "held_large", mp.mpf(1e308), mp.mpf(1e308), (False, True)
    yield "scale_large", mp.mpf(1)/6, mp.mpf(1e308), (True, False)


rows = {words[0]: words[1:] for line in Path(sys.argv[1]).read_text().splitlines()
        if (words := line.split())}
for name, mean, sd, free in references():
    a, b, ll, it, kind, certified = rows[name]
    errors = [abs(mp.mpf(float(x))-ref)/unit for x, ref, unit, vary in
              zip((a, b), (mean, sd), (max(abs(mean), sd), sd), free) if vary]
    print(name, mp.nstr(mean, 45), mp.nstr(sd, 45), kind, certified,
          "error", ",".join(mp.nstr(e, 12) for e in errors))
    if len(sys.argv) > 2:
        if name == "unrepresentable_sd":
            assert kind == "failure"
        else:
            assert kind == "estimate" and certified == "1"
            assert mp.isfinite(mp.mpf(ll))
            assert all(e <= mp.mpf("1e-10") for e in errors)
assert rows["joint_limit"][4:] == ["limit", "1"]
