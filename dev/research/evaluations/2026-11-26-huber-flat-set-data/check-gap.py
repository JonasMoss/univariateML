"""Check the binary64 comparison against exact rational arithmetic."""
from fractions import Fraction
from itertools import product
import math
import random
import struct
import subprocess
import sys

rng = random.Random(364)

def binary64():
    while True:
        value, = struct.unpack("d", struct.pack("Q", rng.getrandbits(64)))
        if math.isfinite(value) and value:
            return value

u = math.ulp(1.)
tiny = math.ulp(0.)
large = sys.float_info.max
cases = [(a, b, c, s) for a, b in product(
            (-large, -1., -tiny, 0., tiny, 1., large), repeat=2)
         for c, s in product((tiny, 1-u, 1., 1+u, large), repeat=2) if a <= b]
for _ in range(20_000):
    a, b = sorted((binary64(), binary64()))
    c, s = abs(binary64()), abs(binary64())
    cases.append((a, b, c, s))
    # Manufacture close products by choosing the scale near the cutoff.
    gap = (Fraction(b)-Fraction(a))/2
    try:
        scale = float(gap/Fraction(c))
    except OverflowError:
        continue
    for s in (math.nextafter(scale, 0.), scale, math.nextafter(scale, math.inf)):
        if math.isfinite(s) and s > 0:
            cases.append((a, b, c, s))

data = ''.join(' '.join(v.hex() for v in row)+'\n' for row in cases)
actual = subprocess.check_output([sys.argv[1], 'gap'], input=data, text=True).splitlines()
assert len(actual) == len(cases)
for result, row in zip(actual, cases):
    a,b,c,s = map(Fraction, row)
    residual = b-a-2*c*s
    assert int(result) == (residual > 0) - (residual < 0), row
print(f'{len(cases)} comparisons match exact rationals (seed 364)')
