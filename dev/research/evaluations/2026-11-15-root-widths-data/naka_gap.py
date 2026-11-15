#!/usr/bin/env python3
"""Prediction test for the untested range 5 < k < 100 of the Square-map shape bisection (naka, Omega held).
Synthetic samples (seeded, n = 7) with population shape k0; emulation with a correctly rounded digamma (an assumption: Boost's own
rounding is not reproduced here). Output naka_gap.txt: offset of the emulated shape from the exact root, and the derived widths."""
import sys, random, math
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
from paths import *
EPS_ = 2.0**-52
print('%6s %5s %12s %12s %12s %12s %12s' % ('k0', 'seed', 'k_hat', 'offset/eps', 'W_actual/eps', 'W_bound/eps', 'claim 2eps+u'))
for k0 in (3, 10, 30, 60, 95):
    for seed in range(4):
        rng = random.Random(7000+100*k0+seed)
        om = 2.0
        x = [math.sqrt(rng.gammavariate(k0, om/k0)) for _ in range(7)]
        o = naka_square_held(x, om)
        if o is None: continue
        sl = o['slope']
        Wa = o['half']+(o['E_G_actual']+o['E_s_actual']+o['E_trunc'])/sl+o['conv_actual']
        Wb = o['half']+(o['E_G_bound']+o['E_s_bound']+o['E_trunc'])/sl+o['conv_bound']
        print('%6g %5d %12.5g %12.1f %12.1f %12.1f %12.2f' % (k0, seed, o['shape'], float(o['offset'])/EPS_, float(Wa)/EPS_, float(Wb)/EPS_, 2+0.5))
