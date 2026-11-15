#!/usr/bin/env python3
"""A priori relative shape error from the score G(k) = log k - psi(k) alone (s exact): E_G / (k |G'(k)|), by k.
For k < 100 the digamma error is ASSUMED 4 u |psi| (boost::math::digamma; not measured here); for k >= 100 the series is evaluated
with the Higham-style bound (bound mode). Output: amplification.txt"""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
from paths import *
CTX.typ = 'd'; CTX.mode = 'bound'
print('%10s %12s %12s %12s %10s' % ('k', 'G(k)', 'E_G bound', 'k|G\'|', 'E_G/(k|G\'|)  [in units of eps]'))
for k in (0.3, 1.0, 1.425, 3.0, 10.0, 30.0, 99.0, 100.0, 1e3, 1e6, 1e12, 3.4e23):
    g, tr = lmd(R(k)); kk = mpv(k)
    slope = abs(1/kk-mp.psi(1, kk))*kk
    print('%10.4g %12.4e %12.3e %12.3e %10.2f   (%s)' % (k, g.v, g.e, slope, (g.e/slope)/D(2)**-52, 'digamma path, psi error assumed' if k < 100 else 'series'))
