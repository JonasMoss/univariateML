#!/usr/bin/env python3
"""Independent 60-digit profile-root oracle; consumes probe.json, writes checks.json.

From the repository root (reserve one workstation slot for each command):
  c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$BOOST_INCLUDE_DIR" \
    dev/research/evaluations/2026-11-06-burr-scale-data/probe.cpp \
    -o build/scratch/burr-scale-probe
  build/scratch/burr-scale-probe \
    univariateML-cpp/include/cppunivariate/distributions/burr/fixed-scale-golden.json \
    > dev/research/evaluations/2026-11-06-burr-scale-data/probe.json
  python3 dev/research/evaluations/2026-11-06-burr-scale-data/check.py
"""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps = 60
folder = Path(__file__).resolve().parent
rows = json.loads((folder / 'probe.json').read_text())
checks = []
for row in rows:
    if row['status'] != 0:
        checks.append({'id': row['id'], 'status': row['status'], 'finite': False})
        continue
    # Parse the same binary64 observations exactly, independently of C++ logs.
    x = [mp.mpf(float(v)) for v in row['x']]
    z = [mp.log(v) for v in x]
    n = len(x)
    def at(c):
        g = mp.fsum(mp.log1p(mp.exp(c*t)) for t in z)
        gp = mp.fsum(t/(1+mp.exp(-c*t)) for t in z)
        d = mp.fsum(t/(1+mp.exp(c*t)) for t in z)
        return mp.mpf(n)/c-n*gp/g+d, g
    lo, hi = mp.mpf(0), mp.mpf(1)
    while at(hi)[0] > 0:
        hi *= 2
    for _ in range(150):
        mid = (lo+hi)/2
        if at(mid)[0] > 0:
            lo = mid
        else:
            hi = mid
    c = (lo+hi)/2
    _, g = at(c)
    k = n/g
    ll = n*(mp.log(k)+mp.log(c)-1)-mp.fsum(mp.log(v) for v in x)-mp.fsum(mp.log1p(mp.exp(-c*t)) for t in z)
    ek, ec, el = float(abs(mp.mpf(row['k'])/k-1)), float(abs(mp.mpf(row['c'])/c-1)), float(abs(mp.mpf(row['logLik'])-ll))
    contained = mp.mpf(row['lower']) <= ll <= mp.mpf(row['upper'])
    # Historical coefficient compatibility is checked by unchanged C++/R goldens.
    # Relative 1e-8 is a separate comparison to the exact profile root.
    assert ek < 1e-8 and ec < 1e-8 and el < 1e-9, (row['id'], ek, ec, el)
    assert contained, (row['id'], 'oracle outside practical likelihood bounds')
    checks.append({'id': row['id'], 'finite': True, 'k_relative_error': ek,
                   'c_relative_error': ec, 'logLik_absolute_error': el,
                   'oracle_inside_bounds': bool(contained),
                   'oracle_k': str(k), 'oracle_c': str(c), 'oracle_logLik': str(ll),
                   'microseconds': row['microseconds'], 'evaluations': row['evaluations']})
(folder / 'checks.json').write_text(json.dumps({'digits': mp.mp.dps, 'root_bisections': 150,
                                               'checks': checks}, indent=2)+'\n')
print(json.dumps(checks, indent=2))
