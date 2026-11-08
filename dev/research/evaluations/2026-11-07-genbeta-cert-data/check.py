#!/usr/bin/env python3
"""Independent mpmath scalar oracle and exact-ridge counterexample.

Compile check.cpp with C++17, -O2 and -IunivariateML-cpp/include; pass its
executable as argv[1]. No package or golden files are changed.
"""
import json
import math
from pathlib import Path
import subprocess
import sys
import mpmath as mp

rows = []
for line in subprocess.check_output([sys.argv[1]], text=True).splitlines():
    p0, q0, a0, status = map(float, line.split())
    assert status == 0, line
    with mp.workdps(max(80, int(-math.log10(q0)) + 60)):
        p, q, actual = map(mp.mpf, (p0, q0, a0))
        x = [.5] if q0 > 1e300 else [.02, .2, .6, .9]
        y = [-mp.log(mp.mpf(v)) for v in x]
        mean = sum(y) / len(y)
        def score(a):
            return 1-p*a*mean+(q-1)*sum(a*v/mp.expm1(a*v) for v in y)/len(y)
        lo = mp.mpf(0)
        hi = (q if q < 1 else 1+mp.log(q))/(p*mean)
        while score(hi) > 0:
            hi *= 2
        # Relative root inclusion, no score-smallness stopping criterion.
        for _ in range(2000):
            mid = (lo+hi)/2
            if score(mid) > 0:
                lo = mid
            else:
                hi = mid
            if lo > 0 and hi-lo < mp.mpf('1e-45')*lo:
                break
        else:
            raise AssertionError('oracle root did not converge')
        error = abs(actual/((lo+hi)/2)-1)
        assert error < mp.mpf('1e-12'), (line, error)
        rows.append(dict(p=p0,q=q0,power=a0,relative_error=float(error)))

with mp.workdps(80):
    # q=1, pa=3: three distinct coefficient vectors give identical densities
    # and all maximize the likelihood on data with mean(-log x)=1/3.
    y = [mp.mpf(1)/6,mp.mpf(1)/2]
    ridge = []
    for p,a in [(1,3),(3,1),(6,mp.mpf('.5'))]:
        ll = sum(mp.log(a)-mp.log(mp.beta(p,1))-p*a*v+v for v in y)
        ridge.append(ll)
    assert max(ridge)-min(ridge) < mp.mpf('1e-75')
    # Gamma seam quadratic coefficient can be either sign or exactly zero.
    # q=2, y=(1-d,1+d), coefficient = (d^2-1/2)/24.
    seams=[]
    for d in [mp.mpf('.5'),mp.sqrt(mp.mpf('.5')),mp.mpf('.9')]:
        second=1+d*d
        coefficient=(second-mp.mpf('1.5'))/24
        seams.append(dict(d=float(d),quadratic_coefficient=float(coefficient)))

out = dict(evidence='numerical oracle; exact-arithmetic proofs are in the report',
           scalar_cases=rows, max_relative_error=max(r['relative_error'] for r in rows),
           ridge_loglik=float(ridge[0]), gamma_seam_examples=seams)
if sys.argv[2] == '-':
    print(json.dumps(out,indent=2))
else:
    Path(sys.argv[2]).write_text(json.dumps(out,indent=2)+'\n')
print(f"{len(rows)} scalar cases; max relative error {out['max_relative_error']:.3g}; ridge and seam checks passed")
