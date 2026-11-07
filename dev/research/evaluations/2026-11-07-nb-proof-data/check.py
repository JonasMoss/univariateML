#!/usr/bin/env python3
"""Numerical sanity checks of the dossier proof, not a proof or production fitter.
Run from the repository root through /tmp/umvl-cpu/run. No random inputs.
"""
import argparse
import itertools
import json
import platform
from fractions import Fraction
from pathlib import Path
import mpmath as mp

parser = argparse.ArgumentParser()
parser.add_argument('--iterlim', type=int, default=128)
parser.add_argument('--output', type=Path, required=True)
args = parser.parse_args()
assert args.iterlim > 0
budget = 128 * min(args.iterlim, 100000)
mp.mp.dps = 80
margin = mp.mpf('1e-55')


def score(xs, r):
    m = mp.mpf(sum(xs)) / len(xs)
    return sum(mp.digamma(r+x)-mp.digamma(r) for x in xs) - len(xs)*mp.log1p(m/r)


def check(xs):
    n, total = len(xs), sum(xs)
    if not total:
        return None
    mf = Fraction(total, n)
    vf = sum((Fraction(x)-mf)**2 for x in xs)/n
    m, v = mp.mpf(mf.numerator)/mf.denominator, mp.mpf(vf.numerator)/vf.denominator
    grid = [mp.power(10, mp.mpf(k)/2) for k in range(-12, 17)]
    gs = []
    for r in grid:
        bp = (mp.log1p(m/r)-m/(r+m))/m
        g = -score(xs, r)/bp
        gs.append(g)
        # The limiting derivative is a strict upper bound, including v=m.
        assert n*(v-m)/m - g > margin
        if vf <= mf:
            assert score(xs, r) > margin
        if max(xs) <= 6:
            # Independently evaluate telescoping identity and curvature.
            curvature = mp.mpf(0)
            for q in xs:
                if not q:
                    continue
                d = min(q-1, int(mp.floor(m)))
                lhs = r*sum(1/(r+k)**2 for k in range(d+1))
                rhs = (d+1)/(r+d) + sum(mp.mpf(k)/((r+k)**2*(r+k-1)) for k in range(1,d+1))
                assert abs(lhs-rhs) < margin
                tqpp = sum((2*(m-k)*(m-3*k)*r+4*m*k*(k-m))/(r+k)**4 for k in range(q))
                assert tqpp > margin
                curvature += tqpp
            assert curvature > margin
    assert all(b-a > margin for a,b in zip(gs,gs[1:]))
    result = {'data':list(xs), 'mean':str(mf), 'variance':str(vf), 'regime':'finite' if vf>mf else 'poisson'}
    if vf > mf:
        lo = hi = mp.mpf(1)
        work = 0
        while score(xs,lo) <= 0:
            lo /= 2; work += 1
            assert work < budget, 'bracket budget exhausted'
        while score(xs,hi) >= 0:
            hi *= 2; work += 1
            assert work < budget, 'bracket budget exhausted'
        while (hi-lo)/lo > mp.mpf('1e-35'):
            mid = (lo+hi)/2
            if score(xs,mid) > 0:
                lo = mid
            else:
                hi = mid
            work += 1
            assert work < budget, 'bisection budget exhausted'
        assert score(xs,lo) > 0 and score(xs,hi) < 0
        assert abs(score(xs,(lo+hi)/2)) < mp.mpf('1e-30')
        result.update(root_lower=mp.nstr(lo,45),root_upper=mp.nstr(hi,45),search_steps=work)
    return result

samples = [xs for n in range(1,5) for xs in itertools.combinations_with_replacement(range(6),n) if sum(xs)]
samples += [(1,1,10), (0,0,0,100)]
for k in (2,10,100,1000):
    for offset in (-1,0,1):
        m = k*k+offset
        samples.append((m-k,m+k))
# Exact omitted-boundary-term calculation for AEE preprint Case 3, M=3.
# Its equation (4) gives u(z,0,F2)=-F2/[2(1+z)], with F2 held.
f2 = Fraction(1, 4)
u1, u2 = -f2 / (2 * (1+1)), -f2 / (2 * (1+2))
assert (u1, u2, u2-u1) == (Fraction(-1,16), Fraction(-1,24), Fraction(1,48))
rows = [check(xs) for xs in samples]
out = {'python':platform.python_version(),'mpmath':mp.__version__,'digits':mp.mp.dps,
       'comparison_margin':str(margin),'grid':'29 half-decade sizes, 1e-6 through 1e8',
       'iterlim':args.iterlim,'budget_per_root':budget,'sample_count':len(rows),
       'finite':sum(x['regime']=='finite' for x in rows),
       'poisson':sum(x['regime']=='poisson' for x in rows),
       'equality':sum(x['mean']==x['variance'] for x in rows),
       'aee_fixed_coordinate_boundary':{'F2':str(f2),'u_at_z1':str(u1),'u_at_z2':str(u2),'difference':str(u2-u1)},
       'result':'PASS','samples':rows}
args.output.write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps({k:v for k,v in out.items() if k!='samples'}))
