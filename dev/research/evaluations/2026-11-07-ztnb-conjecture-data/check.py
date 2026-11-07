"""Independent research checks, not a fitter or an interval-arithmetic proof.

Run through /tmp/umvl-cpu/run. Requires already installed mpmath/scipy.
All iterative work is bounded by --iterlim; exhaustion is an error.
"""
import argparse
from pathlib import Path
import json
import platform
from fractions import Fraction as Q

import mpmath as mp
import numpy as np
from scipy.optimize import linprog


def root(f, lo, hi, budget):
    assert f(lo) < 0 < f(hi)
    for _ in range(budget):
        mid = (lo + hi) / 2
        if f(mid) > 0:
            hi = mid
        else:
            lo = mid
        if hi - lo < mp.power(10, -(mp.mp.dps-20)) * (1 + abs(mid)):
            return (lo + hi) / 2
    raise RuntimeError('mean bracket exhausted')


def mean(r, t):
    if not t:
        return mp.mpf(1)
    return r * mp.expm1(t) / (-mp.expm1(-r*t))


def curves(r, m, budget):
    t = root(lambda t: mean(r, t)-m, mp.mpf(0), mp.log1p(m/r)+1, budget)
    E = lambda z: 1/mp.expm1(z)-1/z
    J = 1-t/mp.expm1(t)
    Jp = (mp.exp(t)*(t-1)+1)/mp.expm1(t)**2
    tp = t*E(r*t)/(1+E(t)-r*E(r*t))
    return t, m*J, -m*Jp*tp


def extremal(r, m, H, budget):
    eta = H/(m-1)
    if eta >= 1 or eta < 1/(r+1):
        return None, 'moment-infeasible'
    def hb(x):
        a = mp.digamma(r+x)-mp.digamma(r+1)
        return x-1-r*a, a-r*(mp.polygamma(1,r+1)-mp.polygamma(1,r+x))
    def c(x):
        return hb(x)[0]/(x-1)
    lo, hi = 2, 4
    for _ in range(budget):
        if eta <= c(hi):
            break
        lo, hi = hi, hi*2
    else:
        raise RuntimeError('support doubling exhausted')
    for _ in range(budget):
        if hi-lo <= 1:
            break
        mid = (lo+hi)//2
        if c(mid) < eta:
            lo = mid
        else:
            hi = mid
    else:
        raise RuntimeError('support integer bisection exhausted')
    j = lo
    h,b = hb(j)
    hn,bn = hb(j+1)
    if j <= 200:
        assert abs(h-sum(i/(r+i) for i in range(1,j))) < mp.mpf('1e-45')
        assert abs(b-sum(i/(r+i)**2 for i in range(1,j))) < mp.mpf('1e-45')
    # v are size-biased weights: v_j=(j-1)*p_j/(m-1).
    v = (hn/j-eta)/(hn/j-h/(j-1))
    pj = (m-1)*v/(j-1)
    pn = (m-1)*(1-v)/j
    p1 = 1-pj-pn
    if min(p1, pj, pn) < -mp.mpf('1e-45'):
        return None, 'moment-infeasible'
    B = pj*b+pn*bn
    alpha = (b/(j-1)-j/(r+j)**2)/(h/(j-1)-j/(r+j))
    beta = j/(r+j)**2-alpha*j/(r+j)
    assert abs(B-alpha*H-beta*(m-1)) < mp.mpf('1e-45')*(1+abs(B))
    return (j, p1, pj, pn, B), 'feasible'


def rational_majorants():
    """Exact arithmetic checks of the discrete supporting-plane lemma."""
    checks = 0
    for r in [Q(1, 10), Q(1, 2), Q(1), Q(7, 3), Q(10), Q(100)]:
        h = [Q(0), Q(0)]
        b = [Q(0), Q(0)]
        for x in range(2, 82):
            h.append(h[-1]+Q(x-1)/(r+x-1))
            b.append(b[-1]+Q(x-1)/(r+x-1)**2)
        for j in range(2, 31):
            alpha = (b[j]/(j-1)-(b[j+1]-b[j])) / (h[j]/(j-1)-(h[j+1]-h[j]))
            beta = b[j+1]-b[j]-alpha*(h[j+1]-h[j])
            for x in range(1, 82):
                gap = alpha*h[x]+beta*(x-1)-b[x]
                assert gap >= 0
                assert (gap == 0) == (x in [1, j, j+1])
                checks += 1
    return checks


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--iterlim', type=int, default=1000)
    ap.add_argument('--dps', type=int, default=85)
    ap.add_argument('--output', type=Path, default=Path(__file__).with_name('checks.json'))
    args = ap.parse_args()
    budget = min(args.iterlim, 100000)
    assert budget > 0
    mp.mp.dps = args.dps
    rows = []
    counts = {}
    lp_checks = []
    workloads = [(mstr, exponent, 3) for mstr in ['1.001','1.01','1.05','1.1','1.25','1.5','2','3','5','10','20','50','100'] for exponent in range(-24,25)]
    workloads += [(str(1+mp.power(10,a)), exponent, 1) for a in range(-8,7) for exponent in range(-12,13)]
    for mstr, exponent, divisor in workloads:
        m = mp.mpf(mstr)
        r = mp.power(10, mp.mpf(exponent)/divisor)
        t, H, D = curves(r, m, budget)
        ext, status = extremal(r, m, H, budget)
        counts[status] = counts.get(status, 0)+1
        if ext is None:
            continue
        j, p1, pj, pn, B = ext
        margin = (D-B)*(1+r)**2
        rows.append(dict(m=mstr, r=str(r), j=j, weights=list(map(str,[p1,pj,pn])),
                         scaled_margin=str(margin), tau=str(t)))
        assert margin > mp.mpf('1e-45'), rows[-1]
        if divisor == 3 and exponent in [-12, 0, 12] and mstr in ['1.1','2','10']:
            M = max(100, j+10)
            x = np.arange(1, M+1)
            rr = float(r)
            js = np.arange(1,M)
            hs = np.r_[0,np.cumsum(js/(rr+js))]
            bs = np.r_[0,np.cumsum(js/(rr+js)**2)]
            res = linprog(-bs, A_eq=[np.ones(M),x,hs], b_eq=[1,float(m),float(H)],
                          bounds=(0,None), method='highs', options={'threads':1})
            assert res.success, res.message
            error = abs(-res.fun-float(B))
            assert error < 2e-7
            lp_checks.append(dict(m=mstr,r=str(r),max_count=M,absolute_error=error))
    # Independent finite-difference envelope check, with mean re-solved.
    hist = [(1, Q(1,2)), (2,Q(1,4)), (5,Q(1,4))]
    m = sum(mp.mpf(w.numerator)/w.denominator*x for x,w in hist)
    def profile(r):
        t,_,_ = curves(r,m,budget)
        return sum(mp.mpf(w.numerator)/w.denominator*(mp.loggamma(r+x)-mp.loggamma(r)
                   -mp.loggamma(x+1)+x*mp.log(-mp.expm1(-t))) for x,w in hist)-mp.log(mp.expm1(r*t))
    r = mp.mpf('2.3')
    t,H,D = curves(r,m,budget)
    he = sum(mp.mpf(w.numerator)/w.denominator*sum(j/(r+j) for j in range(1,x)) for x,w in hist)
    be = sum(mp.mpf(w.numerator)/w.denominator*sum(j/(r+j)**2 for j in range(1,x)) for x,w in hist)
    step = mp.mpf('1e-12')
    pm, pc, pp = profile(r-step), profile(r), profile(r+step)
    first = (pp-pm)/(2*step)
    second = (pp-2*pc+pm)/step**2
    f_error = abs(first-(H-he)/r)
    fp_error = abs(first+r*second-(be-D))
    assert max(f_error,fp_error) < mp.mpf('1e-20')
    # A non-adjacent convex-order example: P(1)=P(3)=1/20, P(2)=9/10.
    lam = root(lambda z: z/(-mp.expm1(-z))-2 if z else -1,mp.mpf(0),mp.mpf(3),budget)
    # Only integer threshold 2 is nonautomatic; E[(Y-2)+] = 2 - 2 + P(Y=1).
    stoploss_margin = lam/mp.expm1(lam)-mp.mpf('0.05')
    assert stoploss_margin > mp.mpf('0.35')
    out = dict(python=platform.python_version(),mpmath=mp.__version__,precision=mp.mp.dps,
               iterlim=args.iterlim,counts=counts,grid_cells=len(workloads),minimum_margin=min(rows,key=lambda z:mp.mpf(z['scaled_margin'])),
               rational_majorant_checks=rational_majorants(),lp_checks=lp_checks,
               derivative_errors=list(map(str,[f_error,fp_error])),lambda_mean_two=str(lam),
               nonadjacent_stoploss_margin=str(stoploss_margin),rows=rows,
               qualification='Finite-grid evidence and exact rational regression checks; not a proof of the remaining scalar inequality.')
    args.output.write_text(json.dumps(out,indent=2)+'\n')
    print(json.dumps({k:v for k,v in out.items() if k!='rows'},indent=2))


if __name__ == '__main__':
    main()
