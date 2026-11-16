#!/usr/bin/env python3
"""High-precision maximizers of the near-constant paralogis, invparalogis and fatigue (mu = 0) samples,
and the log likelihood at given double coefficients.  Samples from the stored snapshot (strict_common.load).
Usage: python3 reference.py            prints the maximizers and the rounded-point likelihoods
       python3 reference.py FAMILY ALPHA_HEX SCALE_HEX   likelihood at given coefficients (hexfloat)
"""
import sys, math
from pathlib import Path
import mpmath as mp
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
import strict_common as sc
mp.mp.dps = 120
SNAP = ROOT/'dev/research/evaluations/2026-11-15-fit-snapshot-data/native.jsonl.gz'

def sample(fam):
    r = sc.load(str(SNAP))[(fam, 'class', 'near-constant')]
    return [mp.mpf(float(v)) for v in r['data']]

def loglik(fam, x, a, th):
    a, th = mp.mpf(a), mp.mpf(th)
    if fam == 'paralogis':
        return sum(2*mp.log(a) + a*mp.log(v/th) - mp.log(v) - (a+1)*mp.log1p((v/th)**a) for v in x)
    if fam == 'invparalogis':
        return sum(2*mp.log(a) + a*mp.log(th/v) - mp.log(v) - (a+1)*mp.log1p((th/v)**a) for v in x)
    b = th   # fatigue, mu = 0: alpha = a, beta = th
    return sum(-mp.log(2*a*b*mp.sqrt(2*mp.pi)) + mp.log((b/v)**mp.mpf(.5)+(b/v)**mp.mpf(-.5)) - mp.log(2*v) - ((v/b)+(b/v)-2)/(2*a*a) + mp.log(2) - mp.log(2) for v in x) if False else sum(
        -mp.log(a) - mp.log(mp.sqrt(2*mp.pi)) + mp.log((mp.sqrt(v/b)+mp.sqrt(b/v))/(2*v)) - (v/b + b/v - 2)/(2*a*a) for v in x)

def maximize(fam, x):
    x0 = x[0]
    if fam == 'fatigue':
        eps = mp.log(x[2]/x[0])
        a = mp.sqrt(2)/3*eps; b = x0*mp.exp(eps/3)
        f = lambda la, lb: loglik(fam, x, mp.exp(la), x0*mp.exp(lb))
        # Newton in (log alpha, v) with v = log(beta/x0)
        z = [mp.log(a), eps/3]
        for _ in range(40):
            g = [mp.diff(lambda p: f(p, z[1]), z[0]), mp.diff(lambda p: f(z[0], p), z[1])]
            H = mp.matrix(2, 2)
            H[0, 0] = mp.diff(lambda p: f(p, z[1]), z[0], 2); H[1, 1] = mp.diff(lambda p: f(z[0], p), z[1], 2)
            H[0, 1] = H[1, 0] = mp.diff(f, (z[0], z[1]), (1, 1))
            s = mp.lu_solve(H, mp.matrix(g)); z = [z[0]-s[0], z[1]-s[1]]
            if abs(s[0]) + abs(s[1]/eps) < mp.mpf(10)**-50: break
        return mp.exp(z[0]), x0*mp.exp(z[1])
    sg = 1 if fam == 'paralogis' else -1
    d = x[2]/x[0]-1
    s0 = mp.mpf('2.116363') if fam == 'paralogis' else mp.mpf('3.192688')
    # coordinates (la, zz): theta = x0 exp(-sg zz / a)
    def f(la, zz):
        a = mp.exp(la); return loglik(fam, x, a, x0*mp.exp(-sg*zz/a))
    z = [mp.log(s0/d), mp.mpf(-38.6867) if fam == 'paralogis' else mp.mpf(-37.5)]
    for _ in range(60):
        g = [mp.diff(lambda p: f(p, z[1]), z[0]), mp.diff(lambda p: f(z[0], p), z[1])]
        H = mp.matrix(2, 2)
        H[0, 0] = mp.diff(lambda p: f(p, z[1]), z[0], 2); H[1, 1] = mp.diff(lambda p: f(z[0], p), z[1], 2)
        H[0, 1] = H[1, 0] = mp.diff(f, (z[0], z[1]), (1, 1))
        s = mp.lu_solve(H, mp.matrix(g)); z = [z[0]-s[0], z[1]-s[1]]
        if abs(s[0]) + abs(s[1]) < mp.mpf(10)**-50: break
    a = mp.exp(z[0]); return a, x0*mp.exp(-sg*z[1]/a)

if __name__ == '__main__':
    if len(sys.argv) == 4:
        fam = sys.argv[1]; x = sample(fam)
        print(mp.nstr(loglik(fam, x, float.fromhex(sys.argv[2]), float.fromhex(sys.argv[3])), 15)); sys.exit()
    for fam in ('paralogis', 'invparalogis', 'fatigue'):
        x = sample(fam)
        a, th = maximize(fam, x)
        L = loglik(fam, x, a, th)
        ra, rth = float(a), float(th)
        print(fam, 'sample', [float(v).hex() for v in x])
        print('  alpha', mp.nstr(a, 30), ' scale', mp.nstr(th, 30), ' loglik', mp.nstr(L, 15))
        print('  rounded alpha', ra.hex(), ' rounded scale', rth.hex(), ' loglik there', mp.nstr(loglik(fam, x, ra, rth), 15), ' loss', mp.nstr(L-loglik(fam, x, ra, rth), 4))
        print('  scale in ulps of x0:', mp.nstr((th-x[0])/(x[2]-x[0]), 20))
