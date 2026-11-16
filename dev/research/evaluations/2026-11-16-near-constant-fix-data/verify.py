#!/usr/bin/env python3
"""Check every changed case of the native snapshot against a 100-digit maximizer.

For each case of paralogis, invparalogis, fatigue whose record differs from the 2026-11-15 baseline and whose new
record is an estimate: Newton at 100 digits in an offset coordinate from the returned point, then relative differences
of the returned coefficients from the maximizer, and the log likelihood loss (supremum minus value at the returned point).
Usage (repository root): python3 dev/research/evaluations/2026-11-16-near-constant-fix-data/verify.py [native|double]
"""
import sys, math
from pathlib import Path
import mpmath as mp
R = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(R/'dev/validation'))
from strict_common import load, number, archive_data
mp.mp.dps = 100
mode = sys.argv[1] if len(sys.argv) > 1 else 'native'
old = load(R/'dev/research/evaluations/2026-11-15-fit-snapshot-data'/(mode+'.jsonl.gz'))
new = load(R/'build/snapshots'/mode)

def ll(fam, x, a, th):
    if fam == 'paralogis': return sum(2*mp.log(a)+a*mp.log(v/th)-mp.log(v)-(a+1)*mp.log1p((v/th)**a) for v in x)
    if fam == 'invparalogis': return sum(2*mp.log(a)+a*mp.log(th/v)-mp.log(v)-(a+1)*mp.log1p((th/v)**a) for v in x)
    return sum(-mp.log(a)-mp.log(mp.sqrt(2*mp.pi))+mp.log((mp.sqrt(v/th)+mp.sqrt(th/v))/(2*v))-(v/th+th/v-2)/(2*a*a) for v in x)

def maximize(fam, x, a0, t0):
    x0 = min(x)
    sg = -1 if fam == 'invparalogis' else 1
    # coordinates (la, y): theta = x0 exp(sg*y/a) for the paralogis pair; fatigue: beta = x0 exp(y)
    if fam == 'fatigue':
        f = lambda la, y: ll(fam, x, mp.exp(la), x0*mp.exp(y)); p = [mp.log(a0), mp.log(t0/x0)]
    else:
        f = lambda la, y: ll(fam, x, mp.exp(la), x0*mp.exp(sg*y/mp.exp(la))); p = [mp.log(a0), mp.log(t0/x0)*mp.exp(p0 := mp.log(a0))*sg]
    for _ in range(80):
        g = [mp.diff(lambda t: f(t, p[1]), p[0]), mp.diff(lambda t: f(p[0], t), p[1])]
        H = mp.matrix(2, 2); H[0, 0] = mp.diff(lambda t: f(t, p[1]), p[0], 2); H[1, 1] = mp.diff(lambda t: f(p[0], t), p[1], 2)
        H[0, 1] = H[1, 0] = mp.diff(f, (p[0], p[1]), (1, 1))
        s = mp.lu_solve(H, mp.matrix(g)); p = [p[0]-s[0], p[1]-s[1]]
        if abs(s[0]) + abs(s[1]) < mp.mpf(10)**-40: break
    a = mp.exp(p[0]); th = x0*mp.exp(p[1]) if fam == 'fatigue' else x0*mp.exp(sg*p[1]/a)
    return a, th, f(*p)

names = {'paralogis': ('shape', 'scale'), 'invparalogis': ('shape', 'scale'), 'fatigue': ('alpha', 'beta')}
print('%-13s %-22s %-9s %-22s %-12s %-12s %-10s %s' % ('family', 'case', 'old', 'n', 'rel shape', 'rel scale', 'loss', 'old->new move'))
for k in sorted(new):
    if k[0] not in names: continue
    a, b = old[k]['result'], new[k]['result']
    if a == b or b['kind'] != 'ok': continue
    held = new[k].get('held', {})
    if held and set(held) - {'mu'}: print(k, 'held', held, 'not on the offset route'); continue
    raw = new[k].get('data')
    if raw is None: raw = archive_data()[int(k[2])]
    x = [mp.mpf(number(v) if isinstance(v, str) else v) for v in raw]
    if k[0] == 'fatigue': x = [v-mp.mpf(held.get('mu', 0.0)) for v in x]
    an, sn = names[k[0]]
    a1, s1 = number(b['estimates'][an]), number(b['estimates'][sn])
    try:
        A, S, L = maximize(k[0], x, mp.mpf(a1), mp.mpf(s1))
    except Exception as e:
        print(k, 'verification failed', type(e).__name__, e); continue
    Lr = ll(k[0], x, mp.mpf(a1), mp.mpf(s1))
    move = ''
    if a['kind'] == 'ok':
        move = 'ok->ok, old rel shape %s' % mp.nstr(abs(mp.mpf(number(a['estimates'][an]))/A-1), 3)
    print('%-13s %-22s %-9s n=%-3d %-12s %-12s %-10s %s' % (k[0], k[1]+'|'+k[2], a['kind'], len(x), mp.nstr(abs(mp.mpf(a1)/A-1), 3), mp.nstr(abs(mp.mpf(s1)/S-1), 3), mp.nstr(L-Lr, 4), move))
