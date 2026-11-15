"""Verdict table: face values and parameters (x scale), snapshot outcome, and the sup verdict from profile.json."""
import mpmath as mp, sys, os, json
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import core, sup
mp.mp.dps = 50
here = os.path.dirname(os.path.abspath(__file__))
prof = json.load(open(os.path.join(here, 'profile.json')))
rows = sc = sup.sc
snap = sc.load(os.path.join(sup.ROOT, 'dev/research/evaluations/2026-11-15-fit-snapshot-data/native.jsonl.gz'))
S = sup.samples(); out = {}
for name, d in S.items():
    x = [mp.mpf(v) for v in d]; y = [mp.log(v) for v in x]; n = len(y)
    P, I, k, ls = core.faces(y); A = core.Acoef(y, k, ls)
    yp = max(y); kap = n/sum(yp-v for v in y)
    r = [v for kk, v in snap.items() if kk[0] == 'expweibull' and kk[2] == name][0]['result']
    best = max(prof[name]['rows'], key=lambda t: t[1])      # (lb, rel, T): best profile point relative to max(P,I)
    verdict = 'finite maximum' if best[1] > 1e-6 else ('power face' if P > I else 'inverse Weibull face')
    out[name] = dict(n=n, P=float(P), I=float(I), A=float(A), invweibull=dict(shape=mp.nstr(k, 12), scale=mp.nstr(mp.e**ls, 12)),
                     power=dict(endpoint=mp.nstr(mp.e**yp, 16), shape=mp.nstr(kap, 12)), best_profile_rel=best, verdict=verdict,
                     snapshot=dict(kind=r['kind'], message=r['message'][:70]))
    print('%-13s n=%d %-21s P=%.5g I=%.5g A=%.3f | IW(shape=%s scale=%s) PW(end=%s shape=%s) | best rel=%.2e @lb=%d | snap=%s' % (
        name, n, verdict, P, I, A, mp.nstr(k, 6), mp.nstr(mp.e**ls, 8), mp.nstr(mp.e**yp, 8), mp.nstr(kap, 6), best[1], best[0], r['kind']))
json.dump(out, open(os.path.join(here, 'verdicts.json'), 'w'), indent=1)
