"""Relative distance of every stored binom and gompertz case (all free) from its face boundary.

binom:    delta = n S + S^2 - n Q   (<= 0: Poisson limit);   gap = delta / (n Q - S^2)  = (mean-var)/var
gompertz: delta = n Q - 2 S^2       (>= 0: exponential limit); gap = delta / (2 S^2)
Exact rationals; the binary64 mirrors are those of cert_replay_faces.py.
"""
import sys, math, json, gzip, importlib.util, hashlib
from fractions import Fraction
from pathlib import Path
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
import strict_common as sc
spec = importlib.util.spec_from_file_location('cr', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
import cert_replay_faces as cf
archive = {}
for c in json.loads(gzip.decompress(cr.fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
rows = sc.load(str(ROOT/'dev/research/evaluations/2026-11-15-fit-snapshot-data/native.jsonl.gz'))
out = []
for family in ('binom', 'gompertz'):
    inputs = cr.rebuild(family, archive)
    for (fam, source, case), r in sorted(rows.items()):
        if fam != family or int(r['mask']) != 0: continue
        c, digest = inputs[(source, case)]
        x = [float(v) for v in c['data']]; n = len(x)
        if not all(map(math.isfinite, x)) or not x or (family == 'gompertz' and min(x) < 0): continue
        o = r['result'].get('outcome') or {}
        if family == 'binom':
            xi = [int(v) for v in x]; S = sum(xi); Q = sum(v*v for v in xi)
            if S == 0 or len(set(xi)) == 1: continue
            d = n*S+S*S-n*Q; g = Fraction(d, n*Q-S*S)
        else:
            fx = [Fraction(v) for v in x]; S = sum(fx); Q = sum(v*v for v in fx)
            if len(set(x)) == 1 or S == 0: continue
            d = n*Q-2*S*S; g = d/(2*S*S)
        out.append((abs(float(g)), family, source, case, n, o.get('kind'), o.get('certified'), float(g)))
out.sort()
for fam in ('binom', 'gompertz'):
    sub = [t for t in out if t[1] == fam]
    print('== %s: %d cases (all free, nonconstant)' % (fam, len(sub)))
    print('  exact ties (gap = 0):', sum(1 for t in sub if t[0] == 0))
    for t in sub[:12]: print('  |gap|=%.3e  gap=%+.3e  %s/%s n=%d outcome=%s certified=%s' % (t[0], t[7], t[2], t[3], t[4], t[5], t[6]))
