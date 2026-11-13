#!/usr/bin/env python3
"""Check every changed estimate of the fix-nonfinite snapshots against 50 digits.

Run from the repository root after the snapshots (build/snap/{native,double}):
    python3 dev/research/evaluations/2026-11-13-fix-nonfinite-data/check-snapshot-diffs.py
For each case whose estimates changed against the stored baseline it prints
the baseline and new values with their relative errors against a 50-digit
reference computed from the closed-form MLE equations (norm, gamma) or the
location score equation (logis, scale held). Writes snapshot-diff-check.json.
"""
import gzip, json, sys
from pathlib import Path
import mpmath as mp

mp.mp.dps = 50
ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
BASE = ROOT/'dev/research/evaluations/2026-11-13-fit-snapshot-data'
ARCHIVE = ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
archive = {('%06d' % c['index']): c for c in json.loads(gzip.decompress(ARCHIVE.read_bytes()))}

def load(path):
    rows = {}
    if path.is_file():
        with gzip.open(path, 'rt') as f:
            for t in f:
                r = json.loads(t); rows[(r['family'], r['source'], r['case'])] = r
    else:
        for part in path.glob('*.jsonl'):
            for t in part.read_text().splitlines()[1:]:
                r = json.loads(t); rows[(r['family'], r['source'], r['case'])] = r
    return rows

def num(v):
    return None if v in (None, 'nan') else mp.mpf(float.fromhex(v)) if isinstance(v, str) and v.startswith(('0x', '-0x')) else v

def reference(family, data, held):
    x = [mp.mpf(v) for v in data]
    n = len(x)
    if family == 'norm':
        m = sum(x)/n; sd = mp.sqrt(sum((v-m)**2 for v in x)/n)
        return {'mean': m, 'sd': sd, 'logLik': -n*(mp.log(sd)+mp.log(2*mp.pi)/2)-mp.mpf(n)/2}
    if family == 'gamma':
        s = mp.log(sum(x)/n)-sum(mp.log(v) for v in x)/n
        k = mp.findroot(lambda k: mp.log(k)-mp.digamma(k)-s, 1/(2*s))
        th = sum(x)/n/k
        return {'shape': k, 'scale': th,
                'logLik': sum(-k*mp.log(th)-mp.loggamma(k)+(k-1)*mp.log(v)-v/th for v in x)}
    if family == 'logis' and 'scale' in held and 'location' not in held:
        s = mp.mpf(held['scale'])
        mu = mp.findroot(lambda m: sum(mp.tanh((v-m)/(2*s)) for v in x), sum(x)/n)
        return {'location': mu, 'scale': s,
                'logLik': sum(-(v-mu)/s-mp.log(s)-2*mp.log1p(mp.exp(-(v-mu)/s)) for v in x)}
    return None

out = []
for mode in ('native', 'double'):
    old, new = load(BASE/(mode+'.jsonl.gz')), load(ROOT/'build/snap'/mode)
    for key in sorted(new):
        if key not in old: continue
        a, b = old[key]['result'], new[key]['result']
        if a['estimates'] == b['estimates'] and a['logLik'] == b['logLik']: continue
        family, source, case = key
        data = new[key].get('data') or archive[case]['data']
        ref = reference(family, data, new[key]['held'])
        row = dict(mode=mode, family=family, source=source, case=case, n=len(data))
        for name in sorted(set(a['estimates']) | set(b['estimates']) | {'logLik'}):
            va = num(a['logLik'] if name == 'logLik' else a['estimates'].get(name))
            vb = num(b['logLik'] if name == 'logLik' else b['estimates'].get(name))
            r = ref[name] if ref else None
            rel = lambda v: None if v is None or r is None else float(abs(v-r)/abs(r))
            row[name] = dict(baseline=None if va is None else float(va), new=None if vb is None else float(vb),
                             reference=None if r is None else mp.nstr(r, 20),
                             baseline_rel_error=rel(va), new_rel_error=rel(vb))
        out.append(row)
        print(json.dumps(row))
(HERE/'snapshot-diff-check.json').write_text(json.dumps(out, indent=1)+'\n')
