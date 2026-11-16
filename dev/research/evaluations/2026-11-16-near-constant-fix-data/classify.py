"""Classify the snapshot differences of the near-constant fix against the 2026-11-15 baselines.

Usage (from the repository root):
  python3 dev/research/evaluations/2026-11-16-near-constant-fix-data/classify.py native|double
Reads dev/research/evaluations/2026-11-15-fit-snapshot-data/<mode>.jsonl.gz and
build/snapshots/<mode>. Per family: changed fields, every kind change (old and new
kind, outcome kind, status), ok->ok estimate moves (relative, largest first),
log-likelihood moves above 1e-12, and candidate-only changes of stopped cases."""
import sys, collections, math
from pathlib import Path
R = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(R/'dev/validation'))
from strict_common import load, number
mode = sys.argv[1]
old = load(R/'dev/research/evaluations/2026-11-15-fit-snapshot-data'/(mode+'.jsonl.gz'))
new = load(R/'build/snapshots'/mode)
fams = sorted({k[0] for k in new})
def rel(a, b):
    a, b = number(a), number(b)
    if a == b: return 0.0
    if not (math.isfinite(a) and math.isfinite(b)): return math.inf
    return abs(a-b)/max(1, abs(a))
def kind(r):
    o = r.get('outcome') or {}
    return '%s/%s/%s/%s' % (r['kind'], o.get('kind'), r.get('status', o.get('reason')), r.get('model'))
print('== %s: %d old rows of these families, %d new' % (mode, sum(1 for k in old if k[0] in fams), len(new)))
for fam in fams:
    keys = sorted(k for k in new if k[0] == fam and k in old)
    changed = collections.Counter(); kinds = []; est = []; ll = []; cand = []; lim = []
    same = 0
    for k in keys:
        a, b = old[k]['result'], new[k]['result']
        fields = tuple(sorted(f for f in set(a) | set(b) if a.get(f) != b.get(f)))
        if not fields: same += 1; continue
        changed[fields] += 1
        if kind(a) != kind(b): kinds.append((k, kind(a), kind(b)))
        elif a['kind'] == 'ok':
            if 'estimates' in fields:
                m = max(rel(a['estimates'][n], b['estimates'][n]) for n in a['estimates'])
                est.append((m, k, {n: (a['estimates'][n], b['estimates'][n]) for n in a['estimates'] if a['estimates'][n] != b['estimates'][n]}))
            if 'logLik' in fields and rel(a['logLik'], b['logLik']) > 1e-12: ll.append((rel(a['logLik'], b['logLik']), k, a['logLik'], b['logLik']))
        else:
            cand.append(k)
            m = 0.0
            for f in ('limit_loglik', 'loglik_lower', 'loglik_upper'):
                if f in a and f in b and isinstance(a[f], str) and isinstance(b[f], str): m = max(m, rel(a[f], b[f]))
            le = 0.0
            for n in (a.get('limit_estimates') or {}):
                if n in (b.get('limit_estimates') or {}): le = max(le, rel(a['limit_estimates'][n], b['limit_estimates'][n]))
            lim.append((le, m, k))
    print('\n### %s: %d cases, %d identical, %d changed' % (fam, len(keys), same, len(keys)-same))
    for f, c in changed.most_common(): print('  fields %s: %d' % (list(f), c))
    outs = collections.Counter()
    for k in keys:
        a, b = old[k]['result'], new[k]['result']
        if a.get('outcome') != b.get('outcome') or a.get('core_status') != b.get('core_status') or a.get('certified') != b.get('certified'):
            outs[(a['kind'], str(a.get('outcome')), a.get('core_status'), a.get('certified'), b['kind'], str(b.get('outcome')), b.get('core_status'), b.get('certified'))] += 1
    print('  outcome/core_status/certified changes: %d cases' % sum(outs.values()))
    for x, c in outs.most_common(): print('    %d x  %s | %s | core %s | cert %s  ->  %s | %s | core %s | cert %s' % ((c,) + x))
    print('  kind changes: %d' % len(kinds))
    for k, x, y in kinds: print('    %s|%s|%s: %s -> %s' % (k[1], k[2], '', x, y))
    est.sort(reverse=True)
    print('  ok->ok estimate moves: %d; max %.3g; above 1e-10: %d; above 1e-13: %d' % (len(est), est[0][0] if est else 0, sum(1 for e in est if e[0] > 1e-10), sum(1 for e in est if e[0] > 1e-13)))
    for m, k, d in est[:12]: print('    %.3g %s|%s %s' % (m, k[1], k[2], d))
    print('  ok->ok logLik moves above 1e-12: %d' % len(ll))
    for m, k, a, b in sorted(ll, reverse=True)[:8]: print('    %.3g %s|%s %s -> %s' % (m, k[1], k[2], a, b))
    print('  same kind, not ok (stopped, candidates etc. changed): %d' % len(cand))
    lim.sort(reverse=True)
    print('  stopped cases: max relative move of limit_estimates %.3g, of loglik fields %.3g' % (max([x[0] for x in lim] or [0]), max([x[1] for x in lim] or [0])))
    for le, m, k in lim[:8]: print('    limit_estimates %.3g, loglik %.3g  %s|%s %s' % (le, m, k[1], k[2], kind(old[k]['result'])))
    print('  stopped cases with limit_estimates moved above 1e-10: %d' % sum(1 for x in lim if x[0] > 1e-10))
