"""Dense tail sweep for psged, psnorm and psstd (TASK-220).

Two stages:

  points: place sweep inputs and compute their mpmath references.
      python3 sweep.py points --binary EVAL --out reference.json.gz
  check:  evaluate a compiled evaluator (evaluate.cpp) at those inputs and
      compare with the references under the d/p/q contract.
      python3 sweep.py check --binary EVAL --reference reference.json.gz --out result.json
  replay: the 3,899 archived TASK-189 SGED/SSTD probability witnesses
      (240-digit expected values) with the natural check down to DBL_MIN.
      python3 sweep.py replay --binary EVAL --out replay.json

Parameter points are every truth-table point of the three families plus
extra points that stress large GED shapes (the hazard of exp(-|y|^nu)
grows like nu*|log p|) and moderate Student shapes. For each point and each
tail, sweep targets cover log p in [log(DBL_MIN), -0.01]: a fixed grid
dense at both ends plus seeded log-uniform draws. The evaluator's own
quantile only places an input x; when it returns a non-finite value the
reference quantile is used. Every reference is computed independently at
the exact binary64 x with the family's truth.py route (the README density's
antiderivative), at 60 decimal digits.

Contract (dev/design/accuracy-contract.md): log tails within
1e-10*max(1,|v|); natural tails within relative 1e-10 when the exact value
is at least the smallest normal double. NaN is a refusal. 'worst' is the
largest finite error in units of that tolerance; bands: scope is
log(DBL_MIN) <= log p <= -0.01, near_one above, subnormal below (log only).
"""
import argparse
import gzip
import json
import math
import random
import subprocess
import sys
import time
import zlib
from pathlib import Path

def dump(value, path):
    """Write JSON, gzip-compressed when the name ends in .gz."""
    with (gzip.open(path, 'wt') if str(path).endswith('.gz') else open(path, 'w')) as f:
        json.dump(value, f, indent=1)


ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev'/'validation'))
import mpmath as mp  # noqa: E402
from quality.truth import module_at  # noqa: E402

DISTRIBUTIONS = ROOT/'univariateML-cpp'/'include'/'cppunivariate'/'distributions'
LOG_DBL_MIN = math.log(2.2250738585072014e-308)  # -708.3964185322641
DIGITS = 60
FIXED = [LOG_DBL_MIN, -708.39, -708.3, -708.0, -707.0, -705.0, -703.0, -700.5, -700.0, -699.5,
         -695.0, -690.7755278982137, -690.0, -680.0, -650.0, -600.0, -550.0, -500.0, -400.0,
         -300.0, -250.0, -200.0, -150.0, -100.0, -75.0, -50.0, -30.0, -20.0, -10.0, -5.0, -3.0,
         -2.0, -1.0, -0.7, -0.5, -0.3, -0.2, -0.1, -0.05, -0.03, -0.02, -0.0101, -0.01]
RANDOM = 24
NAMES = {'sged': ('mean', 'sd', 'nu', 'xi'), 'snorm': ('mean', 'sd', 'xi'), 'sstd': ('mean', 'sd', 'nu', 'xi')}


def extra_points(family):
    if family == 'sged':
        shapes = [5, 10, 20, 25, 30, 40, 60, 100, 300, 1e3, 1e4, 1e5, 5e5, 999999.0]
        return ([[0.0, 1.0, float(nu), xi] for nu in shapes for xi in (3.0, 0.4)]
                + [[0.0, 1.0, nu, xi] for nu in (0.06, 0.5, 1.0, 1.5) for xi in (3.0, 0.4)]
                # xi=1 delegates to pged: the symmetric member at large shape.
                + [[0.0, 1.0, nu, 1.0] for nu in (10.0, 30.0, 100.0, 1e3, 1e4, 1e5)])
    if family == 'snorm':
        return [[0.0, 1.0, xi] for xi in (0.5, 2.0, 1e-3, 1e3, 1e-5)]
    return ([[0.0, 1.0, nu, xi] for nu in (2.5, 4.0, 30.0, 1e3, 1e5, 1e7) for xi in (3.0, 0.4)]
            + [[0.0, 1.0, nu, 1.0] for nu in (30.0, 1e5)])


def full(family, theta):
    """(mean, sd, nu, xi) for the evaluator; snorm has no shape nu."""
    return theta if family != 'snorm' else [theta[0], theta[1], 2.0, theta[2]]


def evaluate(binary, lines):
    out = subprocess.run([binary], input='\n'.join(lines)+'\n', capture_output=True, text=True, check=True)
    return out.stdout.splitlines()


def points(args):
    rows = []
    started = time.monotonic()
    for family in ('sged', 'snorm', 'sstd'):
        truth = json.load(open(DISTRIBUTIONS/family/'truth.json'))
        route = module_at(DISTRIBUTIONS/family/'truth.py')
        thetas = [('truth', i, [float(v) for v in p]) for i, p in enumerate(truth['parameters'])]
        thetas += [('extra', i, p) for i, p in enumerate(extra_points(family))]
        for source, index, theta in thetas:
            mp.mp.dps = DIGITS
            t = {k: mp.mpf(v) for k, v in zip(NAMES[family], theta)}
            ref = route.build(t)
            m, s, nu, xi = full(family, theta)
            targets = []
            for lower in (True, False):
                rng = random.Random(zlib.crc32(f'{family}/{source}/{index}/{lower}'.encode()))
                draws = [-math.exp(rng.uniform(math.log(0.01), math.log(-LOG_DBL_MIN))) for _ in range(RANDOM)]
                targets += [(lower, lp) for lp in FIXED+draws]
            quantiles = evaluate(args.binary, [f'q {family} {m!r} {s!r} {nu!r} {xi!r} {lp!r} {int(lower)}'
                                              for lower, lp in targets])
            for (lower, lp), q in zip(targets, quantiles):
                x = float(q)
                if not math.isfinite(x):
                    x = float(ref.quantile(mp.mpf(lp), lower))
                if not math.isfinite(x):
                    continue
                lo, up = ref.tails(mp.mpf(x))
                rows.append(dict(family=family, source=source, index=index, theta=[m, s, nu, xi],
                                 lower=lower, target=lp, x=x,
                                 ref=[mp.nstr(lo, 30), mp.nstr(up, 30)]))
            print(f'{family} {source} {index} {len(rows)} rows {time.monotonic()-started:.0f}s', flush=True)
    with gzip.open(args.out, 'wt') as f:
        json.dump(dict(digits=DIGITS, log_dbl_min=LOG_DBL_MIN, rows=rows), f)


def check(args):
    data = json.load(gzip.open(args.reference, 'rt'))
    rows = data['rows']
    got = evaluate(args.binary, [f'p {r["family"]} ' + ' '.join(repr(v) for v in r['theta']) + f' {r["x"]!r}'
                                 for r in rows])
    mp.mp.dps = 40
    findings, summary = [], {}
    for r, line in zip(rows, got):
        values = [float(v) for v in line.split()]
        for tail in (0, 1):
            ref = mp.mpf(r['ref'][tail])
            if ref == 0 or not mp.isfinite(ref):
                continue
            band = 'scope' if LOG_DBL_MIN <= ref <= -0.01 else ('near_one' if ref > -0.01 else 'subnormal')
            for scale in ('log', 'natural'):
                if scale == 'natural' and ref < LOG_DBL_MIN:
                    continue
                v = values[tail] if scale == 'log' else values[2+tail]
                if scale == 'log':
                    tol = mp.mpf('1e-10')*max(1, abs(ref))
                    err = abs(mp.mpf(v)-ref) if math.isfinite(v) else None
                else:
                    truth = mp.exp(ref)
                    tol = mp.mpf('1e-10')*truth
                    err = abs(mp.mpf(v)-truth) if math.isfinite(v) else None
                # xi=1 delegates to the symmetric family (pged/pstd/pnorm).
                route = 'symmetric' if r['theta'][3] == 1.0 else 'skew'
                key = (r['family'], route, band, scale)
                s = summary.setdefault(key, dict(checks=0, nan=0, fail=0, worst=0.0))
                s['checks'] += 1
                normalized = None if err is None else float(err/tol)
                if normalized is None:
                    s['nan'] += 1
                else:
                    s['worst'] = max(s['worst'], normalized)
                    if normalized > 1:
                        s['fail'] += 1
                if normalized is None or normalized > args.report_above:
                    findings.append(dict(family=r['family'], source=r['source'], index=r['index'],
                                         theta=r['theta'], x=r['x'], lower=tail == 0, scale=scale,
                                         band=band, ref=r['ref'][tail], got=v, normalized=normalized))
    table = [dict(family=k[0], route=k[1], band=k[2], scale=k[3], **v) for k, v in sorted(summary.items())]
    dump(dict(binary=args.binary, rows=len(rows), summary=table, findings=findings), args.out)
    print(f'{"family":6} {"route":9} {"band":9} {"scale":7} {"checks":>7} {"nan":>5} {"fail":>5} {"worst":>10}')
    for t in table:
        print(f'{t["family"]:6} {t["route"]:9} {t["band"]:9} {t["scale"]:7} {t["checks"]:7d} {t["nan"]:5d} {t["fail"]:5d} {t["worst"]:10.3g}')


def replay(args):
    """Archived TASK-189 skew witnesses (probability rows) at the new natural threshold."""
    path = ROOT/'dev'/'research'/'evaluations'/'2026-11-11-dpqr-extreme-data'/'skew-smoke.json.gz'
    rows = [r for r in json.load(gzip.open(path, 'rt'))['rows'] if r['job'][1] == 1]
    got = evaluate(args.binary, [f'p {r["job"][0]} ' + ' '.join(repr(float(v)) for v in r['job'][5])
                                 + f' {r["job"][2]!r}' for r in rows])
    counts, findings = {}, []
    for r, line in zip(rows, got):
        family, _, x, lower, log, theta = r['job']
        values = [float(v) for v in line.split()]
        v = values[(0 if lower else 1)+(0 if log else 2)]
        b = float(r['expected'])
        if not log and not b >= 2.2250738585072014e-308:
            continue
        error = (abs(v-b)/(abs(b) if not log else max(1, abs(b)))) if math.isfinite(v) and math.isfinite(b) else (0 if v == b else math.inf)
        c = counts.setdefault(f'{family} {"log" if log else "natural"}', dict(checks=0, nan=0, fail=0, worst=0.0))
        c['checks'] += 1
        if math.isnan(v):
            c['nan'] += 1
        elif error > 1e-10:
            c['fail'] += 1
        else:
            c['worst'] = max(c['worst'], error)
        if math.isnan(v) or error > 1e-10:
            findings.append(dict(job=r['job'], expected=b, actual=v, error=error))
    dump(dict(binary=args.binary, summary=counts, findings=findings), args.out)
    for k, c in sorted(counts.items()):
        print(k, c)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('stage', choices=('points', 'check', 'replay'))
    parser.add_argument('--binary', required=True)
    parser.add_argument('--reference')
    parser.add_argument('--out', required=True)
    parser.add_argument('--report-above', type=float, default=0.1,
                        help='record findings with normalized error above this fraction of the tolerance')
    args = parser.parse_args()
    {'points': points, 'check': check, 'replay': replay}[args.stage](args)
