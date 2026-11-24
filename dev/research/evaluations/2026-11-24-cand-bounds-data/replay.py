"""Compare the four snapshots and independently evaluate TASK-236 candidates.

Run at the repository root with the path containing fresh native, double,
routes-native and routes-double snapshot directories.
"""
import argparse
from collections import Counter
import gzip
import json
import math
from pathlib import Path
import runpy

import mpmath as mp


MODES = ('native', 'double', 'routes-native', 'routes-double')
EVALUATIONS = Path('dev/research/evaluations')
BASELINE = EVALUATIONS / '2026-11-15-fit-snapshot-data'


def load(path):
    def key(row):
        return row['family'], row['source'], row['case'], row['input_sha256']

    if path.is_dir():
        return {key(row): row for part in sorted(path.glob('*.jsonl'))
                for row in map(json.loads, part.read_text().splitlines()[1:])}
    with gzip.open(path, 'rt') as file:
        return {key(row): row for row in map(json.loads, file)}


def moves(before, after, writer, mode):
    counts = Counter()
    for key in sorted(before.keys() | after.keys()):
        if key not in before or key not in after:
            print(mode, *key[:3], 'added' if key in after else 'removed', sep='\t', file=writer)
            counts['row changes'] += 1
            continue
        old, new = before[key]['result'], after[key]['result']
        fields = [f for f in sorted(old.keys() | new.keys()) if old.get(f) != new.get(f)]
        if fields:
            print(mode, *key[:3], ','.join(fields), sep='\t', file=writer)
            counts[key[0]] += 1
    return dict(sorted(counts.items()))


def likelihood(model, c, x):
    n = len(x)
    if model == 'unif':
        lo, hi = c['min'], c['max']
        return -n * mp.log(hi-lo) if lo <= min(x) and max(x) <= hi else -mp.inf
    if model == 'norm' or model == 'hnorm':
        loc, sd = (c['mean'], c['sd']) if model == 'norm' else (c['mu'], c['sigma'])
        return -n * mp.log(sd * mp.sqrt(2 * mp.pi)) - mp.fsum(((v-loc)/sd)**2 for v in x)/2 + (
            n * mp.log(2) if model == 'hnorm' else 0)
    if model == 'pareto':
        a, b = c['a'], c['b']
        return mp.fsum(mp.log(a/v)-a*mp.log(v/b) for v in x)
    if model == 'power':
        a, b = c['beta'], c['alpha']
        return mp.fsum(mp.log(a/v)+a*mp.log(v/b) for v in x)
    if model in ('weibull', 'invweibull'):
        shape, scale = c['shape'], c['scale']
        log_power = [shape * mp.log(v/scale) for v in x]
        if model == 'invweibull':
            log_power = [-v for v in log_power]
        return mp.fsum(mp.log(shape/v)+z-mp.exp(z) for v,z in zip(x, log_power))
    if model == 'cmp':
        eta, nu = mp.log(c['lambda']), c['nu']
        mode = int(mp.floor(mp.exp(eta/nu)))
        normalizer = mp.mpf(1)
        for direction in (-1, 1):
            k, weight = mode, mp.mpf(1)
            while k > 0 or direction > 0:
                if direction > 0:
                    k += 1
                    weight *= mp.exp(eta - nu * mp.log(k))
                else:
                    weight *= mp.exp(nu * mp.log(k) - eta)
                    k -= 1
                normalizer += weight
                if weight < mp.mpf('1e-65'):
                    break
        logz = mp.log(normalizer)
        reference = nu * mp.loggamma(mode + 1)
        return mp.fsum((v-mode)*eta-nu*mp.loggamma(v+1)+reference-logz for v in x)
    raise ValueError(model)


def analytic_face(model, c, x, held):
    n = len(x)
    if model == 'unif':
        if 'mu' in held:
            width = 2*max(abs(v-held['mu']) for v in x)
        elif 'mean' in held and 'xi' in held:
            centre, skew = held['mean'], held['xi']
            width = (1+skew**2)*max(centre-min(x), (max(x)-centre)/skew**2)
        else:
            width = max(x)-min(x)
        return dict(width=width, loglik=-n*mp.log(width))
    if model in ('norm', 'hnorm'):
        mean = mp.fsum(x)/n if model == 'norm' else c['mu']
        sd = mp.sqrt(mp.fsum((v-mean)**2 for v in x)/n)
        coef = dict(mean=mean, sd=sd) if model == 'norm' else dict(mu=mean, sigma=sd)
        return dict(**coef, loglik=likelihood(model,coef,x))
    if model == 'pareto':
        shape = n/mp.fsum(mp.log(v/min(x)) for v in x)
        return dict(a=shape, b=min(x), loglik=likelihood(model,dict(a=shape,b=min(x)),x))
    if model == 'power':
        shape = n/mp.fsum(mp.log(max(x)/v) for v in x)
        return dict(beta=shape, alpha=max(x),
                    loglik=likelihood(model,dict(beta=shape,alpha=max(x)),x))
    if model in ('weibull', 'invweibull'):
        sign = -1 if model == 'invweibull' else 1
        logs = [sign*mp.log(v/x[0]) for v in x]
        unit = max(logs)-min(logs)
        z = [(v-max(logs))/unit for v in logs]

        def score(t):
            weights = [mp.exp(t*v) for v in z]
            return 1/t+mp.fsum(z)/n-mp.fsum(v*w for v,w in zip(z,weights))/mp.fsum(weights)

        lo, hi = mp.mpf('0.1'), mp.mpf(16)
        assert score(lo) > 0 > score(hi)
        for _ in range(240):
            mid = (lo+hi)/2
            if score(mid) > 0:
                lo = mid
            else:
                hi = mid
        t = (lo+hi)/2
        log_scale = max(logs)+unit*mp.log(mp.fsum(mp.exp(t*v) for v in z)/n)/t
        coef = dict(shape=t/unit,scale=x[0]*mp.exp(sign*log_scale))
        return dict(**coef, loglik=likelihood(model,coef,x))
    return dict(loglik=likelihood(model,c,x))


def coefficients(candidate):
    return {name: mp.mpf(float.fromhex(value)) for name,value in candidate['estimates'].items()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('snapshots', type=Path)
    parser.add_argument('evidence', type=Path)
    args = parser.parse_args()
    known = json.loads(Path('dev/validation/strict-known.json').read_text())['known']
    fs = runpy.run_path('dev/validation/fit-snapshot.py')
    archive = {}
    with gzip.open(fs['ARCHIVE'],'rt') as file:
        for row in json.load(file):
            archive.setdefault(row['family'], []).append(row)
    mp.mp.dps = 80
    with (args.evidence/'snapshot-moves.tsv').open('w') as changes, (
            args.evidence/'candidate-replay.jsonl').open('w') as evidence:
        print('mode\tfamily\tsource\tcase\tfields', file=changes)
        for mode in MODES:
            before = load(BASELINE/f'{mode}.jsonl.gz')
            after = load(args.snapshots/mode)
            print(mode, moves(before, after, changes, mode))
            old = {key[:3]:row['result'] for key,row in before.items()}
            fresh = {key[:3]:row['result'] for key,row in after.items()}
            for key, entry in known.items():
                item, precision, family, source, case = key.split('|')
                if precision != mode or entry['ref'] != 'TASK-236':
                    continue
                inputs = next(r for r in fs['cases'](family,archive)
                              if (r['source'],r['case']) == (source,case))
                x = [mp.mpf(v) for v in inputs['data']]
                held = {name:mp.mpf(v) for name,v in inputs['held'].items()}
                i = int(item.split('#')[1])
                old_fit, new_fit = old[family,source,case], fresh[family,source,case]
                old_face, new_face = (r['candidates'][i] for r in (old_fit,new_fit))
                model = old_face['model']
                assert new_face['model'] == model
                old_point, new_point = coefficients(old_face), coefficients(new_face)
                new_value = likelihood(model,new_point,x)
                lo,hi = (float.fromhex(new_face[k]) for k in ('loglik_lower','loglik_upper'))
                slack = mp.mpf('1e-10')*max(1,abs(lo),abs(hi))
                assert lo-slack <= new_value <= hi+slack, key
                closed = analytic_face(model,old_point,x,held)
                other = {}
                if family in ('burr', 'pareto4', 'invburr'):
                    other_face = old_fit['candidates'][0]
                    other_model = other_face['model']
                    other_value = analytic_face(other_model, coefficients(other_face), x, held)
                    other = {other_model: mp.nstr(other_value['loglik'], 35)}
                print(json.dumps(dict(key=key, law=model, archive_finding=entry['detail'],
                    input_hex=[float(v).hex() for v in inputs['data']],
                    held_hex={k:float(v).hex() for k,v in inputs['held'].items()},
                    old_candidate=old_face, new_candidate=new_face,
                    old_point_ll=mp.nstr(likelihood(model,old_point,x),30),
                    new_point_ll=mp.nstr(new_value,30),
                    analytic_face={k:mp.nstr(v,35) for k,v in closed.items()},
                    competing_face_loglik=other,
                    input_to_comparison={'ged': 'rounded radius',
                        'invtrgamma': 'absolute log gaps',
                        'cmb': 'uncentred likelihood'}.get(family, 'analytic face'),
                    old_global_bounds=[old_fit['loglik_lower'],old_fit['loglik_upper']],
                    new_global_bounds=[new_fit['loglik_lower'],new_fit['loglik_upper']],
                    old_outcome=old_fit['outcome'], new_outcome=new_fit['outcome'],
                    old_selected=old_fit['model'], new_selected=new_fit['model'])),file=evidence)


if __name__ == '__main__':
    main()
