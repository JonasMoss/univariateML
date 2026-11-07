"""Independent high-precision references for golden fits.

The reference MLE for a golden sample is computed from the README log density
alone: a damped Newton iteration at 50 significant digits in the free regular
coefficients, started from the golden estimates. Coefficients that are held
(golden `fixed`, README `fit_args`), integer, support-bound, kinked or active at
a domain bound are not refined and are reported with the reason. The result
records local evidence (score norm, negative-definite information) and global
evidence (the dossier's uniqueness proof, or none).

Usage (PYTHONPATH=dev/validation):
  python3 -m quality reference [ids ...]           # report only
  python3 -m quality reference --write [ids ...]   # update golden.json
"""
import argparse
import hashlib
import json
from pathlib import Path

import mpmath as mp
import sympy as sp
import yaml

from quality.density import density_expression
from quality.harness.check import DISTS, interval

DPS = 50


def _meta(folder):
    return yaml.safe_load((folder / 'README.md').read_text().split('---', 2)[1])


def _held_names(meta, golden):
    params = meta['params']
    return set(golden.get('fixed', [])) | {
        p['name'] for p in params if golden.get('fit_args', {}).get(p['name']) is not None}


def reference(id, dps=DPS):
    folder = DISTS / id
    meta = _meta(folder)
    golden = json.loads((folder / 'golden.json').read_text())
    params = meta['params']
    values = {k: float(v) if isinstance(v, str) else v for k, v in golden['estimates'].items()}
    domains = [interval(p['domain'], values) for p in params]
    syms, x, expr = density_expression(folder, meta, domains)
    held_names = _held_names(meta, golden)
    theta = [values[p['name']] for p in params]
    not_refined = {}
    for i, p in enumerate(params):
        if p['name'] in held_names:
            not_refined[p['name']] = 'held'
        elif p.get('integer'):
            not_refined[p['name']] = 'integer'
        elif p.get('bound'):
            not_refined[p['name']] = 'support bound'
        elif p.get('kink'):
            not_refined[p['name']] = 'kink'
        else:
            lo, hi = domains[i]
            if theta[i] <= lo or theta[i] >= hi:
                not_refined[p['name']] = 'at domain bound'
    free = [i for i, p in enumerate(params) if p['name'] not in not_refined]
    held = {syms[i]: sp.nsimplify(theta[i]) if params[i]['name'] in held_names else theta[i]
            for i in range(len(params)) if i not in free}
    e = expr.subs(held).doit()
    e = e.replace(sp.Abs, lambda a: sp.Piecewise((a, a >= 0), (-a, True)))
    fs = [syms[i] for i in free]
    data = golden['data']
    out = {'id': id, 'free': [params[i]['name'] for i in free], 'not_refined': not_refined}
    with mp.workdps(dps + 10):
        xs = [mp.mpf(v) for v in data]
        f = sp.lambdify([*fs, x], e, 'mpmath', cse=True)
        g = [sp.lambdify([*fs, x], sp.diff(e, s), 'mpmath', cse=True) for s in fs]
        h = [[sp.lambdify([*fs, x], sp.diff(e, a, b), 'mpmath', cse=True) for b in fs] for a in fs]
        full = lambda t: [theta[i] if i not in free else t[free.index(i)] for i in range(len(params))]

        def ll(t):
            try:
                v = mp.fsum(f(*t, xi) for xi in xs)
            except (ValueError, ZeroDivisionError, TypeError):
                return None
            return v if mp.isfinite(v) and mp.im(v) == 0 else None

        t = [mp.mpf(theta[i]) for i in free]
        cur = ll(t)
        if cur is None:
            out['error'] = 'log-likelihood not finite at the golden estimates'
            return out
        iterations = 0
        if free:
            for iterations in range(1, 200):
                G = mp.matrix([mp.fsum(gi(*t, xi) for xi in xs) for gi in g])
                H = mp.matrix([[mp.fsum(hij(*t, xi) for xi in xs) for hij in row] for row in h])
                try:
                    step = mp.lu_solve(H, -G)
                except ZeroDivisionError:
                    out['error'] = 'singular observed information'
                    return out
                scale = 1
                accepted = False
                for _ in range(80):
                    trial = [ti + scale * si for ti, si in zip(t, step)]
                    inside = all(domains[i][0] < float(tv) < domains[i][1] for i, tv in zip(free, trial))
                    val = ll(trial) if inside else None
                    if val is not None and val >= cur - mp.mpf(10) ** (-dps) * (1 + abs(cur)):
                        accepted = True
                        break
                    scale /= 2
                if not accepted:
                    out['error'] = 'line search failed'
                    return out
                t, cur = trial, val
                size = max(abs(scale * si) / max(1, abs(ti)) for si, ti in zip(step, t))
                if scale == 1 and size < mp.mpf(10) ** (-dps):
                    break
            G = [mp.fsum(gi(*t, xi) for xi in xs) for gi in g]
            H = mp.matrix([[mp.fsum(hij(*t, xi) for xi in xs) for hij in row] for row in h])
            try:
                mp.cholesky(-H)
                definite = True
            except (ValueError, ZeroDivisionError):
                definite = False
            out['score_norm'] = mp.nstr(mp.sqrt(mp.fsum(v ** 2 for v in G)), 3)
            out['information_negative_definite'] = definite
        out['iterations'] = iterations
        ref = full(t)
        out['estimates'] = {p['name']: mp.nstr(mp.mpf(v), 25) for p, v in zip(params, ref)}
        out['logLik'] = mp.nstr(cur, 25)
        out['relative_difference'] = {
            p['name']: float(abs((mp.mpf(theta[i]) - ref[i]) / ref[i])) if ref[i] != 0 else float(abs(theta[i]))
            for i, p in enumerate(params) if i in free}
        out['logLik_difference'] = float(golden['logLik'] - cur)
    existence = meta.get('existence', {}) or {}
    out['global'] = ('unique maximum proved in the dossier' if existence.get('uniqueness') == 'proved'
                     else 'local only')
    out['input_sha256'] = hashlib.sha256(json.dumps(
        {'data': data, 'log_density': meta['log_density'], 'held': {k: theta[i] for i, k in
         enumerate(p['name'] for p in params) if k in not_refined}},
        sort_keys=True).encode()).hexdigest()
    return out


def write(id, ref):
    path = DISTS / id / 'golden.json'
    golden = json.loads(path.read_text())
    golden['estimates'] = {k: float(v) for k, v in ref['estimates'].items()}
    golden['logLik'] = float(ref['logLik'])
    golden['reference'] = {
        'method': f'damped Newton at {DPS} digits on the README log density, from the fitted values',
        **{k: ref[k] for k in ('estimates', 'logLik', 'free', 'not_refined', 'score_norm',
                               'information_negative_definite', 'global', 'input_sha256') if k in ref},
    }
    tmp = path.with_suffix('.json.tmp')
    tmp.write_text(json.dumps(golden, indent=2, allow_nan=False) + '\n')
    tmp.replace(path)


def main(argv=None):
    ap = argparse.ArgumentParser(prog='quality reference')
    ap.add_argument('ids', nargs='*')
    ap.add_argument('--write', action='store_true')
    args = ap.parse_args(argv)
    ids = args.ids or sorted(p.name for p in DISTS.iterdir() if (p / 'golden.json').exists())
    failures = 0
    for id in ids:
        try:
            r = reference(id)
        except Exception as exc:  # report and continue: one family must not stop the census
            r = {'id': id, 'error': f'{type(exc).__name__}: {exc}'}
        if 'error' in r:
            failures += 1
            print(f'{id:14s} ERROR {r["error"]}')
            continue
        worst = max(r['relative_difference'].values(), default=0.0)
        print(f'{id:14s} worst rel {worst:.1e}  dlogLik {r["logLik_difference"]:+.1e}  '
              f'score {r.get("score_norm", "-")}  nd {r.get("information_negative_definite", "-")}  '
              f'{r["global"]}  not refined {r["not_refined"] or "-"}', flush=True)
        if args.write:
            write(id, r)
    return 1 if failures else 0
