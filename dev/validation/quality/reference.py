"""Independent high-precision references for golden fits.

The reference MLE for a golden sample is computed from the README log density
alone: damped Newton at 50 significant digits in free regular coefficients,
started from the stored estimates, plus independent closed-form and finite
integer routes for nonregular coordinates. Remaining held, kinked or bound-active
coordinates carry explicit reasons. Local score/curvature checks, one-sided
boundary scores, scoped dossier statements and numerical multistart evidence
are recorded separately. Binary64 observations and held values are interpreted
exactly, without decimal rationalization.

Usage (PYTHONPATH=dev/validation):
  python3 -m quality reference [ids ...]           # report only
  python3 -m quality reference --write [ids ...]   # update golden.json
"""
import argparse
import hashlib
import json
import re
from pathlib import Path

import mpmath as mp
import sympy as sp
import yaml

from quality.density import density_expression
from quality.harness.check import DISTS, interval, check, r_bound_to_sympy

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
    held_names = _held_names(meta, golden)
    # Frozen fit arguments are authoritative, including their numeric form.
    # Writing rounded reference estimates must not change the next input hash.
    for name in held_names:
        supplied = golden.get('fit_args', {}).get(name)
        if supplied is not None:
            values[name] = float(supplied) if isinstance(supplied, str) else supplied
    for i, p in enumerate(params):
        if p['name'] in held_names:
            continue
        rules = p.get('fit_domain', [])
        if isinstance(rules, dict):
            rules = [rules]
        for rule in rules:
            if any(set(h).issubset(held_names) for h in rule['restriction'].get('exempt_when_held', [])):
                continue
            lo, hi = domains[i]
            domains[i] = (max(lo, rule.get('lower', -mp.inf)), min(hi, rule.get('upper', mp.inf)))
    syms, x, expr = density_expression(folder, meta, domains)
    theta = [values[p['name']] for p in params]
    method = f'damped Newton at {dps} digits on the README log density, from the fitted values'
    closed = {}
    evidence = {}
    with mp.workdps(dps + 10):
        xs = sorted(mp.mpf(v) for v in golden['data'])
        if id == 'laplace':
            lower, upper = xs[(len(xs)-1)//2], xs[len(xs)//2]
            mu = mp.mpf(theta[0]) if 'mu' in held_names else upper
            closed['mu'] = mu
            if 'sigma' not in held_names:
                closed['sigma'] = mp.fsum(abs(v-mu) for v in xs)/len(xs)
                if closed['sigma'] <= 0:
                    raise ValueError('Laplace scale collapse: no finite MLE')
            evidence['median_interval'] = [mp.nstr(v, 25) for v in (lower, upper)]
            method = 'closed form: median interval upper endpoint and mean absolute deviation at 50 digits'
        if id in ('unif', 'dunif', 'pareto', 'power', 'hnorm', 'zipf'):
            for p in params:
                name = p['name']
                if name in held_names:
                    continue
                if p.get('bound'):
                    closed[name] = xs[0] if p['bound'] == 'lower' else xs[-1]
                elif id == 'dunif':
                    closed[name] = xs[0] if name == 'min' else xs[-1]
                elif id == 'zipf' and name == 'N':
                    closed[name] = xs[-1]
            if closed:
                method = 'sample-extreme closed form (support monotonicity); ' + method
        # Finite population designs permit exhaustive integer enumeration. The
        # binomial size cases need a separate tail argument, not an arbitrary cap.
        if id == 'dlaplace' and 'location' not in held_names:
            closed['location'] = xs[(len(xs)-1)//2]
            evidence['integer_enumeration'] = {'coefficient': 'location',
                'median_interval': [int(xs[(len(xs)-1)//2]), int(xs[len(xs)//2])],
                'argument': 'absolute-deviation convexity; all integer medians maximize'}
            method = 'integer median closed form; ' + method
        if id in ('binom', 'ztbinom', 'zib', 'zmbinom') and 'size' not in held_names:
            from quality.reference_integer import integer_profile
            checked, evidence['integer_enumeration'] = integer_profile(id, golden)
            closed['size'] = checked['size']
            method = 'exhaustive integer enumeration with Poisson tail bound at 50 digits; ' + method
        if id in ('hyper', 'nhyper') and 'm' not in held_names:
            N = int(values['N'])
            fn = sp.lambdify([*syms, x], expr, 'mpmath', cse=True)
            candidates = []
            for m in range(N+1):
                t = [mp.mpf(v) for v in theta]
                t[0] = m
                # nhyper observations count total draws up to the r-th success,
                # so r<=m and x-r<=N-m for every observation.
                feasible = (xs[-1] <= m <= N-values['k']+xs[0] if id == 'hyper'
                            else values['r'] <= m <= N-xs[-1]+values['r'])
                if feasible:
                    candidates.append((mp.fsum(fn(*t, v) for v in xs), m))
            if not candidates:
                raise ValueError('empty feasible integer design')
            best = max(candidates)
            closed['m'] = mp.mpf(best[1])
            evidence['integer_enumeration'] = {'coefficient': 'm', 'feasible_cells': len(candidates),
                'range': [min(v[1] for v in candidates), max(v[1] for v in candidates)],
                'runner_up_gap': mp.nstr(best[0]-sorted(candidates)[-2][0], 25) if len(candidates)>1 else None}
            method = 'exhaustive finite integer enumeration at 50 digits; ' + method
    for i, p in enumerate(params):
        if p['name'] in closed:
            theta[i] = closed[p['name']]
    not_refined = {}
    for i, p in enumerate(params):
        if p['name'] in held_names:
            not_refined[p['name']] = 'held'
        elif p['name'] in closed:
            not_refined[p['name']] = 'closed form or exhaustive enumeration (independently checked)'
        elif p.get('integer'):
            not_refined[p['name']] = 'integer'
        elif p.get('bound'):
            not_refined[p['name']] = 'support bound'
        elif p.get('kink'):
            not_refined[p['name']] = 'kink'
        else:
            lo, hi = domains[i]
            if theta[i] < lo or theta[i] > hi:
                raise ValueError(f'{p["name"]} outside the fitting domain')
            if theta[i] == lo or theta[i] == hi:
                not_refined[p['name']] = 'at domain bound'
    free = [i for i, p in enumerate(params) if p['name'] not in not_refined]
    held = {syms[i]: sp.Integer(int(theta[i])) if params[i].get('integer') else (sp.Rational(float(theta[i])) if mp.isfinite(theta[i]) else sp.sympify(float(theta[i]))) if params[i]['name'] in held_names else sp.Float(theta[i], dps+10)
            for i in range(len(params)) if i not in free}
    e = expr.subs(held).doit()
    e = e.replace(sp.Abs, lambda a: sp.Piecewise((a, a >= 0), (-a, True)))
    fs = [syms[i] for i in free]
    data = golden['data']
    out = {'id': id, 'method': method, **evidence, 'free': [params[i]['name'] for i in free], 'not_refined': not_refined}
    with mp.workdps(dps + 10):
        xs = [mp.mpf(v) for v in data]
        # Check the actual joint domain and data support, not only a box
        # formed by substituting the starting values into dependent bounds.
        def parse_bound(v):
            v = str(v)
            for p in sorted(params, key=lambda p: len(p['name']), reverse=True):
                v = re.sub(r'\b'+p['name']+r'\b', 'p_'+p['name'], v)
            return sp.sympify(v, locals={**{'p_'+p['name']:s for p,s in zip(params,syms)}, 'inf':sp.oo})
        domain_fns = [[sp.lambdify(syms, parse_bound(v.strip()), 'mpmath')
                       for v in p['domain'][1:-1].split(',')] for p in params]
        support_fns = [sp.lambdify(syms, parse_bound(r_bound_to_sympy(str(meta['support'][key]))), 'mpmath')
                       for key in ('lower','upper')]
        def feasible(t):
            all_t = full(t)
            for i, ((lo_fn, hi_fn), p) in enumerate(zip(domain_fns, params)):
                lo, hi = lo_fn(*all_t), hi_fn(*all_t)
                v = all_t[i]
                if v < lo or v > hi or (v == lo and p['domain'][0]=='(') or (v == hi and p['domain'][-1]==')'):
                    return False
            lo, hi = [fn(*all_t) for fn in support_fns]
            return all(lo<=xi<=hi for xi in xs)
        f = sp.lambdify([*fs, x], e, 'mpmath', cse=True)
        g = [sp.lambdify([*fs, x], sp.diff(e, s), 'mpmath', cse=True) for s in fs]
        h = [[sp.lambdify([*fs, x], sp.diff(e, a, b), 'mpmath', cse=True) for b in fs] for a in fs]
        full = lambda t: [theta[i] if i not in free else t[free.index(i)] for i in range(len(params))]

        def ll(t):
            try:
                if not feasible(t):
                    return None
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
            if iterations == 199 and size >= mp.mpf(10) ** (-dps):
                out['error'] = 'Newton budget exhausted without convergence'
                return out
            if not definite or mp.sqrt(mp.fsum(v ** 2 for v in G)) > mp.mpf(10) ** (-dps+5):
                out['error'] = 'unchecked stationary point (score or information)'
                return out
            out['score_norm'] = mp.nstr(mp.sqrt(mp.fsum(v ** 2 for v in G)), 3)
            out['information_negative_definite'] = definite
        # A bound-active coefficient needs the appropriate one-sided score.
        active_scores = {}
        for i, p in enumerate(params):
            if not_refined.get(p['name']) != 'at domain bound':
                continue
            integer_values = {syms[j]: sp.Integer(int(theta[j]))
                              for j,pj in enumerate(params) if pj.get('integer')}
            derivative = sp.diff(expr, syms[i]).subs(integer_values).doit()
            df = sp.lambdify([*syms, x], derivative, 'mpmath', cse=True)
            score = mp.fsum(df(*full(t), xi) for xi in xs)
            side = 'lower' if theta[i] == domains[i][0] else 'upper'
            if not (score <= 0 if side == 'lower' else score >= 0):
                out['error'] = 'one-sided score violates bound optimality'
                return out
            active_scores[p['name']] = {'side': side, 'score': mp.nstr(score,25)}
        if active_scores:
            out['one_sided'] = active_scores
        out['iterations'] = iterations
        ref = full(t)
        out['estimates'] = {p['name']: mp.nstr(mp.mpf(v), 25) for p, v in zip(params, ref)}
        out['logLik'] = mp.nstr(cur, 25)
        out['relative_difference'] = {
            p['name']: (0.0 if mp.mpf(values[p['name']]) == ref[i] else
                        float(abs((mp.mpf(values[p['name']]) - ref[i]) / ref[i])) if ref[i] != 0 else float(abs(values[p['name']])))
            for i, p in enumerate(params) if p['name'] not in held_names}
        out['logLik_difference'] = float(golden['logLik'] - cur)
    existence = meta.get('existence', {}) or {}
    out['global'] = ({'kind': 'dossier optimality/uniqueness proof',
                      'statement': existence.get('statement', ''),
                      'reason': existence.get('reason', '')}
                     if existence.get('uniqueness') == 'proved' else 'local only')
    out['input_sha256'] = hashlib.sha256(json.dumps(
        {'data': data, 'log_density': meta['log_density'], 'held': {k: mp.nstr(theta[i], dps+10) for i, k in
         enumerate(p['name'] for p in params) if k in not_refined}},
        sort_keys=True).encode()).hexdigest()
    return out


def rescore_oracle(id, ref, oracle, dps=DPS):
    """Check an apparent float-oracle improvement with the declared density."""
    folder = DISTS / id
    meta = _meta(folder)
    golden = json.loads((folder / 'golden.json').read_text())
    params = meta['params']
    values = rounded_estimates(id, ref)
    domains = [interval(p['domain'], values) for p in params]
    syms, x, expr = density_expression(folder, meta, domains)
    integers = {s: int(values[p['name']]) for p,s in zip(params,syms) if p.get('integer')}
    regular = [(p,s) for p,s in zip(params,syms) if not p.get('integer')]
    f = sp.lambdify([*[s for p,s in regular], x], expr.subs(integers).doit(), 'mpmath', cse=True)
    with mp.workdps(dps + 10):
        xs = [mp.mpf(v) for v in golden['data']]
        def total(theta):
            t = [mp.mpf(theta[p['name']]) for p,s in regular]
            value = mp.fsum(f(*t,xi) for xi in xs)
            if not mp.isfinite(value) or mp.im(value) != 0:
                raise ValueError('nonfinite high-precision oracle likelihood')
            return value
        ll_ref = total(ref['estimates'])
        ll_oracle = total(oracle['estimates'])
        margin = mp.mpf('1e-40') * (1 + abs(ll_ref))
        return {'precision_digits': dps,
                'gap': mp.nstr(ll_oracle-ll_ref,25),
                'comparison_margin': mp.nstr(margin,3)}


def rounded_estimates(id, ref):
    integer = {p['name'] for p in _meta(DISTS/id)['params'] if p.get('integer')}
    return {k: int(mp.mpf(v)) if k in integer else
            float(v) if mp.isfinite(mp.mpf(v)) else ('Inf' if mp.mpf(v)>0 else '-Inf')
            for k,v in ref['estimates'].items()}


def update_golden_text(original, updates):
    """Replace top-level values only, preserving frozen input text byte for byte."""
    decoder = json.JSONDecoder()
    pos = original.index('{') + 1
    replacements = []
    pending = dict(updates)
    while True:
        while original[pos].isspace() or original[pos] == ',':
            pos += 1
        if original[pos] == '}':
            break
        key, pos = decoder.raw_decode(original, pos)
        pos = original.index(':', pos) + 1
        while original[pos].isspace():
            pos += 1
        start = pos
        _, pos = decoder.raw_decode(original, pos)
        if key in pending:
            value = json.dumps(pending.pop(key), indent=2, allow_nan=False).replace('\n', '\n  ')
            replacements.append((start, pos, value))
    for start, end, value in reversed(replacements):
        original = original[:start] + value + original[end:]
    for key, value in pending.items():
        encoded = json.dumps(value, indent=2, allow_nan=False).replace('\n', '\n  ')
        original = original.rstrip()[:-1].rstrip() + ',\n  ' + json.dumps(key) + ': ' + encoded + '\n}\n'
    return original


def write(id, ref):
    path = DISTS / id / 'golden.json'
    golden = json.loads(path.read_text())
    golden['estimates'] = rounded_estimates(id, ref)
    golden['logLik'] = float(ref['logLik'])
    golden['reference'] = {
        'method': ref['method'],
        'precision_digits': DPS,
        **{k: ref[k] for k in ('estimates', 'logLik', 'free', 'not_refined', 'score_norm',
                               'information_negative_definite', 'global', 'input_sha256', 'median_interval',
                               'integer_enumeration', 'one_sided') if k in ref},
    }
    tmp = path.with_suffix('.json.tmp')
    tmp.write_text(update_golden_text(path.read_text(),
        {k:golden[k] for k in ('estimates','logLik','reference')}))
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
            try:
                if r['global'] == 'local only':
                    candidate = json.loads((DISTS / id / 'golden.json').read_text())
                    candidate['estimates'] = rounded_estimates(id, r)
                    candidate['logLik'] = float(r['logLik'])
                    oracle = check(id, golden=candidate)['oracle']
                    r['global'] = {'kind': 'numerical multistart, not a proof', 'starts': 0, **oracle}
                    if oracle['status'] == 'fail':
                        raise ValueError(f'oracle disagreement: {oracle}')
                    if oracle.get('gap',0) > 1e-12*(1+abs(float(r['logLik']))):
                        verified = rescore_oracle(id,r,oracle)
                        r['global']['high_precision_check'] = verified
                        if mp.mpf(verified['gap']) > mp.mpf(verified['comparison_margin']):
                            raise ValueError(f'high-precision oracle improvement: {verified}')
                        r['global']['rounding_note'] = 'apparent float improvement vanishes on high-precision rescoring'
                write(id, r)
            except Exception as exc:
                failures += 1
                print(f'{id:14s} ERROR reference not written: {exc}', flush=True)
    return 1 if failures else 0
