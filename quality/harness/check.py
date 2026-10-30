import re
"""Check real fitter goldens against symbolic README densities."""
import argparse
import json
import warnings
import time
from pathlib import Path
import mpmath as mp
import numpy as np
import sympy as sp
import yaml
from quality.density import density_expression
from scipy.optimize import minimize
from scipy.special import expit, polygamma
ROOT = Path(__file__).resolve().parents[2]
DISTS = ROOT / 'cppunivariate/include/cppunivariate/distributions'


def r_bound_to_sympy(expr):
    """Translate an R support bound (README `support:`) to sympy syntax.

    Bounds may be R conditionals, `if (xi > 0) mu - sigma/xi else -Inf`,
    possibly chained with `else if`; they become `Piecewise`. `Inf` becomes
    `oo` and `a == b` becomes `Eq(a, b)`.
    """
    s = expr.strip()
    if s.startswith("if"):
        i = s.index("(")
        depth = 0
        for j in range(i, len(s)):
            depth += {"(": 1, ")": -1}.get(s[j], 0)
            if depth == 0:
                break
        cond, rest = s[i + 1:j], s[j + 1:]
        k = rest.index(" else ")
        then, other = rest[:k], rest[k + len(" else "):]
        cond = re.sub(r"(.+?)\s*==\s*(.+)", r"Eq(\1, \2)", cond)
        return f"Piecewise(({r_bound_to_sympy(then)}, {cond}), ({r_bound_to_sympy(other)}, True))"
    return re.sub(r"\bInf\b", "oo", s)

def interval(domain, values):
    return tuple((float(sp.sympify(v.strip(), locals={**values, 'inf': sp.oo})) for v in domain[1:-1].split(',')))

def check(id):
    folder = DISTS / id
    meta = yaml.safe_load((folder / 'README.md').read_text().split('---', 2)[1])
    golden = json.loads((folder / 'golden.json').read_text())
    params, values = (meta['params'], {k: float(v) if isinstance(v, str) else v for k, v in golden['estimates'].items()})
    domains = [interval(p['domain'], values) for p in params]
    fixed_names = set(golden.get('fixed', [])) | {
        p['name'] for p in params
        if golden.get('fit_args', {}).get(p['name']) is not None}
    # R5 bounds constrain free coefficients, with P0's sufficient held-set
    # exemptions. The reference-domain checks remain independent below.
    fit_bounds = {}
    for i, p in enumerate(params):
        rules = p.get('fit_domain', [])
        if isinstance(rules, dict):
            rules = [rules]
        if p['name'] in fixed_names:
            continue
        lo, hi = domains[i]
        for rule in rules:
            exempt = rule['restriction'].get('exempt_when_held', [])
            # Before C5, ht has an implicit held threshold mu = 0.
            effective_holds = fixed_names | ({'mu'} if id == 'ht' else set())
            if any(set(h).issubset(effective_holds) for h in exempt):
                continue
            lo = max(lo, float(rule.get('lower', -np.inf)))
            hi = min(hi, float(rule.get('upper', np.inf)))
            fit_bounds[i] = (lo, hi)
        domains[i] = (lo, hi)
    syms, x, expr = density_expression(folder, meta, domains)
    theta = [values[p['name']] for p in params]
    # Held: support endpoints, integer parameters, and parameters the golden
    # fit held fixed (golden.json `fixed`).
    held = {syms[i]: theta[i] for i, p in enumerate(params) if p.get('bound') or p.get('integer') or p['name'] in fixed_names}
    expr = expr.subs(held).doit()
    expr = expr.replace(sp.Abs, lambda a: sp.Piecewise((a, a >= 0), (-a, True)))
    if expr.free_symbols - set(syms) - {x}:
        raise ValueError(f'undeclared symbols: {expr.free_symbols - set(syms) - {x}}')
    data = np.asarray(golden['data'], dtype=float)
    boundary_offsets = {}
    for i, p in enumerate(params):
        if p.get('bound'):
            extreme = min(data) if p['bound'] == 'lower' else max(data)
            boundary_offsets[p['name']] = theta[i] - extreme
            # Some fitters nudge an open endpoint by sqrt(machine epsilon).
            if abs(theta[i] - extreme) > np.sqrt(np.finfo(float).eps) * (1 + abs(extreme)):
                raise ValueError(f"{p['name']}: bound estimate differs from sample extreme")
    result = {'id': id, 'boundary_offsets': boundary_offsets, 'limits': meta.get('limits', []), 'skipped_parameters': {p['name']: {k: p[k] for k in ('bound', 'integer', 'fixed') if k in p} or {'fixed': True} for i, p in enumerate(params) if syms[i] in held}}
    fn = sp.lambdify([*syms, x], expr, 'mpmath', cse=True)
    with mp.workdps(30):
        ll = float(mp.fsum((fn(*map(mp.mpf, theta), mp.mpf(v)) for v in data)))
    reported = golden['logLik']
    if not np.isfinite(ll):
        raise ValueError('nonfinite symbolic logLik')
    result['logLik'] = {'status': 'pass' if abs(ll - reported) <= 1e-09 * (1 + abs(reported)) else 'fail', 'symbolic': ll, 'golden': reported, 'difference': ll - reported}
    oracle_regular = [i for i in range(len(params)) if syms[i] not in held]
    regular = [i for i in oracle_regular if not params[i].get('kink')]
    result['derivative_held_parameters'] = [p['name'] for p in params if p.get('kink')]
    active = {}
    for i in regular:
        lo, hi = domains[i]
        if (params[i]['domain'][0] == '[' or i in fit_bounds) and theta[i] == lo:
            active[i] = 'lower'
        elif (params[i]['domain'][-1] == ']' or i in fit_bounds) and theta[i] == hi:
            active[i] = 'upper'
    smooth = [i for i in regular if i not in active]

    def total(e):
        """Scalar branches avoid invalid powers in inactive Piecewise arms."""
        f = sp.lambdify([*syms, x], e, [{'polygamma': polygamma}, 'math'], cse=False)
        return float(sum((f(*theta, float(v)) for v in data)))
    try:
        if any((meta['log_density'].find('Abs') >= 0 and np.any(np.isclose(data, theta[i], rtol=0, atol=1e-13)) for i in regular if params[i]['role'] == 'location')):
            raise ValueError('nonsmooth location at an observation; observed Hessian undefined')
        g = {i: total(sp.diff(expr, syms[i])) for i in regular}
        H = np.array([[-total(sp.diff(expr, syms[i], syms[j]).replace(sp.DiracDelta, lambda *a: sp.S.Zero)) for j in smooth] for i in smooth])
        if not all((np.isfinite(v) for v in g.values())) or not np.all(np.isfinite(H)):
            raise ValueError('nonfinite score or observed information')
        if smooth:
            np.linalg.cholesky(H)
            standard = max((abs(g[i]) * np.sqrt(np.linalg.inv(H)[j, j]) for j, i in enumerate(smooth)))
        else:
            standard = 0.0
        ok = standard <= 0.001 and all((g[i] <= 1e-08 if side == 'lower' else g[i] >= -1e-08 for i, side in active.items()))
        result['score_hessian'] = {'status': 'pass' if ok else 'fail', 'standardised_score': standard, 'score': {params[i]['name']: v for i, v in g.items()}, 'information': H.tolist(), 'one_sided': {params[i]['name']: side for i, side in active.items()}}
    except (ValueError, TypeError, NameError, np.linalg.LinAlgError) as e:
        result['score_hessian'] = {'status': 'skipped' if 'nonsmooth' in str(e) else 'fail', 'reason': str(e)}
    if not regular:
        result['score_hessian'] = {'status': 'skipped', 'reason': 'no regular differentiable parameters'}
    regular = oracle_regular
    if not regular:
        result['score_hessian'] = {'status': 'skipped', 'reason': 'no regular parameters'}
        result['oracle'] = {'status': 'skipped', 'gap': 0.0, 'reason': 'no regular parameters'}
        return result
    f = sp.lambdify([*syms, x], expr, 'scipy', cse=True)

    def encode(t):
        z = []
        for i in regular:
            lo, hi = domains[i]
            v = t[i]
            if np.isfinite(lo) and np.isfinite(hi):
                q = np.clip((v - lo) / (hi - lo), 1e-08, 1 - 1e-08)
                z.append(np.log(q / (1 - q)))
            elif np.isfinite(lo):
                z.append(np.log(max(v - lo, 1e-08)))
            elif np.isfinite(hi):
                z.append(np.log(max(hi - v, 1e-08)))
            else:
                z.append(v)
        return np.array(z)

    def decode(z):
        t = list(theta)
        for i, v in zip(regular, z):
            lo, hi = domains[i]
            if np.isfinite(lo) and np.isfinite(hi):
                t[i] = lo + (hi - lo) * expit(v)
            elif np.isfinite(lo):
                t[i] = lo + np.exp(v)
            elif np.isfinite(hi):
                t[i] = hi - np.exp(v)
            else:
                t[i] = v
        return t
    # Domains can depend on other fitted coordinates (a <= c <= b).
    # Keep the transform's numerical bracket, but validate the actual joint
    # domain at every trial rather than the golden's substituted endpoints.
    # Coefficient names can be Python keywords (Tukey `lambda`): parse them
    # as `p_<name>`, as quality/density.py does for the log density.
    names = sorted((p['name'] for p in params), key=len, reverse=True)
    safe = {'p_' + p['name']: s for p, s in zip(params, syms)}
    def parse(v):
        for n in names:
            v = re.sub(r'\b' + n + r'\b', 'p_' + n, v)
        return sp.sympify(v, locals={**safe, 'inf': sp.oo})
    domain_fns = [[sp.lambdify(syms, parse(v.strip()), 'numpy')
                   for v in p['domain'][1:-1].split(',')] for p in params]
    support_fns = [sp.lambdify(syms, parse(r_bound_to_sympy(str(meta['support'][key]))), 'numpy') for key in ('lower', 'upper')]

    def objective(z):
        try:
            t = decode(z)
            if any((not domains[i][0] < t[i] < domains[i][1] for i in regular)):
                return 1e+100
            for i, (lo_fn, hi_fn) in enumerate(domain_fns):
                lo, hi = lo_fn(*t), hi_fn(*t)
                if (t[i] < lo or t[i] > hi or
                    (t[i] == lo and params[i]['domain'][0] == '(') or
                    (t[i] == hi and params[i]['domain'][-1] == ')')):
                    return 1e+100
            for bound_fn, op in zip(support_fns, (np.greater_equal, np.less_equal)):
                if not np.all(op(data, bound_fn(*t))):
                    return 1e+100
            y = float(np.sum(np.broadcast_to(f(*t, data), data.shape)))
            return -y if np.isfinite(y) else 1e+100
        except (ValueError, OverflowError, ZeroDivisionError, TypeError):
            return 1e+100
    rng = np.random.default_rng(20261028)
    z = encode(theta)
    starts = [z, encode([p['simulate'] for p in params])] + [z + rng.normal(0, 1, len(z)) for _ in range(6)]
    best = -ll
    best_theta = theta
    feasible = 0
    with warnings.catch_warnings(), np.errstate(all='ignore'):
        warnings.simplefilter('ignore')
        for start in starts:
            nm = minimize(objective, start, method='Nelder-Mead', options={'maxiter': 700, 'xatol': 1e-08, 'fatol': 1e-09})
            bf = minimize(objective, nm.x, method='BFGS', options={'maxiter': 200, 'gtol': 1e-07})
            for fit in (nm, bf):
                if np.isfinite(fit.fun) and fit.fun < 1e+99:
                    feasible += 1
                if fit.fun < best:
                    best = float(fit.fun)
                    best_theta = decode(fit.x)
    gap = -best - reported
    result['oracle'] = {'status': 'pass' if feasible and gap <= 1e-06 * (1 + abs(reported)) else 'fail', 'gap': gap, 'logLik': -best, 'estimates': {p['name']: v if np.isfinite(v) else str(v) for p, v in zip(params, best_theta)}, 'starts': len(starts), 'feasible_results': feasible}
    return result

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('command', choices=['check'])
    parser.add_argument('ids', nargs='*')
    args = parser.parse_args()
    out = ROOT / 'build/quality'
    out.mkdir(parents=True, exist_ok=True)
    results = []
    for id in args.ids or sorted((p.parent.name for p in DISTS.glob('*/README.md'))):
        started = time.monotonic()
        try:
            r = check(id)
        except Exception as e:
            r = {'id': id, 'error': f'{type(e).__name__}: {e}'}
        r['seconds'] = time.monotonic() - started
        r['passed'] = 'error' not in r and all((r[k]['status'] != 'fail' for k in ('logLik', 'score_hessian', 'oracle')))
        results.append(r)
        (out / f'{id}.json').write_text(json.dumps(r, indent=2, allow_nan=False) + '\n')
        print(id, 'pass' if r['passed'] else 'FAIL', r.get('error', ''), flush=True)
    lines = ['| id | logLik | score/Hessian | oracle | gap | skipped parameters / reason |', '|---|---|---|---|---:|---|']
    for r in results:
        cells = [r['id']] + [r.get(k, {}).get('status', 'unchecked') for k in ('logLik', 'score_hessian', 'oracle')] + [f"{r['oracle']['gap']:.3g}" if 'oracle' in r else '', r.get('error', str(r.get('skipped_parameters', {})) + ' ' + r.get('score_hessian', {}).get('reason', ''))]
        lines.append('| ' + ' | '.join(cells) + ' |')
    lines += ['', 'Limits are listed, not executed; folder tests own status checks.', ''] + [f"- {r['id']}: {json.dumps(r.get('limits', []))}" for r in results]
    (out / 'report.md').write_text('\n'.join(lines) + '\n')
    return int(any((not r['passed'] for r in results)))
