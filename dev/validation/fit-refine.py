#!/usr/bin/env python3
"""Strict layer: an estimate is within tol of a high-precision refinement.

For the golden sample of every family and modifier route, and for a seeded
sample of fit-audit archive cases (at most ten with every coefficient free
and ten with exactly one held, per family and route, drawn from the cases
whose result in the stored baseline of master c167a975 is an estimate), this
refines the returned estimate at 50 significant digits with the dossier's
symbolic log density, using dev/validation/quality/reference.py unchanged:
damped Newton in the free regular coefficients from the returned values,
closed forms or exhaustive enumeration for support-bound, Laplace,
discrete-Laplace, binomial-size and hypergeometric coordinates, a one-sided
score check for a coefficient on a domain bound. Each case is given to
reference() as a golden.json in a shadow copy of the family folder. Where
reference() stops on scale alone (its absolute score threshold, a pivot test
on a badly scaled Hessian), a fallback runs the same Newton in coordinates
scaled by each coefficient's size, at 50, 100 or 200 digits, and accepts a
relative Newton step below 10^-(digits-10) with negative definite scaled
information.

Then the accuracy contract (dev/design/accuracy-contract.md) is checked for
every free coefficient the refinement determined:

  |theta_hat - theta_star| <= TOL * max(|theta_star|, s)

with TOL = 1e-10 (every snapshot fit uses default controls) and s the floor
for coefficients that can be zero or change sign: the fitted scale for a
location, 1 for another coefficient whose domain reaches zero, none (s = 0)
for a strictly positive coefficient. Coefficients the
refinement leaves alone (integer, kink, support bound without closed form,
held) are listed as not refined. A case whose refinement stops on an applicability check, singular information
or a work limit is open. Unexpected reference exceptions are reported by case
and fail the checker independently of the fitter failure ratchet.

References are cached in dev/validation/strict-refine.jsonl.gz, keyed by
family, data, held values, density source and any fitted coordinates on
which a profile conditions.
--cache-only judges from the cache alone and lists cases without an entry.
The CPU budget of a full refinement is two hours on one slot: goldens come
first, then archive cases round robin across families; a family stops after
FAMILY_ALLOWANCE CPU seconds of archive refinements or two cases that hit the
per-case guard (--case-limit, wall seconds), and nothing new starts once the
cache holds TOTAL_BUDGET CPU seconds. Cases not attempted are listed as open
with the reason and are not cached.

Usage:
  fit-refine.py NATIVE [DOUBLE] [--cache-only] [--family a,b] [--jobs N] [--prune | --add REF [--match RE]]
Failures are keyed 'refine|mode|family|source|case'.
"""
import argparse, collections, gzip, hashlib, json, math, random, re, shutil, signal, sys, time
from contextlib import ExitStack
from concurrent.futures import ProcessPoolExecutor
from functools import lru_cache
from pathlib import Path
import yaml
from strict_common import ROOT, HERE, add_arguments, load, load_one, mode_of, number, ratchet, snapshot_module

sys.set_int_max_str_digits(0)   # exhaustive hypergeometric enumeration prints large integers
from refine_profiles import nhyper_composition, series_preflight, triang_endpoint, zmbinom_profile
TOL = 1e-10
PER_KIND = 10
CHECK = 'refine'
CACHE = HERE/'strict-refine.jsonl.gz'
BASELINE = ROOT/'dev/research/evaluations/2026-11-13-fit-snapshot-data'
WORK = ROOT/'build/strict/refine'
VERSION = 'reference.py@dps50, scaled fallback v3'
FAMILY_ALLOWANCE = 150    # CPU seconds of archive refinements per family (the golden is not counted)
TOTAL_BUDGET = 7000       # CPU seconds over the whole cache: the job's two CPU-hours
# Documented, deliberate ValueErrors from the unchanged shared integer oracle.
INTEGER_OPENS = ('integer profile with held probability requires a separate tail envelope',
                 'integer enumeration budget exhausted:', 'integer profile root budget exhausted',
                 'integer winner does not beat the Poisson tail envelope')


class CaseGuard(Exception):
    pass


def reference_engine(ident, folder, meta, golden, data, values, hold):
    from quality import reference as qr
    if ident == 'triang' and values['c'] in (values['a'], values['b']):
        return triang_endpoint(data, values, hold)
    if ident == 'zmbinom':
        return zmbinom_profile(data, values, hold)
    if ident == 'nhyper' and 'm' not in hold:
        return nhyper_composition(data, values, hold)
    reason = series_preflight(ident, data, values)
    if reason:
        return {'error': reason}
    try:
        out = qr.reference(ident)
    except ValueError as e:
        if ident == 'binom' and str(e).startswith(INTEGER_OPENS):
            return {'error': str(e)}
        raise
    if out.get('error', '').startswith(FALLBACK):
        first = out['error']
        for dps in (50, 100, 200):
            out = scaled_newton(folder, meta, golden, dps)
            if 'error' not in out:
                break
        out['first_method'] = 'reference.py: '+first
    return out


def profile_coordinates(ident, values, hold):
    """Coordinates copied from a fit that affect its independent reference."""
    if ident == 'triang':
        mode = values['c']
        face = 'lower' if mode == values['a'] else 'upper' if mode == values['b'] else 'interior'
        return face, float(mode).hex()
    if ident == 'zmbinom':
        return (float(values['size']).hex(),)
    if ident == 'nhyper' and 'm' not in hold:
        # m only breaks an exact composition tie.
        return tuple(float(values[k]).hex() for k in ('N', 'r', 'm'))
    return ()


def load_cache():
    if not CACHE.exists():
        return {}, {}
    cache, stale = {}, {}
    with gzip.open(CACHE, 'rt') as source:
        for line in source:
            ref = json.loads(line)
            if ref.get('version') != VERSION:
                continue
            error = ref.get('error', '')
            if not ref.get('profile_revision'):
                family = ref['family']
                composition = ref.get('not_refined', {}).get('m', '').startswith('closed form')
                if (family in ('binom+zero=modified', 'triang')
                        or (family in ('cmp', 'dgamma') and error.startswith('AssertionError:'))
                        or (family == 'nhyper' and (composition or error.startswith('ValueError: Exceeds the limit')))):
                    stale[ref['key']] = ref
                    continue
                if error.startswith('ValueError: ') and error[12:].startswith(INTEGER_OPENS):
                    ref['error'] = error[12:]
                elif re.match(r'^[A-Z]\w*(?:Error|Exception|Interrupt):', error):
                    ref['exception'] = ref.pop('error')
            cache[ref['key']] = ref
    return cache, stale

fs = snapshot_module()

def route_ids():
    return {route['id']: ident for ident, route in fs.ROUTES.items()}

@lru_cache(maxsize=None)
def law_meta(ident):
    """Dossier metadata as reference() reads it; a modifier law's from the route table."""
    if ident in fs.ROUTES:
        meta = json.loads(json.dumps(fs.ROUTES[ident]['meta']))
        if ident == 'zipf':   # the zeta window admits every real exponent (zeta dossier, R notes)
            for p in meta['params']:
                if p['name'] == 's': p['domain'] = '(-inf, inf)'
        return meta
    return yaml.safe_load((fs.DISTS/ident/'README.md').read_text().split('---', 2)[1])

@lru_cache(maxsize=None)
def density_source(ident):
    folder = fs.DISTS/ident
    meta = law_meta(ident)
    spec = (folder/'spec.py').read_text() if (folder/'spec.py').exists() else ''
    return json.dumps([meta['log_density'], meta['params'], meta['support'], spec], sort_keys=True)

def selection(archive):
    """Deterministic case list: goldens plus a seeded archive sample, independent of the judged snapshot."""
    base = {}
    for name in ('native', 'routes-native'):
        base.update(load_one(BASELINE/(name+'.jsonl.gz')))
    families = sorted({k[0] for k in base})
    picks = [(family, 'golden', 'golden') for family in families]
    sample = {}
    for family in families:
        ok = [k for k in base if k[0] == family and k[1] == 'archive' and base[k]['result']['kind'] == 'ok']
        free = sorted(k for k in ok if base[k]['mask'] == 0)
        one = sorted(k for k in ok if bin(base[k]['mask']).count('1') == 1)
        rng = random.Random('strict-refine:'+family)
        a = rng.sample(free, min(PER_KIND, len(free))); b = rng.sample(one, min(PER_KIND, len(one)))
        # fill to twenty from the other kind when one kind is short
        if len(a) < PER_KIND: b += rng.sample(sorted(set(one)-set(b)), min(PER_KIND-len(a), len(one)-len(b)))
        if len(b) < PER_KIND: a += rng.sample(sorted(set(free)-set(a)), min(PER_KIND-len(b), len(free)-len(a)))
        sample[family] = sorted(a+b)
    for k in range(2*PER_KIND):   # round robin, so a budget cut leaves every family some cases
        picks += [sample[f][k] for f in families if k < len(sample[f])]
    return picks

def rebuild(family, archive):
    routes = route_ids()
    ident = routes.get(family, family)
    fs.ROUTE_MODE = family in routes
    try:
        meta, _ = fs.read(ident)
        names = [p['name'] for p in meta['params']]
        return ident, {(c['source'], c['case']): (c, hashlib.sha256(fs.line(c, names).encode()).hexdigest()[:16])
                       for c in fs.cases(ident, archive)}
    finally:
        fs.ROUTE_MODE = False

def law_values(ident, meta, r, data):
    """Start values in the law's coordinates, and the names to hold for the refinement."""
    estimates = {k: number(v) for k, v in r['result']['estimates'].items()}
    if ident == 'zmlogarithmic' and 'theta' in estimates: estimates['prob'] = estimates.pop('theta')
    held = {('prob' if ident == 'zmlogarithmic' and k == 'theta' else k): v for k, v in r['held'].items()}
    names = [p['name'] for p in meta['params']]
    values = dict(held); values.update(estimates)
    hold = set(held)
    if ident in fs.ROUTES:   # window bounds are data-determined designs of the route, never refined
        lower, upper = min(data), max(data)
        if ident in ('tbinom', 'tpois'):
            lo = max(0., math.floor(held['a'])+1) if 'a' in held else lower
            hi = math.floor(held['b']) if 'b' in held else upper
            values.update(a=max(-1., math.ceil(lo)-1), b=math.floor(hi)); hold |= {'a', 'b'}
        elif ident == 'tnorm':
            values.setdefault('a', held.get('a', lower)); values.setdefault('b', held.get('b', upper)); hold |= {'a', 'b'}
        elif ident == 'zipf':
            values.setdefault('N', held.get('N', upper))
    missing = [n for n in names if n not in values]
    return values, hold, missing

def refine(ident, meta, data, values, hold, case_limit):
    """Refine a case in a shadow folder and report its reference or the reason it stopped."""
    from quality import reference as qr

    def guard(*_):
        raise CaseGuard('case guard of %d s' % case_limit)

    old = signal.signal(signal.SIGALRM, guard)
    saved = qr.DISTS
    started = time.process_time()
    shadow = None
    try:
        signal.alarm(case_limit)
        key = hashlib.sha256(json.dumps([ident, data, sorted(values.items()), sorted(hold)]).encode()).hexdigest()[:20]
        shadow = WORK/'shadow'/key
        folder = shadow/ident
        folder.mkdir(parents=True, exist_ok=True)
        real = fs.DISTS/ident
        if ident in fs.ROUTES:
            (folder/'README.md').write_text('---\n'+yaml.safe_dump(meta, sort_keys=False)+'---\n')
        else:
            shutil.copyfile(real/'README.md', folder/'README.md')
            if (real/'spec.py').exists():
                shutil.copyfile(real/'spec.py', folder/'spec.py')
        def encode(v):
            return v if math.isfinite(v) else ('Inf' if v > 0 else '-Inf')
        golden = dict(data=data, estimates={k: encode(v) for k, v in values.items()},
                      fit_args={k: encode(values[k]) for k in hold}, logLik=0.0)
        (folder/'golden.json').write_text(json.dumps(golden))
        qr.DISTS = shadow
        out = reference_engine(ident, folder, meta, golden, data, values, hold)
    except CaseGuard as e:
        out = {'error': str(e)}
    except Exception as e:
        out = {'exception': '%s: %s' % (type(e).__name__, str(e)[:200])}
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, old)
        qr.DISTS = saved
        if shadow is not None:
            shutil.rmtree(shadow, ignore_errors=True)
    keep = ('estimates', 'not_refined', 'logLik', 'error', 'exception', 'method', 'one_sided',
            'score_norm', 'first_method', 'relative_step')
    return {k: out[k] for k in keep if k in out}, time.process_time()-started


FALLBACK = ('singular observed information', 'unchecked stationary point', 'line search failed',
            'Newton budget exhausted')


def scaled_newton(folder, meta, golden, dps=50):
    """Damped Newton at dps digits in coordinates u_i = (theta_i - start_i) / c_i, c_i = max(|start_i|, s_i).

    The fallback when reference() stops on scale alone (an absolute score
    threshold, a pivot test on a badly scaled Hessian). Same density, domains,
    feasibility and coordinate classes as reference(); convergence is a
    relative Newton step below 10^-(dps-10) with negative definite scaled
    information, the contract's Newton-step error estimate."""
    import mpmath as mp, sympy as sp, re as _re
    from quality.density import density_expression
    from quality.harness.check import interval, r_bound_to_sympy
    params = meta['params']; names = [p['name'] for p in params]
    values = {k: float(v) if isinstance(v, str) else v for k, v in golden['estimates'].items()}
    held_names = {k for k, v in golden['fit_args'].items() if v is not None}
    domains = [interval(p['domain'], values) for p in params]
    for i, p in enumerate(params):
        if p['name'] in held_names: continue
        rules = p.get('fit_domain', []); rules = [rules] if isinstance(rules, dict) else rules
        for rule in rules:
            if any(set(h).issubset(held_names) for h in rule['restriction'].get('exempt_when_held', [])): continue
            lo, hi = domains[i]; domains[i] = (max(lo, rule.get('lower', -mp.inf)), min(hi, rule.get('upper', mp.inf)))
    syms, x, expr = density_expression(folder, meta, domains)
    theta = [values[n] for n in names]
    not_refined = {}
    for i, p in enumerate(params):
        if p['name'] in held_names: not_refined[p['name']] = 'held'
        elif p.get('integer'): not_refined[p['name']] = 'integer'
        elif p.get('bound'): not_refined[p['name']] = 'support bound'
        elif p.get('kink'): not_refined[p['name']] = 'kink'
        elif theta[i] in domains[i]: not_refined[p['name']] = 'at domain bound'
    free = [i for i, p in enumerate(params) if p['name'] not in not_refined]
    def parse(v):
        v = str(v)
        for p in sorted(params, key=lambda p: len(p['name']), reverse=True): v = _re.sub(r'\b'+p['name']+r'\b', 'p_'+p['name'], v)
        return sp.sympify(v, locals={**{'p_'+p['name']: s for p, s in zip(params, syms)}, 'inf': sp.oo})
    # A free coefficient that puts a support bound exactly on the sample extreme is a support-bound
    # coordinate the dossier does not flag (a location with support [mu, inf)): hold it, refine the rest.
    for key, extreme in (('lower', min(golden['data'])), ('upper', max(golden['data']))):
        bound_expr = parse(r_bound_to_sympy(str(meta['support'][key])))
        try: at = float(bound_expr.subs({syms[j]: sp.Rational(float(theta[j])) for j in range(len(params))
                                            if math.isfinite(theta[j])}))
        except (TypeError, ValueError): continue
        if at == extreme:
            for i in list(free):
                if syms[i] in bound_expr.free_symbols:
                    not_refined[params[i]['name']] = 'on the support bound (sample %s)' % ('minimum' if key == 'lower' else 'maximum')
                    free.remove(i)
    if not free: return {'error': 'no free regular coefficient left to refine', 'not_refined': not_refined}
    subs = {syms[i]: (sp.Integer(int(theta[i])) if params[i].get('integer') else sp.Rational(float(theta[i])))
            for i in range(len(params)) if i not in free}
    # A coefficient on a domain bound can make an unused branch log(0); its value is -inf, not an error.
    e = expr.subs(subs).doit().replace(sp.Abs, lambda a: sp.Piecewise((a, a >= 0), (-a, True))).subs(sp.zoo, -sp.oo)
    fs_ = [syms[i] for i in free]
    f = sp.lambdify([*fs_, x], e, 'mpmath', cse=True)
    g = [sp.lambdify([*fs_, x], sp.diff(e, a), 'mpmath', cse=True) for a in fs_]
    h = [[sp.lambdify([*fs_, x], sp.diff(e, a, b), 'mpmath', cse=True) for b in fs_] for a in fs_]
    domain_fns = [[sp.lambdify(syms, parse(v.strip()), 'mpmath') for v in p['domain'][1:-1].split(',')] for p in params]
    support_fns = [sp.lambdify(syms, parse(r_bound_to_sympy(str(meta['support'][k]))), 'mpmath') for k in ('lower', 'upper')]
    scale_of = {}   # conditioning, not the contract floor: each coordinate in units of its own size
    for i in free:
        s = abs(theta[i]) or 1.0
        if params[i].get('role') == 'location':
            sc = [j for j, q in enumerate(params) if q.get('role') == 'scale']
            if sc and math.isfinite(theta[sc[0]]) and theta[sc[0]] > 0: s = max(abs(theta[i]), theta[sc[0]])
        scale_of[i] = s
    with mp.workdps(dps+10):
        xs = [mp.mpf(v) for v in golden['data']]
        c = [mp.mpf(scale_of[i]) for i in free]
        full = lambda t: [mp.mpf(theta[i]) if i not in free else t[free.index(i)] for i in range(len(params))]
        def ll(t):
            a = full(t)
            try:
                for i, ((lo_fn, hi_fn), p) in enumerate(zip(domain_fns, params)):
                    lo, hi = lo_fn(*a), hi_fn(*a); v = a[i]
                    if v < lo or v > hi or (v == lo and p['domain'][0] == '(') or (v == hi and p['domain'][-1] == ')'): return None
                lo, hi = [fn(*a) for fn in support_fns]
                if not all(lo <= xi <= hi for xi in xs): return None
                v = mp.fsum(f(*t, xi) for xi in xs)
            except (ValueError, ZeroDivisionError, TypeError): return None
            return v if mp.isfinite(v) and mp.im(v) == 0 else None
        t = [mp.mpf(theta[i]) for i in free]
        cur = ll(t)
        if cur is None: return {'error': 'log-likelihood not finite at the returned estimate'}
        size = mp.inf
        for iteration in range(1, 200):
            G = [mp.fsum(gi(*t, xi) for xi in xs) for gi in g]
            H = [[mp.fsum(hij(*t, xi) for xi in xs) for hij in row] for row in h]
            Gu = mp.matrix([G[a]*c[a] for a in range(len(free))])
            Hu = mp.matrix([[H[a][b]*c[a]*c[b] for b in range(len(free))] for a in range(len(free))])
            try: step = mp.lu_solve(Hu, -Gu)
            except ZeroDivisionError: return {'error': 'singular scaled information'}
            scale = mp.mpf(1)
            for _ in range(80):
                trial = [ti+scale*si*ci for ti, si, ci in zip(t, step, c)]
                val = ll(trial)
                if val is not None and val >= cur-mp.mpf(10)**(-dps)*(1+abs(cur)): break
                scale /= 2
            else: return {'error': 'scaled line search failed'}
            t, cur = trial, val
            size = max(abs(scale*si) for si in step)
            if scale == 1 and size < mp.mpf(10)**(-(dps-10)): break
        else: return {'error': 'scaled Newton budget exhausted'}
        H = mp.matrix([[mp.fsum(hij(*t, xi) for xi in xs)*c[a]*c[b] for b, hij in enumerate(row)] for a, row in enumerate(h)])
        try: mp.cholesky(-H)
        except (ValueError, ZeroDivisionError): return {'error': 'scaled information not negative definite'}
        ref = full(t)
        return {'estimates': {p['name']: mp.nstr(v, 25) for p, v in zip(params, ref)}, 'not_refined': not_refined,
                'logLik': mp.nstr(cur, 25), 'method': 'scaled damped Newton at %d digits (fit-refine.py fallback)' % dps,
                'iterations': iteration, 'relative_step': mp.nstr(size, 3)}

def floor_scale(meta, name, star, values):
    """The contract's s_i: a floor only for coefficients that can be zero or change sign.

    A location uses the fitted scale; another coefficient whose domain reaches
    zero or below uses 1; a strictly positive coefficient has no floor."""
    from quality.harness.check import interval
    p = next(p for p in meta['params'] if p['name'] == name)
    if p.get('role') == 'location':
        scale = [q['name'] for q in meta['params'] if q.get('role') == 'scale']
        if scale:
            v = star.get(scale[0], values.get(scale[0]))
            if v is not None and math.isfinite(float(v)) and float(v) > 0: return float(v)
        return 1.0
    try: lo = interval(p['domain'], {k: float(v) for k, v in values.items()})[0]
    except Exception: lo = -math.inf
    return 0.0 if lo > 0 or (lo == 0 and p['domain'].strip()[0] == '(') else 1.0

def selected_cases(picks, snapshots, archive):
    rebuilt = {}
    for family, source, case in picks:
        if family not in rebuilt:
            rebuilt[family] = rebuild(family, archive)
        ident, inputs = rebuilt[family]
        meta = law_meta(ident)
        entry = inputs.get((source, case))
        if entry is None:
            raise SystemExit('%s %s %s: not among fit-snapshot.py cases' % (family, source, case))
        c, digest = entry
        convert = int if meta['support']['type'] == 'integer' else float
        data = [convert(v) for v in c['data']]
        for mode, rows in snapshots.items():
            r = rows.get((family, source, case))
            if r is None:
                continue
            if r['input_sha256'] != digest:
                raise SystemExit('%s %s %s: snapshot input differs' % (family, source, case))
            if r['result']['kind'] != 'ok':
                continue
            values, hold, absent = law_values(ident, meta, r, data)
            key = hashlib.sha256(json.dumps([family, source, case, digest,
                sorted((k, float(v).hex()) for k, v in r['held'].items()), density_source(ident), VERSION]
                ).encode()).hexdigest()[:24]
            scope = profile_coordinates(ident, values, hold) if not absent else ()
            scoped_key = hashlib.sha256(json.dumps([key, scope]).encode()).hexdigest()[:24] if scope else key
            yield (mode, family, source, case), (scoped_key, key), (ident, meta, data, values, hold), absent


def budget_reason(cache, family, source):
    family_refs = [ref for ref in cache.values() if ref['family'] == family]
    if sum(ref.get('error', '').startswith('case guard') for ref in family_refs) >= 2:
        return 'not attempted: two cases hit the case guard'
    if source == 'archive' and sum(ref['seconds'] for ref in family_refs if ref['source'] == 'archive') >= FAMILY_ALLOWANCE:
        return 'not attempted: family archive allowance of %d CPU s spent' % FAMILY_ALLOWANCE
    if sum(ref['seconds'] for ref in cache.values()) >= TOTAL_BUDGET:
        return 'not attempted: total refinement budget of %d CPU s spent' % TOTAL_BUDGET
    return None


def resolve(cases, cache, stale, args, pool, timings):
    """Share references across modes and keep at most one miss in flight per family."""
    pending = {}
    pending_families = set()
    previously_admitted = set(stale)
    retried = set()
    fresh = 0

    def collect():
        nonlocal fresh
        for key, (future, inputs) in pending.items():
            (_, family, source, case), (_, old_key), (_, _, _, values, _), _ = inputs[0]
            out, seconds = future.result() if pool else future
            ref = dict(key=key, version=VERSION, profile_revision=1, family=family, source=source, case=case,
                       start={k: float(v).hex() for k, v in values.items()},
                       seconds=round(seconds, 2), **out)
            cache[key] = ref
            stale.pop(old_key, None)
            timings[family] += seconds
            fresh += 1
            if fresh % 20 == 0:
                save({**stale, **cache})
                print('  %d refined (%s)' % (fresh, family), flush=True)
            for case_input in inputs:
                yield case_input, ref
        pending.clear()
        pending_families.clear()

    for case_input in cases:
        (_, family, source, _), (key, old_key), inputs, absent = case_input
        if absent:
            yield case_input, {'error': 'coefficients missing ' + ','.join(absent)}
            continue
        if key in pending:
            pending[key][1].append(case_input)
            continue
        ref = cache.get(key)
        retry = (ref is not None and not args.cache_only and args.retry_open and key not in retried
                 and re.search(args.retry_open, ref.get('error', ref.get('exception', ''))))
        if ref is not None and not retry:
            yield case_input, ref
            continue
        if len(pending) >= args.jobs or family in pending_families:
            yield from collect()
        if args.budget and sum(timings.values()) > args.budget:
            yield case_input, None
            continue
        # Revisit invalidated references the ledger had already admitted.
        reason = None if retry or old_key in previously_admitted else budget_reason(cache, family, source)
        if reason:
            yield case_input, ref or {'error': reason}
        elif args.cache_only:
            yield case_input, None
        else:
            retried.add(key)
            call = (*inputs, args.case_limit)
            work = pool.submit(refine, *call) if pool else refine(*call)
            pending[key] = (work, [case_input])
            pending_families.add(family)
    yield from collect()
    if fresh:
        save({**stale, **cache})
        print('refinement CPU seconds this run: %.0f over %d references' % (sum(timings.values()), fresh))
        for family, seconds in sorted(timings.items(), key=lambda kv: -kv[1])[:15]:
            print('  %-26s %7.1f' % (family, seconds))


def judge(case_input, ref, not_refined):
    (mode, family, source, case), _, (_, meta, _, values, _), _ = case_input
    rec = dict(mode=mode, family=family, source=source, case=case)
    if ref is None:
        return dict(rec, status='missing')
    if 'exception' in ref:
        return dict(rec, status='exception', reason=ref['exception'])
    if 'error' in ref:
        return dict(rec, status='open', reason=ref['error'])
    star = {k: float(v) for k, v in ref['estimates'].items()}
    worst = None
    for p in meta['params']:
        name = p['name']
        why = ref.get('not_refined', {}).get(name)
        if why == 'held':
            continue
        if why and not why.startswith('closed form'):
            if why == 'at domain bound' and name in ref.get('one_sided', {}):
                why += ' (one-sided score checked)'
            not_refined['%s %s: %s' % (family, name, why)] += 1
            continue
        hat, target = values[name], star[name]
        allowed = TOL*max(abs(target), floor_scale(meta, name, star, values))
        err = abs(hat-target) if math.isfinite(target) or hat != target else 0.0
        ratio = err/allowed if allowed > 0 else (0.0 if err == 0 else math.inf)
        if worst is None or ratio > worst['ratio']:
            worst = dict(ratio=ratio, name=name, fitted=hat, reference=target, error=err, allowed=allowed)
    if worst is None:
        return dict(rec, status='open', reason='no independently refinable coefficient')
    return dict(rec, status='fail' if worst['ratio'] > 1 else 'checked', worst=worst)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('native'); ap.add_argument('double', nargs='?')
    ap.add_argument('--cache-only', action='store_true', help='judge from the cache alone; list cases without an entry')
    ap.add_argument('--family', help='comma-separated families or route ids to refine/judge')
    ap.add_argument('--case-limit', type=int, default=300, help='wall-clock seconds per refinement (audit guard)')
    ap.add_argument('--records', type=Path, help='write one JSON line per selected estimate')
    ap.add_argument('--budget', type=float, help='stop starting new refinements after this many CPU seconds (resume later)')
    ap.add_argument('--jobs', type=int, default=1, help='maximum concurrent reference workers (different families)')
    ap.add_argument('--retry-open', metavar='REGEX', help='recompute cached references whose open reason or exception matches')
    add_arguments(ap)
    args = ap.parse_args()
    if args.jobs < 1 or args.case_limit < 1:
        ap.error('--jobs and --case-limit must be positive')

    archive = {}
    for c in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
    picks = selection(archive)
    if args.family: picks = [p for p in picks if p[0] in args.family.split(',')]
    snapshots = {mode_of(p): load(p) for p in filter(None, (args.native, args.double))}
    cases = selected_cases(picks, snapshots, archive)
    opened, not_refined = collections.Counter(), collections.Counter()
    timings = collections.defaultdict(float)
    failures, exceptions, judged, missing = {}, {}, set(), []
    cache, stale = load_cache()

    with ExitStack() as stack:
        pool = stack.enter_context(ProcessPoolExecutor(args.jobs)) if args.jobs > 1 and not args.cache_only else None
        output = stack.enter_context(args.records.open('w')) if args.records else None
        for case_input, ref in resolve(cases, cache, stale, args, pool, timings):
            rec = judge(case_input, ref, not_refined)
            if output:
                output.write(json.dumps(rec)+'\n')
            labels = case_input[0]
            key = '|'.join((CHECK, *labels))
            if rec['status'] == 'exception':
                exceptions[key] = rec['reason']
            elif rec['status'] == 'open':
                opened[labels[1], rec['reason']] += 1
            elif rec['status'] == 'missing':
                missing.append(labels)
            else:
                judged.add(labels)
                if rec['status'] == 'fail':
                    w = rec['worst']
                    failures[key] = '%s = %.17g, refined %.17g: error %.3g is %.3g x the contract bound' % (
                        w['name'], w['fitted'], w['reference'], w['error'], w['ratio'])
    if opened:
        print('open (refinement could not settle the case; not a failure):')
        for (family, reason), count in sorted(opened.items()):
            print('  %s: %s: %d' % (family, reason, count))
    print('reference exceptions (harness failures): %d' % len(exceptions))
    for key, reason in sorted(exceptions.items()):
        print('  %s: %s' % (key, reason))
    if not_refined:
        print('coefficients not refined (no independent value; not judged):')
        for k, v in sorted(not_refined.items()): print('  %s: %d' % (k, v))
    if missing: print('no cached reference (run without --cache-only): %d cases, e.g. %s' % (len(missing), missing[:5]))
    print('%d cases judged: %d passed, %d failed' % (len(judged), len(judged)-len(failures), len(failures)))
    status = ratchet(failures, judged, lambda check: check == CHECK, args, 'fit-refine')
    sys.exit(status or bool(exceptions))


def save(cache):
    tmp = CACHE.with_suffix('.tmp')
    text = ''.join(json.dumps(cache[key], sort_keys=True)+'\n' for key in sorted(cache))
    with open(tmp, 'wb') as raw, gzip.GzipFile(fileobj=raw, mode='wb', mtime=0, filename='') as dest:
        dest.write(text.encode())
    tmp.replace(CACHE)

if __name__ == '__main__': main()
