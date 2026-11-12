"""Independent d/p/q oracle, derived from README/spec densities.

Points: sampler and fitted parameters, then one-coordinate stresses, ordered
by README parameter order. Positive distances to a single bound are multiplied
by .1 and 10; finite intervals move 90% towards each edge; unbounded nonzero
coordinates are multiplied by .1 and 10 (zero moves to -1 and +1). Integer,
design, fixed and endpoint coordinates remain at the fitted golden value.
The uncapped extreme extension multiplies each positive free coordinate by
1e-8 and 1e8, projects dependent domains, and adds location/scale cancellation
and family-local joint limit paths. Family-local special_points take priority
over historical stresses. At most eight historical
points, plus the README default if absent. Sampling selects the default and fit.

No shared distribution registry: optional truth.py beside README supplies a
high-precision antiderivative/support correction or a documented gap. Routes
are verified against the declarative density, not against C++, scipy or R.
"""
import argparse
import hashlib
import inspect
import importlib.util
import json
import math
import signal
import time
from pathlib import Path
import mpmath as mp
import sympy as sp
import yaml
from quality.density import density_expression
from quality.truth_reference import Reference, complement

ROOT = Path(__file__).resolve().parents[3]
DISTS = ROOT/'univariateML-cpp/include/cppunivariate/distributions'
REPORT = ROOT/'build/truth-audit'
U = ('1e-300', '1e-100', '1e-30', '1e-12', '1e-6', '.01', '.25', '.5')
DIGITS = 80


def number(v):
    return mp.mpf(float(v))  # exact binary input supplied to C++


def as_double(v):
    try:
        return float(v)
    except OverflowError:
        if abs(v) < 1: return 0.0 if v > 0 else -0.0
        return math.inf if v > 0 else -math.inf


def encode(v):
    v = as_double(v)
    if math.isnan(v): raise ArithmeticError('oracle produced a NaN truth cell')
    if math.isinf(v): return 'Inf' if v > 0 else '-Inf'
    return v  # Python's shortest round-trip JSON representation is <=17 digits.


def read(folder):
    meta = yaml.safe_load((folder/'README.md').read_text().split('---', 2)[1])
    golden = folder/'golden.json'
    if golden.exists():
        g = json.loads(golden.read_text())
    elif meta.get('stage') == 'dpqr':
        # A staged probability-only family has no fitted sample yet.
        values = {p['name']: p['simulate'] for p in meta['params']}
        g = {'estimates': values, 'sampler_args': values}
    else:
        raise FileNotFoundError(golden)
    return meta, g


def input_sha256(folder):
    """Versioned dependency fingerprint; dossier prose is deliberately excluded.

    Include point selection inputs, the local route/spec, and shared numerical
    code. External Python dependencies belong to the recorded run environment.
    Keep absent parameter keys absent: points() tests membership for `fixed`.
    """
    meta, g = read(folder)
    keys = ('name', 'domain', 'role', 'integer', 'bound', 'fixed', 'required',
            'kind', 'simulate', 'default')
    inputs = dict(version=1, log_density=meta['log_density'], support=meta['support'],
                  params=[{k: p[k] for k in keys if k in p} for p in meta['params']],
                  r_args=meta.get('r', {}).get('args', {}),
                  golden={k: g[k] for k in ('estimates', 'fixed', 'fit_args', 'sampler_args') if k in g})
    sources = {'route': folder/'truth.py', 'generator': Path(__file__),
               'density': Path(__file__).with_name('density.py'),
               'reference': Path(__file__).with_name('truth_reference.py')}
    route = module_at(folder/'truth.py')
    if getattr(route, 'use_spec', False):
        sources['spec'] = folder/'spec.py'
    inputs['sources'] = {k: p.read_text() for k, p in sources.items()}
    return hashlib.sha256(json.dumps(inputs, sort_keys=True, separators=(',', ':'),
                                     allow_nan=False).encode()).hexdigest()


def bound(text, values):
    v = sp.sympify(sp.sympify(str(text), locals={**values, 'inf': sp.oo}))
    return mp.mpf(float(v)) if v in (sp.oo, -sp.oo) else mp.mpf(str(v.evalf(40)))


def domains(meta, values):
    return [tuple(bound(v.strip(), values) for v in p['domain'][1:-1].split(',')) for p in meta['params']]


def module_at(path):
    spec = importlib.util.spec_from_file_location('truth_route_'+path.parent.name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def historical_points(meta, g, route):
    ps = meta['params']
    fit = {p['name']: float(g['estimates'][p['name']]) for p in ps}
    held = {p['name'] for p in ps if p.get('integer') or p.get('bound') or
            'fixed' in p or p.get('required') or p.get('kind') in ('design', 'threshold', 'endpoint') or
            any(w in p['role'] for w in ('endpoint', 'design', 'trials')) or
            p['role'] in ('lower', 'upper', 'integer centre')}
    held |= set(g.get('fixed', [])) | {k for k, v in g.get('fit_args', {}).items() if v is not None}
    held |= set(getattr(route, 'held_parameters', []))
    held -= {p['name'] for p in ps if p['name'] in getattr(route, 'regular_parameters', [])
             and not (p.get('integer') or p.get('bound') or p.get('required') or 'fixed' in p
                      or p.get('kind') in ('design', 'threshold')
                      or p['name'] in g.get('fixed', []))}
    sample = {p['name']: float(g.get('sampler_args', {}).get(p['name'], p.get('simulate', p.get('default', fit[p['name']])))) for p in ps}
    for arg, name in meta.get('r', {}).get('args', {}).items():
        if name in sample and arg in g.get('sampler_args', {}): sample[name] = float(g['sampler_args'][arg])
    for name in held & fit.keys(): sample[name] = fit[name]
    candidates = [sample, fit]
    for name, vals in getattr(route, 'special_points', {}).items():
        candidates += [{**sample, name: v} for v in vals]
    for p, (lo, hi) in zip(ps, domains(meta, sample)):
        name, v = p['name'], sample[p['name']]
        if name in held: continue
        if mp.isfinite(lo) and mp.isfinite(hi): choices = [lo+(v-lo)/10, hi-(hi-v)/10]
        elif mp.isfinite(lo): choices = [lo+(v-lo)/10, lo+(v-lo)*10]
        elif mp.isfinite(hi): choices = [hi-(hi-v)/10, hi-(hi-v)*10]
        else: choices = [v/10, v*10] if v else [-1., 1.]
        candidates += [{**sample, name: float(w)} for w in choices]
    result = []
    for pt in candidates:
        valid = True
        for p, (lo, hi) in zip(ps, domains(meta, pt)):
            v = pt[p['name']]
            valid &= lo <= v <= hi and not (v == lo and p['domain'][0] == '(') and not (v == hi and p['domain'][-1] == ')')
        if valid and pt not in result: result.append(pt)
        if len(result) == 8: break
    # Preserve the historical d/p/q stress grid; add the public default
    # independently of golden sampler arguments and held-fit coordinates.
    default = {p['name']: float(p['simulate']) for p in ps}
    if default not in result:
        result.append(default)
    for pt in boundary_points(meta, route):
        if pt not in result:
            result.append(pt)
    return result


def extreme_points(meta, route=None):
    """Binary64 parameter stresses, independent of the historical point cap.

    Move one positive coordinate at a time. Project dependent domains, including
    open bounds using the next interior double. Location/scale cancellation is
    a separate stress, not a multiplicative displacement of location zero.
    Local routes can add joint limit paths using extreme_parameter_sets.
    """
    ps = meta['params']
    default = {p['name']: float(p['simulate']) for p in ps}
    candidates = []
    for p, (lo, hi) in zip(ps, domains(meta, default)):
        name, value = p['name'], default[p['name']]
        if p.get('integer') or p.get('kind') == 'design':
            continue
        if value > 0 and lo >= 0:
            candidates.extend({**default, name: value*f} for f in (1e-8, 1e8))
        elif lo < 0 < hi:
            scales = [default[q['name']] for q in ps
                      if q['role'] in ('scale', 'sd', 'standard deviation')]
            unit = scales[0] if scales else max(1., abs(value))
            candidates.extend({**default, name: sign*unit} for sign in (-1, 1))
    # Joint location/scale cancellation in a concentrating shape regime.
    # This includes the Frechet-to-Gumbel path without a family registry.
    locations = [p for p, (lo, hi) in zip(ps, domains(meta, default))
                 if lo < 0 < hi and any(word in p['role'] for word in ('location', 'endpoint', 'lower'))]
    scales = [p for p in ps if p['role'] == 'scale']
    shapes = [p for p in ps if p['role'] in ('shape', 'df') and default[p['name']] > 0]
    # A nonunit scale exposes loss in log(x)-log(scale) that unit-scale
    # one-coordinate stresses cannot reveal.
    if scales:
        for shape in shapes:
            candidates.append({**default, scales[0]['name']: default[scales[0]['name']]*1000,
                               shape['name']: default[shape['name']]*1e8})
    if locations and scales and shapes:
        loc, scale, shape = locations[0]['name'], scales[0]['name'], shapes[0]['name']
        value = default[scale]*1e8
        candidates.append({**default, loc: -value, scale: value,
                           shape: default[shape]*1e8})
    for overrides in getattr(route, 'extreme_parameter_sets', []):
        if set(overrides)-set(default):
            raise ValueError('unknown extreme parameter: '+str(set(overrides)-set(default)))
        candidates.append({**default, **overrides})
    result = []
    for candidate in candidates:
        pt = candidate.copy()
        for _ in range(len(ps)+1):
            previous = pt.copy()
            for p, (lo, hi) in zip(ps, domains(meta, pt)):
                name = p['name']
                a = float(lo) if p['domain'][0] == '[' else math.nextafter(float(lo), math.inf)
                b = float(hi) if p['domain'][-1] == ']' else math.nextafter(float(hi), -math.inf)
                pt[name] = min(b, max(a, pt[name]))
            if pt == previous: break
        if all(not math.isnan(v) and (math.isfinite(v) or v == default[k]) for k, v in pt.items()) and pt != default and pt not in result:
            result.append(pt)
    return result


def points(meta, g, route):
    result = historical_points(meta, g, route)
    for pt in extreme_points(meta, route):
        if pt not in result: result.append(pt)
    return result


def boundary_points(meta, route=None):
    """One attained face at a time, independent of the historical point cap.

    Dependent inclusive bounds are projected until stable (e.g. N=0 forces
    m=k=0). Open bounds are never projected. Infinite attained endpoints
    (truncated normal) are retained. Fit restrictions are not model domains.
    """
    default = {p['name']: float(p['simulate']) for p in meta['params']}
    result = []
    for p, edges in zip(meta['params'], domains(meta, default)):
        for closed, edge in zip((p['domain'][0] == '[', p['domain'][-1] == ']'), edges):
            if not closed:
                continue
            pt = {**default, p['name']: float(edge)}
            if route is not None and hasattr(route, 'boundary_adjust'):
                pt = route.boundary_adjust(pt)
            for _ in range(len(meta['params'])+1):
                previous = pt.copy()
                for q, (lo, hi) in zip(meta['params'], domains(meta, pt)):
                    name = q['name']
                    if pt[name] < lo and q['domain'][0] == '[': pt[name] = float(lo)
                    if pt[name] > hi and q['domain'][-1] == ']': pt[name] = float(hi)
                if pt == previous: break
            if all(lo <= pt[q['name']] <= hi and
                   not (pt[q['name']] == lo and q['domain'][0] == '(') and
                   not (pt[q['name']] == hi and q['domain'][-1] == ')')
                   for q, (lo, hi) in zip(meta['params'], domains(meta, pt))):
                if pt not in result: result.append(pt)
    for name, values in getattr(route, 'boundary_parameters', {}).items():
        for value in values:
            pt = {**default, name: float(value)}
            if pt not in result: result.append(pt)
    for overrides in getattr(route, 'boundary_parameter_sets', []):
        pt = {**default, **overrides}
        if pt not in result: result.append(pt)
    return result


def declarative(folder, meta, theta, route):
    ds = domains(meta, theta)
    # A local override is required when the existing spec is a golden-only
    # approximation. Exact README expressions are otherwise preferred.
    syms, x, expr = density_expression(folder, meta, ds, prefer_spec=False)
    if getattr(route, 'use_spec', False):
        syms, x, expr = density_expression(folder, meta, ds, prefer_spec=True)
    if hasattr(route, 'expression'):
        expr = route.expression(dict(zip([p['name'] for p in meta['params']], syms)), x)
    if expr.free_symbols - set(syms) - {x}:
        raise ValueError('undeclared density symbols: '+str(expr.free_symbols-set(syms)-{x}))
    for summation in expr.atoms(sp.Sum):
        evaluated = summation.subs({sym: (sp.Integer(theta[p['name']]) if p.get('integer') else sp.Float(theta[p['name']],80)) for sym, p in zip(syms, meta['params'])}).doit()
        expr = expr.xreplace({summation: evaluated})
    fn = sp.lambdify([*syms, x], expr, 'mpmath', cse=True)
    args = [number(theta[p['name']]) for p in meta['params']]
    def density(v):
        if hasattr(route, 'endpoint_log_density'):
            endpoint = route.endpoint_log_density(dict(zip([p['name'] for p in meta['params']], args)), v)
            if endpoint is not None:
                return endpoint
        try:
            r = fn(*args, v)
        except (ZeroDivisionError, ValueError):
            # Endpoint values are one-sided limits of the DECLARATIVE law.
            sub = {sym: sp.Rational(float(theta[param["name"]])) for sym,param in zip(syms,meta["params"])}
            r = sp.limit(expr.subs(sub), x, sp.Float(str(v), 80), dir='-' if v == getattr(density, 'upper', mp.inf) else '+')
            r = mp.mpf(float(r)) if r in (sp.oo,-sp.oo) else mp.mpf(str(r.evalf(70)))
        if mp.isnan(r):
            r = sp.limit(expr.subs({sym: sp.Rational(float(theta[param["name"]])) for sym,param in zip(syms,meta["params"])}), x, sp.Float(str(v),80), dir='-' if v == getattr(density, 'upper', mp.inf) else '+')
            r = mp.mpf(float(r)) if r in (sp.oo,-sp.oo) else mp.mpf(str(r.evalf(70)))
        if isinstance(r, mp.mpc):
            if abs(r.imag) > mp.mpf('1e-60'): raise ValueError('complex density')
            r = r.real
        return r
    return density


def verify(ref, logd):
    if ref.discrete:
        if hasattr(ref, 'normalization'):
            mass = ref.normalization
        else:
            raise ValueError('discrete route must supply controlled normalization')
        # Check antiderivative differences against the pmf in its body.
        lo = int(ref.lower) if mp.isfinite(ref.lower) else -6
        for k in range(lo, min(int(ref.upper) if mp.isfinite(ref.upper) else lo+12, lo+12)+1):
            before, after = ref.tails(k-1), ref.tails(k)
            if after[0] == -mp.inf or before[1] == -mp.inf:
                observed = -mp.inf
            elif after[0] < -mp.log(2):
                observed = after[0]+complement(before[0]-after[0])
            else:
                observed = before[1]+complement(after[1]-before[1])
            expected = logd(mp.mpf(k))
            if observed == expected:
                continue
            # Compare the original relative/absolute error in log arithmetic.
            # exp(-k**b) need not be constructed when b is 100 million.
            error = max(observed,expected)+complement(min(observed,expected)-max(observed,expected))
            threshold = mp.log(mp.mpf('1e-25'))+max(expected,mp.log(mp.mpf('1e-60')))
            if error > threshold:
                raise ArithmeticError(f'pmf/antiderivative mismatch at {k}')
        return mass
    if hasattr(ref, 'normalization_identity'):
        mass, error = ref.normalization_identity, mp.mpf(0)
    elif hasattr(ref, 'normalization_integrand'):
        mass, error = mp.quad(ref.normalization_integrand,
                              getattr(ref, 'normalization_domain', [0, mp.mpf('.1'), mp.mpf('.5'), mp.mpf('.9'), 1]), error=True)
    else:
        # Integrate the density in a logarithmic support coordinate. Each
        # omitted end mass is 1e-32; splitting also resolves local kinks.
        cuts = [ref.quantile(mp.log(mp.mpf(u)), tail)
                for tail in (True, False) for u in ('1e-32', '.001', '.1', '.5')]
        cuts += [mp.mpf(v) for v in ref.breaks]
        ts = sorted(set(ref.unchart(v) for v in cuts if ref.lower < v < ref.upper))
        def integrand(t):
            x, jac = ref.chart(t)
            return mp.exp(logd(x)+jac)
        mass, error = mp.quad(integrand, ts, maxdegree=8, error=True)
    if error > mp.mpf('1e-28'):
        raise ArithmeticError('normalization quadrature error estimate '+str(error))
    for u in ('.1', '.3', '.7', '.9'):
        x = ref.quantile(mp.log(mp.mpf(u)), True)
        # At a concentrated finite endpoint the exact quantile may require
        # millions of digits to separate from the edge. Differentiate at an
        # interior chart point instead; normalization is checked independently.
        if not mp.isfinite(x) or not ref.lower < x < ref.upper:
            x = ref.chart(mp.mpf(0))[0]
        scale = min([abs(x)+1]+([x-ref.lower] if mp.isfinite(ref.lower) else [])+([ref.upper-x] if mp.isfinite(ref.upper) else []))
        scale = min(scale, 1/mp.exp(logd(x)))
        h = mp.mpf('1e-15')*scale
        if getattr(ref, 'numerical_tails', False):
            derivative = (mp.exp(ref.tails(x+h)[0])-mp.exp(ref.tails(x-h)[0]))/(2*h)
        else:
            # An absolute step can disappear at huge x or cross a tiny support gap.
            derivative = mp.diff(lambda v:mp.exp(ref.tails(v)[0]),x,h=mp.sqrt(mp.eps)*scale)
        expected = mp.exp(logd(x))
        if abs(derivative-expected) > mp.mpf('1e-25')*expected:
            raise ArithmeticError('density/antiderivative derivative mismatch')
    return mass


def quantile_acceptance(ref, logd, q, inp, lower, logp):
    """Adjacent lattice answers near a jump; scale-aware floor at interior zero.

    Compare the independent tail on the input's own scale, relative to the
    binary64 probe. This is an acceptance rule, not a change to the true root.
    """
    rounded = as_double(q)
    a = math.nextafter(rounded, -math.inf)
    b = math.nextafter(rounded, math.inf)
    ambiguous, floor = 0, 0.
    if ref.discrete:
        a = b = rounded
        if mp.isfinite(q) and abs(q) < 2**53 and math.isfinite(inp) and (inp < 0 if logp else 0 < inp < 1):
            probe = number(inp)
            jumps = (list(zip(ref.atoms, ref.atoms[1:])) if hasattr(ref, 'atoms')
                     else [(q-1, q), (q, q+1)])
            for j, next_atom in jumps:
                if not ref.lower <= j < ref.upper:
                    continue
                tail = ref.tails(j)[0 if lower else 1]
                value = tail if logp else (mp.mpf(0) if tail < -10000 else mp.exp(tail))
                if abs(value-probe) <= abs(probe)*mp.mpf('1e-12'):
                    a, b, ambiguous = as_double(j), as_double(next_atom), 1
                    break
    elif math.isfinite(rounded):
        a = rounded if number(rounded) <= q else a
        b = rounded if number(rounded) >= q else b
        if q == 0 and ref.lower < 0 < ref.upper:
            density = mp.exp(logd(mp.mpf(0)))
            if mp.isfinite(density) and density > 0:
                floor = as_double(mp.mpf('1e-12')/density)
    else:
        a = b = rounded
    return a, b, ambiguous, floor


def make(folder, parameter_points=None):
    meta, g = read(folder)
    if not (folder/'truth.py').exists():
        raise ValueError('gap: no independently validated antiderivative/support route')
    route = module_at(folder/'truth.py')
    if hasattr(route, 'gap'): raise ValueError('gap: '+route.gap)
    pts = points(meta, g, route) if parameter_points is None else parameter_points
    result = dict(schema=3, id=folder.name, digits=DIGITS,
                  parameter_names=[p['name'] for p in meta['params']],
                  parameters=[], normalization=[], values=[], quantiles=[],
                  quantile_limits='nearest binary64; columns 7/8: jump ambiguity flag / zero absolute floor; ambiguous bounds are two acceptable lattice points; jump relative distance 1e-12 on input scale',
                  input_sha256=input_sha256(folder))
    default = {p['name']: float(p['simulate']) for p in meta['params']}
    fit = {p['name']: float(g['estimates'][p['name']]) for p in meta['params']}
    # Explicit indices keep special defaults and general fitted branches covered.
    result['sampler_points'] = [[pts.index(pt)] if pt in pts else [] for pt in (default, fit)]
    historical=historical_points(meta,g,route)
    result['historical_sampler_points']=[[i] for i,pt in enumerate(pts) if pt in historical]
    for index, theta in enumerate(pts):
        started = time.monotonic()
        small = min([float(theta[p['name']]) for p in meta['params'] if p['role']=='shape' and float(theta[p['name']])>1e-6] or [1.])
        mp.mp.dps = DIGITS
        t = {k: number(v) for k, v in theta.items()}
        # Routes with exact limiting log densities need not instantiate a
        # singular symbolic formula (or a many-million-term normalizer).
        density_cache = []
        def logd(v):
            if not density_cache:
                density_cache.append(declarative(folder, meta, theta, route))
                density_cache[0].upper = getattr(logd, "upper", mp.inf)
            return density_cache[0](v)
        ref = route.build(t, logd) if len(inspect.signature(route.build).parameters) == 2 else route.build(t)
        if not hasattr(ref, 'normalization_integrand') and ((mp.isfinite(ref.lower) and ref.lower != 0) or mp.isfinite(ref.upper)):
            mp.mp.dps = min(600,max(DIGITS,int(40/small)+20))
            t = {k: number(v) for k,v in theta.items()}
            ref = route.build(t, logd) if len(inspect.signature(route.build).parameters) == 2 else route.build(t)
        if ref.log_density is not None: logd = ref.log_density
        logd.upper = ref.upper
        mass = verify(ref, logd)
        if not mp.isfinite(mass) or abs(mass-1) > mp.mpf('1e-25'):
            raise ArithmeticError('normalization failed at '+str(theta)+': '+mp.nstr(mass,40))
        result['parameters'].append([encode(theta[p['name']]) for p in meta['params']])
        result['normalization'].append(dict(point=index, mass=mp.nstr(mass,40)))
        xs = set()
        for lower in (True, False):
            for u in U:
                v = as_double(ref.quantile(mp.log(mp.mpf(u)), lower))
                if math.isfinite(v): xs.add(v)
        for lower in (True,False):
            for logp, inp in getattr(route, 'quantile_inputs', []):
                lp=number(inp) if logp else mp.log(number(inp))
                value=as_double(ref.quantile(lp,lower))
                if math.isfinite(value): xs.add(value)
        for edge in (ref.lower, ref.upper):
            e = float(edge)
            xs.add(e)
            if math.isfinite(e):
                xs.update((math.nextafter(e, -math.inf), math.nextafter(e, math.inf)))
        for x in (1e100, 1e300):
            if ref.lower < x < ref.upper: xs.add(x)
        if ref.discrete:
            lo = int(ref.lower) if mp.isfinite(ref.lower) else -100
            hi = min(int(ref.upper) if mp.isfinite(ref.upper) else lo+199, lo+199)
            xs.update(range(lo, hi+1)); xs.add(lo+.5)
        for x in sorted(xs):
            v = number(x)
            outside = v < ref.lower or v > ref.upper or mp.isinf(v)
            ld = -mp.inf if outside or (ref.discrete and v != mp.floor(v)) else logd(v)
            ll, lu = ref.tails(v)
            result['values'].append([index, encode(x), encode(ld), encode(ll), encode(lu)])
        for lower in (True, False):
            # Each input is evaluated after binary64 rounding, including log u.
            inputs = [(True, float(mp.log(mp.mpf(u)))) for u in U]+[(True,-800.), (True,-1e4), (True,-math.inf), (True,0.)]
            inputs += [(False,float(u)) for u in U]+[(False,0.), (False,1.)]
            inputs += list(getattr(route, 'quantile_inputs', []))
            for logp, inp in inputs:
                lp = number(inp) if logp else mp.log(number(inp))
                q = ref.quantile(lp, lower)
                rounded = as_double(q)
                a, b, ambiguous, floor = quantile_acceptance(ref, logd, q, inp, lower, logp)
                result['quantiles'].append([index,int(lower),int(logp),encode(inp),encode(q),encode(a),encode(b),ambiguous,encode(floor)])
        print(folder.name, index, 'verified', round(time.monotonic()-started,2), flush=True)
    return apply_exclusions(result, route)


def apply_point_exclusions(result, route):
    """Archive complete independent laws whose public d/p/q/r member fails.

    Remove their active indices so R consumers do not sample an unsupported
    member. The archived rows use a local point index zero. Public default
    and golden sampler points may never be excluded.
    """
    declarations = getattr(route, 'truth_point_exclusions', [])
    removed = {}
    for index, theta in enumerate(result['parameters']):
        reasons = [reason for params, reason in declarations if theta == params]
        if reasons: removed[index] = '; '.join(reasons)
    if not removed: return result
    if any(row[0] in removed for row in result['sampler_points'] if row):
        raise ValueError('cannot exclude a default or fitted sampler point')
    mapping = {}
    parameters, normalization = [], []
    for index, theta in enumerate(result['parameters']):
        if index not in removed:
            mapping[index] = len(parameters)
            parameters.append(theta)
        else:
            entry = dict(parameters=theta, reason=removed[index])
            entry['normalization'] = [dict(point=0, mass=r['mass']) for r in result['normalization'] if r['point'] == index]
            for key in ('values', 'quantiles'):
                entry[key] = [[0, *r[1:]] for r in result[key] if r[0] == index]
            result.setdefault('excluded_parameter_points', []).append(entry)
    for r in result['normalization']:
        if r['point'] not in removed:
            normalization.append({**r, 'point': mapping[r['point']]})
    for key in ('values', 'quantiles'):
        result[key] = [[mapping[r[0]], *r[1:]] for r in result[key] if r[0] not in removed]
    result['parameters'], result['normalization'] = parameters, normalization
    result['sampler_points'] = [[mapping[row[0]]] if row else [] for row in result['sampler_points']]
    if 'historical_sampler_points' in result:
        result['historical_sampler_points']=[[mapping[row[0]]] for row in result['historical_sampler_points'] if row and row[0] in mapping]
    return result


def apply_exclusions(result, route):
    """Keep independently computed failing rows visible, outside active checks.

    Each local declaration names exact encoded parameters and input fields,
    plus a reason. No tolerance is relaxed and no oracle value is replaced.
    """
    apply_point_exclusions(result, route)
    declarations = getattr(route, 'truth_exclusions', {})
    if set(declarations) - {'values', 'quantiles'}:
        raise ValueError('unknown truth exclusion category')
    for key in ('values', 'quantiles'):
        exclusions = declarations.get(key, [])
        kept, excluded = [], []
        for row in result[key]:
            probe = row[1:2] if key == 'values' else row[1:4]
            reasons = [reason for theta, at, reason in exclusions
                       if result['parameters'][row[0]] == theta and probe == at]
            if hasattr(route, 'truth_workload_exclusion'):
                theta = dict(zip(result['parameter_names'], result['parameters'][row[0]]))
                reason = route.truth_workload_exclusion(theta, key, row)
                if reason: reasons.append(reason)
            if reasons:
                excluded.append(dict(row=row, reason='; '.join(reasons)))
            else:
                kept.append(row)
        result[key] = kept
        if excluded:
            result['excluded_'+key] = excluded
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['truth'])
    parser.add_argument('ids', nargs='*')
    parser.add_argument('--timeout', type=int, default=900,
                        help='wall-clock seconds per family (default: 900)')
    parser.add_argument('--report-dir', type=Path, default=REPORT,
                        help='directory for per-family generation status')
    args = parser.parse_args()
    if args.timeout <= 0: parser.error('--timeout must be positive')
    args.report_dir.mkdir(parents=True, exist_ok=True)
    failures = 0
    for id in args.ids or sorted(p.parent.name for p in DISTS.glob('*/README.md')):
        if Path(id).name != id or not (DISTS/id/'README.md').exists(): parser.error('unknown family '+id)
        started = time.monotonic()
        def timeout(signum, frame):
            raise TimeoutError(f'family exceeded {args.timeout} seconds; increase --timeout or inspect the route')
        signal.signal(signal.SIGALRM,timeout)
        signal.alarm(args.timeout)
        with mp.workdps(DIGITS):
            try:
                result = make(DISTS/id)
                path = DISTS/id/'truth.json'
                tmp = path.with_suffix('.json.tmp')
                tmp.write_text(json.dumps(result,indent=2,allow_nan=False)+'\n'); tmp.replace(path)
                status = dict(id=id,status='generated',points=len(result['parameters']),sampler_points=len(result['sampler_points']),values=len(result['values']),quantiles=len(result['quantiles']))
            except Exception as e:
                status = dict(id=id,status='gap',reason=f'{type(e).__name__}: {e}')
                failures += 1
        signal.alarm(0)
        status['seconds'] = time.monotonic()-started
        (args.report_dir/(id+'.json')).write_text(json.dumps(status,indent=2)+'\n')
        print(id,status,flush=True)
    return int(failures > 0)
