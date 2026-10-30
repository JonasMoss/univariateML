"""Independent d/p/q oracle, derived from README/spec densities.

Points: sampler and fitted parameters, then one-coordinate stresses, ordered
by README parameter order. Positive distances to a single bound are multiplied
by .1 and 10; finite intervals move 90% towards each edge; unbounded nonzero
coordinates are multiplied by .1 and 10 (zero moves to -1 and +1). Integer,
design, fixed and endpoint coordinates remain at the fitted golden value.
Family-local special_points take priority over stresses. At most eight historical
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
from quality.truth_reference import Reference

ROOT = Path(__file__).resolve().parents[3]
DISTS = ROOT/'univariateML-cpp/include/cppunivariate/distributions'
REPORT = ROOT/'dev/research/evaluations/2026-10-29-dpqr-truth'
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
    g = json.loads((folder/'golden.json').read_text())
    return meta, g


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


def points(meta, g, route):
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
        try:
            r = fn(*args, v)
        except (ZeroDivisionError, ValueError):
            # Endpoint values are one-sided limits of the DECLARATIVE law.
            sub = dict(zip(syms, args))
            r = sp.limit(expr.subs(sub), x, sp.Float(str(v), 80), dir='-' if v == getattr(density, 'upper', mp.inf) else '+')
            r = mp.mpf(float(r)) if r in (sp.oo,-sp.oo) else mp.mpf(str(r.evalf(70)))
        if mp.isnan(r):
            r = sp.limit(expr.subs(dict(zip(syms,args))), x, sp.Float(str(v),80), dir='-' if v == getattr(density, 'upper', mp.inf) else '+')
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
            if after[0] < -mp.log(2):
                observed = mp.exp(after[0])*(-mp.expm1(before[0]-after[0]))
            else:
                observed = mp.exp(before[1])*(-mp.expm1(after[1]-before[1]))
            expected = mp.exp(logd(mp.mpf(k)))
            if abs(observed-expected) > mp.mpf('1e-25')*max(expected, mp.mpf('1e-60')):
                raise ArithmeticError(f'pmf/antiderivative mismatch at {k}')
        return mass
    if hasattr(ref, 'normalization_integrand'):
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
        scale = min([abs(x)+1]+([x-ref.lower] if mp.isfinite(ref.lower) else [])+([ref.upper-x] if mp.isfinite(ref.upper) else []))
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
            for j in (q-1, q):
                if not ref.lower <= j < ref.upper:
                    continue
                tail = ref.tails(j)[0 if lower else 1]
                value = tail if logp else mp.exp(tail)
                if abs(value-probe) <= abs(probe)*mp.mpf('1e-12'):
                    a, b, ambiguous = as_double(j), as_double(j+1), 1
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


def make(folder):
    meta, g = read(folder)
    if not (folder/'truth.py').exists():
        raise ValueError('gap: no independently validated antiderivative/support route')
    route = module_at(folder/'truth.py')
    if hasattr(route, 'gap'): raise ValueError('gap: '+route.gap)
    pts = points(meta, g, route)
    result = dict(schema=3, id=folder.name, digits=DIGITS,
                  parameter_names=[p['name'] for p in meta['params']],
                  parameters=[], normalization=[], values=[], quantiles=[],
                  quantile_limits='nearest binary64; columns 7/8: jump ambiguity flag / zero absolute floor; ambiguous bounds are two acceptable lattice points; jump relative distance 1e-12 on input scale',
                  density_sha256=hashlib.sha256((folder/'README.md').read_bytes()).hexdigest())
    default = {p['name']: float(p['simulate']) for p in meta['params']}
    fit = {p['name']: float(g['estimates'][p['name']]) for p in meta['params']}
    # Explicit indices keep special defaults and general fitted branches covered.
    result['sampler_points'] = [[pts.index(default)], [pts.index(fit)]]
    for index, theta in enumerate(pts):
        started = time.monotonic()
        small = min([float(theta[p['name']]) for p in meta['params'] if p['role']=='shape' and float(theta[p['name']])>1e-6] or [1.])
        mp.mp.dps = DIGITS
        t = {k: number(v) for k, v in theta.items()}
        logd = declarative(folder, meta, theta, route)
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
        result['normalization'].append(dict(point=index, mass=mp.nstr(mass,40), seconds=time.monotonic()-started))
        xs = set()
        for lower in (True, False):
            for u in U:
                v = as_double(ref.quantile(mp.log(mp.mpf(u)), lower))
                if math.isfinite(v): xs.add(v)
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
            for logp, inp in inputs:
                lp = number(inp) if logp else mp.log(number(inp))
                q = ref.quantile(lp, lower)
                rounded = as_double(q)
                a, b, ambiguous, floor = quantile_acceptance(ref, logd, q, inp, lower, logp)
                result['quantiles'].append([index,int(lower),int(logp),encode(inp),encode(q),encode(a),encode(b),ambiguous,encode(floor)])
        print(folder.name, index, 'verified', round(time.monotonic()-started,2), flush=True)
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
