#!/usr/bin/env python3
"""Check the declared equivariance of dossier families (existence.equivariance).

A family is affine-equivariant if X -> A + B X (B > 0) stays in the family,
and power-scale equivariant if X -> A X^B (A, B > 0, positive data) does,
with the coefficients mapped by the declared expressions (unlisted
coefficients unchanged). The claim is the density identity

  log f(T(x); phi(theta)) + log T'(x) = log f(x; theta)

for every x in the support and every coefficient vector theta, where T is the
data map and phi the coefficient map. This script checks it

  symbolic   sp.simplify of the difference of the dossier's log_density
             (a 20-second guard per family; 'not shown' is not a failure);
  dossier    the dossier's log_density at 40 digits (SymPy evalf) at 24
             seeded points (theta, A, B, x), relative tolerance 1e-25;
             skipped for an implicit density (Tukey lambda, written in u = F(x));
  c++        the family's dpqr.hpp log density in binary64 at the same
             points, |difference| <= 1e-9 (1 + |a| + |b|).

It also checks the structure: mapped coefficients keep the parameter domain
at every seeded point and carry no fit-domain restriction, so the map is a
bijection of the fitting domain (its inverse is the map of (-A/B, 1/B) or
(A^(-1/B), 1/B)). Numerical agreement is regression evidence, not a proof;
'symbolic' is a proof of the identity, given SymPy's simplification.

Usage:
  equivariance.py [--family a,b] [--no-cpp] [--json FILE]
"""
import argparse, json, math, random, re, signal, sys
from pathlib import Path
import sympy as sp
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import existence_keys as ek
from quality.density import density_expression
from quality.relations import support as support_bounds

SEED = 20261114
TRIALS = 24
DOSSIER_TOL = 1e-25
CPP_TOL = 1e-9

def declared(only=None):
    out = []
    for folder in sorted(ek.DISTS.iterdir()):
        if not (folder/'README.md').exists() or (only and folder.name not in only): continue
        meta = ek.dossier(folder.name)
        keys = ek.read(meta)
        if 'equivariance' in keys: out.append((folder.name, meta, keys['equivariance']))
    return out

def interval(domain, values):
    """(lo, hi, lo_open, hi_open) of a domain string such as '(0, inf)' or '[a, b]'."""
    text = domain.strip()
    lo, hi = (v.strip() for v in text[1:-1].split(','))
    def val(v):
        if v in ('inf', 'Inf'): return math.inf
        if v in ('-inf', '-Inf'): return -math.inf
        try: return float(v)
        except ValueError: return float(values[v])
    return val(lo), val(hi), text[0] == '(', text[-1] == ')'

def inside(meta, values):
    for p in meta['params']:
        lo, hi, lo_open, hi_open = interval(p['domain'], values)
        v = values[p['name']]
        if not math.isfinite(v) or v < lo or v > hi or (v == lo and lo_open) or (v == hi and hi_open): return False
    return True

def parse(expr, local):
    """SymPy expression of a map entry; names are aliased (coefficients may be keywords such as lambda)."""
    source, env = str(expr), {}
    for name in sorted(local, key=len, reverse=True):
        alias = '_eq_'+name
        source = re.sub(r'\b%s\b' % re.escape(name), alias, source)
        env[alias] = local[name]
    return sp.sympify(source, locals=env)

def mapped(eq, values, A, B):
    """phi(theta) as 40-digit SymPy numbers (A, B and theta are binary64 values)."""
    local = {k: sp.Float(v, 40) for k, v in values.items()}
    local.update(A=sp.Float(A, 40), B=sp.Float(B, 40))
    out = dict(values)
    for k, expr in eq['map'].items(): out[k] = parse(expr, local)
    return out

def transform(eq, x, A, B):
    """T(x) and log T'(x) as SymPy expressions."""
    if eq['group'] == 'affine': return A+B*x, sp.log(B)
    return A*x**B, sp.log(A*B)+(B-1)*sp.log(x)

def points(ident, meta, eq):
    """Seeded (theta, A, B, x) with theta and phi(theta) in the parameter domain."""
    rng = random.Random('%s:%s' % (SEED, ident))
    out = []
    for attempt in range(40*TRIALS):
        values = {}
        for p in meta['params']:
            v = float(p['simulate'])
            if p['name'] in eq['held']: v = eq['held'][p['name']]
            elif attempt and p['role'] in ('location', 'threshold', 'endpoint', 'lower endpoint'): v += rng.uniform(-.3, .3)
            elif attempt and v > 0: v *= rng.uniform(.65, 1.6)
            elif attempt: v += rng.uniform(-.5, .5)
            values[p['name']] = v
        if not inside(meta, values): continue
        A = rng.uniform(-2, 2) if eq['group'] == 'affine' else rng.uniform(.3, 3)
        B = rng.uniform(.3, 3)
        image = {k: float(v) for k, v in mapped(eq, values, A, B).items()}
        if not inside(meta, image): raise ValueError('%s: mapped coefficients leave the domain at %s (A=%g, B=%g)' % (ident, values, A, B))
        lo, hi = support_bounds(meta, {k: sp.Float(v) for k, v in values.items()})
        if math.isfinite(lo) and math.isfinite(hi): x = rng.uniform(lo, hi)
        elif math.isfinite(lo): x = lo+rng.uniform(.05, 5)
        elif math.isfinite(hi): x = hi-rng.uniform(.05, 5)
        else: x = values.get(next((p['name'] for p in meta['params'] if p['role'] == 'location'), ''), 0.0)+rng.uniform(-4, 4)
        if eq['group'] == 'power-scale' and x <= 0: continue
        out.append((values, A, B, x))
        if len(out) == TRIALS: return out
    raise ValueError('%s: could not draw %d valid points' % (ident, TRIALS))

def implicit(meta):
    local = {p['name']: sp.Symbol('p_'+p['name']) for p in meta['params']}
    local['x'] = sp.Symbol('x')
    return 'u' in {s.name for s in parse(meta['log_density'], local).free_symbols}

def symbolic(ident, meta, eq):
    """True if SymPy simplifies the identity's residual to zero within the guard; None if not shown."""
    folder = ek.DISTS/ident
    syms, x, expr = density_expression(folder, meta, [(-math.inf, math.inf)]*len(meta['params']), prefer_spec=False)
    names = {p['name']: s for p, s in zip(meta['params'], syms)}
    A, B = sp.Symbol('A', positive=eq['group'] == 'power-scale'), sp.Symbol('B', positive=True)
    local = dict(names, A=A, B=B)
    image = {names[k]: parse(v, local) for k, v in eq['map'].items()}
    if eq['group'] == 'power-scale':   # positive support: x > 0 lets SymPy split logs of powers
        xp = sp.Symbol('x', positive=True); expr = expr.subs(x, xp); x = xp
    tx, jac = transform(eq, x, A, B)
    lhs = expr.subs(image, simultaneous=True).subs(x, tx)+jac
    def guard(*_): raise TimeoutError
    old = signal.signal(signal.SIGALRM, guard); signal.alarm(20)
    try:
        d = lhs-expr
        if sp.simplify(sp.expand_log(d, force=True)) == 0: return True
        d = sp.expand_log(sp.powsimp(sp.expand_power_base(d, force=True), force=True), force=True)
        return sp.simplify(d) == 0 or None
    except TimeoutError: return None
    finally: signal.alarm(0); signal.signal(signal.SIGALRM, old)

def dossier_values(ident, meta, eq, pts):
    folder = ek.DISTS/ident
    syms, x, expr = density_expression(folder, meta, [(-math.inf, math.inf)]*len(meta['params']), prefer_spec=False)
    names = [p['name'] for p in meta['params']]
    out = []
    for values, A, B, xv in pts:
        image = mapped(eq, values, A, B)
        Af, Bf, X = sp.Float(A, 40), sp.Float(B, 40), sp.Float(xv, 40)
        tx, jac = transform(eq, X, Af, Bf)
        left = sp.N(expr.evalf(40, subs={**{s: image[n] for s, n in zip(syms, names)}, x: tx})+jac.evalf(40), 40)
        right = sp.N(expr.evalf(40, subs={**{s: sp.Float(values[n], 40) for s, n in zip(syms, names)}, x: X}), 40)
        if left.is_finite and right.is_finite: rel = float(sp.Abs(left-right)/max(1, abs(complex(right))))
        else: rel = 0.0 if left == right else math.inf
        out.append((rel, complex(right)))   # (relative difference at 40 digits, value)
    return out

class Cpp:
    """The fit-density.py probe: every family's dpqr.hpp log density, binary64.

    fit-consistency.py passes its own probe, index and table."""
    def __init__(self, double=False, probe=None, index=None, table=None):
        self.owned = probe is None
        if probe is None:
            import importlib.util
            spec = importlib.util.spec_from_file_location('fit_density', HERE/'fit-density.py')
            fd = importlib.util.module_from_spec(spec); spec.loader.exec_module(fd)
            table = fd.laws()
            exe, index = fd.build(table, double)
            probe = fd.fs.Probe(exe)
        self.probe, self.index, self.table = probe, index, table
    def close(self):
        if self.owned: self.probe.close()
    def log_density(self, ident, values, xs):
        params = [values[n] for n in self.table[ident][2]]
        line = '%d %d %d %s\n' % (self.index[ident], len(params), len(xs), ' '.join(float(v).hex() for v in params+list(xs)))
        answer = self.probe.ask(line, 60)
        if not isinstance(answer, dict): raise RuntimeError('%s: density probe %s' % (ident, answer))
        return [float(t) if t in ('nan', 'inf', '-inf') else float.fromhex(t) for t in answer['terms']]

def cpp_values(cpp, ident, eq, pts):
    out = []
    for values, A, B, xv in pts:
        image = {k: float(v) for k, v in mapped(eq, values, A, B).items()}
        if eq['group'] == 'affine': tx, jac = A+B*xv, math.log(B)
        else: tx, jac = A*xv**B, math.log(A*B)+(B-1)*math.log(xv)
        left = cpp.log_density(ident, image, [tx])[0]+jac
        right = cpp.log_density(ident, values, [xv])[0]
        out.append((left, right))
    return out

def structure(meta, eq):
    restricted = [p['name'] for p in meta['params'] if p['name'] in eq['map'] and p.get('fit_domain')]
    return 'mapped coefficients with a fit-domain restriction: %s' % restricted if restricted else None

def check(ident, meta, eq, cpp=None, dossier=True, show_symbolic=True):
    """Result dict: ok (bool), and the evidence of each part."""
    r = dict(family=ident, group=eq['group'], map=eq['map'], held=eq['held'])
    problems = []
    s = structure(meta, eq)
    if s: problems.append(s)
    try: pts = points(ident, meta, eq)
    except ValueError as e: return dict(r, ok=False, problems=[str(e)])
    r['points'] = len(pts)
    if dossier:
        if implicit(meta): r['dossier'] = 'implicit density (u = F(x)): not evaluated'
        else:
            vals = dossier_values(ident, meta, eq, pts)
            worst = max(rel+abs(b.imag) for rel, b in vals)
            r['dossier'] = 'max relative difference %.2g at %d points (40 digits)' % (worst, len(vals))
            if not worst <= DOSSIER_TOL: problems.append('dossier log density: '+r['dossier'])
        if show_symbolic and not implicit(meta):
            sym = symbolic(ident, meta, eq)
            r['symbolic'] = 'shown' if sym else 'not shown within the guard'
    if cpp is not None:
        vals = cpp_values(cpp, ident, eq, pts)
        bad = [(a, b) for a, b in vals if not (a == b or (math.isfinite(a) and math.isfinite(b) and abs(a-b) <= CPP_TOL*(1+abs(a)+abs(b))))]
        worst = max((abs(a-b)/(1+abs(a)+abs(b)) if math.isfinite(a) and math.isfinite(b) else 0.0) for a, b in vals)
        r['cpp'] = '%d of %d points agree (max %.2g)' % (len(vals)-len(bad), len(vals), worst)
        if bad: problems.append('C++ log density: %s; first %r' % (r['cpp'], bad[0]))
    r['ok'] = not problems; r['problems'] = problems
    return r

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--family', help='comma-separated family ids')
    ap.add_argument('--no-cpp', action='store_true', help='skip the C++ density')
    ap.add_argument('--no-symbolic', action='store_true', help='skip sp.simplify')
    ap.add_argument('--json', type=Path, help='write the results as JSON')
    args = ap.parse_args()
    families = declared(set(args.family.split(',')) if args.family else None)
    cpp = None if args.no_cpp else Cpp()
    results = []
    try:
        for ident, meta, eq in families:
            r = check(ident, meta, eq, cpp, show_symbolic=not args.no_symbolic)
            results.append(r)
            print('%-11s %-11s %s' % (ident, eq['group'], 'ok' if r['ok'] else 'FAIL'),
                  '| symbolic:', r.get('symbolic', '-'), '| dossier:', r.get('dossier', '-'), '| c++:', r.get('cpp', '-'), flush=True)
            for p in r['problems']: print('    ', p)
    finally:
        if cpp: cpp.close()
    if args.json: args.json.write_text(json.dumps(results, indent=1, default=str)+'\n')
    print('%d families, %d failing' % (len(results), sum(not r['ok'] for r in results)))
    sys.exit(0 if all(r['ok'] for r in results) else 1)

if __name__ == '__main__': main()
