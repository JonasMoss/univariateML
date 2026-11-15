#!/usr/bin/env python3
"""Strict layer: the same phenomenon ends in the same outcome in every family.

Reads the shared outcome (kind and reason) that a snapshot records for the
degenerate and invalid classes of fit-snapshot.py (default holds, the golden
fit's), tabulates it per family, and checks rules that hold for every family:

  empty           an empty sample ends in failure(invalid-input)
  non-finite      a sample with a NaN or infinite observation ends in
                  failure(invalid-input)
  one-constant    'one' ([m]) and 'constant' ([m, m, m, m]) end in the same
                  kind and reason: the constant sample's likelihood is the
                  fourth power of the single observation's, so both have the
                  same maximizers, suprema and escapes
  valid-data      observations with a positive likelihood somewhere (the C++
                  log density at the golden coefficients is finite) never end
                  in failure(invalid-input) or failure(invalid-argument); the
                  golden holds are valid by construction. Nor in
                  failure(unsupported), except for a sample of size
                  2 <= n < existence.min_n
  outside-support observations beyond a support bound that no free coefficient
                  moves (the bound evaluated with the held values alone) end
                  in failure(invalid-input): the likelihood is zero everywhere.
                  So do observations on such a bound when it is closed and its
                  endpoint log density is -Inf with the held values alone (zero
                  density for every admissible coefficient vector): the
                  declared endpoint_log_density, or where none is declared the
                  SymPy limit of the dossier's log_density
  collapse        constant valid data in a continuous family whose location
                  and scale are both free end in nonexistence(unbounded): at
                  location m - scale * mode the density grows like 1/scale
  constant-scale  constant valid data with a free scale or rate never end in a
                  numerical failure (no-convergence, evaluation, unresolved,
                  tolerance, conditioning)

and the rules of dev/research/2026-11-14-strict-rule-questions.md, which read
the dossier's strict-layer keys under `existence` (existence_keys.py):

  near-constant   (continuous) [c, c, c+] never ends in failure(no-convergence,
                  evaluation, invalid-input, invalid-argument, unsupported);
                  a refusal already listed under valid-data is not repeated
  near-constant-tie3
                  (continuous, existence.equivariance applies under the
                  golden holds) near-constant ends in the kind and reason of
                  tie3 = [q50, q50, q75], of which it is an exact image, or in
                  failure(conditioning)
  two-point       (existence.equivariance applies) 'two' ends in the declared
                  existence.two_point outcome; undeclared, with three or more
                  free coefficients, in failure(unsupported) below a declared
                  min_n, and never in a numerical failure or budget
  tie-collapse    (continuous, valid, nonconstant samples) with
                  existence.collapse and free location and scale: e*, the
                  declared infimum at the sample's (n, k), decides: e* < 0
                  requires nonexistence(unbounded), e* > 0 forbids it, e* = 0
                  forbids invalid-input, invalid-argument and unsupported (not
                  repeated when valid-data lists the case). 'two' is left to
                  two-point when a two-point outcome is declared.
                  Without the key, free location and scale and no spike
                  restriction: never nonexistence(unbounded)
  equivariance    the declared map holds for the family's C++ log density at
                  seeded points (equivariance.py)

A declared existence.strict_exceptions entry replaces the default verdict of
its class: the fit must end in exactly the declared outcome (reported under
the rule the class belongs to).

Failures are keyed 'phenomenon:<rule>|mode|family|class|<label>' (the
equivariance check: 'phenomenon:equivariance|mode|family|dossier|equivariance')
and compared with strict-known.json, like fit-judge.py.

Usage:
  fit-consistency.py NATIVE [DOUBLE] [--table FILE] [--prune | --add REF [--match RE]]
--table writes the outcome table (Markdown) for the classes below.
"""
import argparse, collections, importlib.util, json, math, re, sys
from pathlib import Path
from strict_common import HERE, add_arguments, load, mode_of, number, ratchet
import existence_keys as ek
import equivariance as eqv

CLASSES = ['empty', 'nan', 'inf', 'neginf', 'negative', 'zeros', 'one', 'two', 'constant', 'ties', 'half-ties',
           'tie3', 'near-constant']
VALID = ['one', 'two', 'constant', 'ties', 'half-ties', 'tie3', 'near-constant', 'zeros', 'negative']
NUMERICAL = {'no-convergence', 'evaluation', 'unresolved', 'tolerance', 'conditioning'}
NEAR_FORBIDDEN = {'no-convergence', 'evaluation', 'invalid-input', 'invalid-argument', 'unsupported'}
REFUSED = {'invalid-input', 'invalid-argument', 'unsupported'}
RULE_OF_CLASS = {'near-constant': 'near-constant', 'two': 'two-point', 'ties': 'tie-collapse',
                 'half-ties': 'tie-collapse', 'tie3': 'tie-collapse'}
CHECK = 'phenomenon'

spec = importlib.util.spec_from_file_location('fit_density', HERE/'fit-density.py')
fd = importlib.util.module_from_spec(spec); spec.loader.exec_module(fd)
fs = fd.fs

def code(o):
    if o is None: return '?'
    s = {'estimate': 'E', 'limit': 'L', 'nonexistence': 'N', 'budget': 'B', 'failure': 'F'}[o['kind']]
    if o['kind'] == 'estimate': return s+('' if o['certified'] else 'u')+('' if o['unique'] else 'm')
    if o['kind'] == 'limit': return s+('' if o['certified'] else 'm')
    return s+(':'+o['reason'] if o['reason'] != 'none' else '')

def declared_code(kind, reason, unique=None):
    s = {'estimate': 'E', 'limit': 'L', 'nonexistence': 'N', 'budget': 'B', 'failure': 'F'}[kind]
    return s+('m' if unique is False else '')+(':'+reason if reason != 'none' else '')

def bound(text, held, names):
    """A support bound (or endpoint log density) evaluated with held values only; None if it depends on a free coefficient."""
    text = str(text).strip()
    match = re.fullmatch(r'if\s*\((.*?)\)\s*(.*?)\s+else\s+(.*)', text)
    if match:
        condition = bound(match[1], held, names)
        return None if condition is None else bound(match[2] if condition else match[3], held, names)
    for name in sorted(names, key=len, reverse=True):   # coefficient names can be keywords (Tukey lambda)
        text = re.sub(r'\b%s\b' % re.escape(name), 'p_'+name, text)
    env = dict(floor=math.floor, min=min, max=max, Inf=math.inf, inf=math.inf, log=math.log, sqrt=math.sqrt,
               pi=math.pi, lgamma=math.lgamma, lbeta=lambda a, b: math.lgamma(a)+math.lgamma(b)-math.lgamma(a+b))
    env.update({'p_'+k: v for k, v in held.items()})
    try: value = eval(text, {'__builtins__': {}}, env)
    except NameError: return None
    return value if isinstance(value, (int, float, bool)) else None

_ENDPOINT = {}
def symbolic_endpoint(ident, meta, held, side, value):
    """Limit of the dossier's log_density at a fixed endpoint, held values substituted: -inf when it
    is -Inf whatever the free coefficients are (no endpoint_log_density declared). None if unknown."""
    key = (ident, side, value, tuple(sorted(held.items())))
    if key in _ENDPOINT: return _ENDPOINT[key]
    folder = fs.DISTS/ident
    result = None
    if (folder/'README.md').exists():
        import signal
        import sympy as sp
        from quality.density import density_expression
        def lower_of(domain):
            try: return float(domain.strip()[1:-1].split(',')[0])
            except ValueError: return -math.inf
        def guard(*_): raise TimeoutError
        old = signal.signal(signal.SIGALRM, guard); signal.alarm(20)
        try:
            syms, x, expr = density_expression(folder, meta, [(lower_of(p['domain']), math.inf) for p in meta['params']], prefer_spec=False)
            expr = expr.subs({s: held[p['name']] for s, p in zip(syms, meta['params']) if p['name'] in held})
            limit = sp.limit(expr, x, value, dir='+' if side == 'lower' else '-')
            if limit == -sp.oo: result = -math.inf
        except Exception: result = None
        finally: signal.alarm(0); signal.signal(signal.SIGALRM, old)
    _ENDPOINT[key] = result
    return result

def outside(ident, meta, held, data):
    """True if some observation lies beyond a support bound fixed by the held values, or on such a
    closed bound whose endpoint log density is -Inf whatever the free coefficients are: declared in
    endpoint_log_density, or else the symbolic limit of the dossier's log_density."""
    s = meta['support']; closed = s.get('closed', [True, True])
    names = {p['name'] for p in meta['params']}
    lo, hi = bound(s['lower'], held, names), bound(s['upper'], held, names)
    ends = s.get('endpoint_log_density') or {}
    def zero(side, value):
        if side in ends: return bound(ends[side], held, names) == -math.inf
        return symbolic_endpoint(ident, meta, held, side, value) == -math.inf
    for x in data:
        if lo is not None and (x < lo or (x == lo and (not closed[0] or zero('lower', lo)))): return True
        if hi is not None and (x > hi or (x == hi and (not closed[1] or zero('upper', hi)))): return True
    return False

def witness(ident, meta):
    """Coefficients at which a finite log likelihood shows the data are valid: the golden estimates."""
    golden = fs.ROUTES[ident]['golden'] if ident in fs.ROUTES else json.loads((fs.DISTS/ident/'golden.json').read_text())
    values = {}
    for p in meta['params']:
        v = golden['estimates'].get(p['name'], (golden.get('fit_args') or {}).get(p['name']))
        if v is None: return None
        values[p['name']] = float(v) if not isinstance(v, str) else float(v.replace('Inf', 'inf'))
    return values

def applies(eq, held):
    """The declared equivariance holds under these holds: mapped coefficients free, required holds present."""
    return eq is not None and not set(eq['map']) & set(held) and all(held.get(k) == v for k, v in eq['held'].items())

def spike(params):
    """A free coefficient carries a fit-domain restriction with the spike mechanism."""
    for p in params:
        domains = p.get('fit_domain') or []
        for d in (domains if isinstance(domains, list) else [domains]):
            if (d.get('restriction') or {}).get('mechanism') == 'spike': return True
    return False

def multiplicity(data, at):
    if at == 'minimum': return sum(v == min(data) for v in data)
    return max(collections.Counter(data).values())

def infimum(collapse, n, k):
    return eval(collapse['infimum'], {'__builtins__': {}}, dict(n=n, k=k))

class Equivariance:
    """The declared map against the C++ log density (equivariance.py), once per family and mode."""
    def __init__(self, probe, index, table):
        self.cpp = eqv.Cpp(probe=probe, index=index, table=table); self.done = {}
    def __call__(self, ident, meta, eq):
        if ident not in self.done:
            r = eqv.check(ident, meta, eq, cpp=self.cpp, dossier=False)
            self.done[ident] = None if r['ok'] else '; '.join(r['problems'])
        return self.done[ident]

def judge(rows, mode, table, probe, index):
    failures = {}; outcomes = collections.defaultdict(dict); judged = set()
    routes = fd.route_ids()
    equivariant = Equivariance(probe, index, table)
    by_family = collections.defaultdict(dict)
    for (family, source, case), r in rows.items():
        if source == 'class' and case in CLASSES: by_family[family][case] = r
    for family, cases in sorted(by_family.items()):
        ident = routes.get(family, family)
        fs.ROUTE_MODE = family in routes
        meta, golden = fs.read(ident)
        fs.ROUTE_MODE = False
        keys = {} if family in routes else ek.read(meta)
        held = fs.golden_held(meta, golden)
        free = [p for p in meta['params'] if p['name'] not in held]
        roles = {p.get('role') for p in free}
        integer = meta['support']['type'] == 'integer'
        theta = witness(ident, meta)
        def fail(rule, label, detail):
            failures['|'.join((CHECK+':'+rule, mode, family, 'class', label))] = detail
        def valid(r):
            """Positive likelihood at the golden coefficients (finite summed C++ log density)."""
            data = [float(int(v)) if integer else float(v) for v in r['data']]
            if theta is None or not data or not all(math.isfinite(v) for v in data): return False
            params, missing = fd.coefficients(ident, table, theta, {}, data, False)
            if params is None: return False
            line = '%d %d %d %s\n' % (index[ident], len(params), len(data), ' '.join(float(v).hex() for v in params+data))
            answer = probe.ask(line, fd.CPU_LIMIT)
            return isinstance(answer, dict) and all(math.isfinite(number(t)) for t in answer['terms'])
        o = {label: r['result'].get('outcome') for label, r in cases.items()}
        for label in cases:
            judged.add((mode, family, 'class', label)); outcomes[family][label] = code(o[label])
        is_failure = lambda label, reason: o[label] and o[label]['kind'] == 'failure' and o[label]['reason'] == reason
        kr = lambda label: (o[label]['kind'], o[label]['reason']) if o[label] else None
        if 'empty' in o and not is_failure('empty', 'invalid-input'):
            fail('empty', 'empty', 'ends in '+code(o['empty']))
        for label in ('nan', 'inf', 'neginf'):
            if label in o and not is_failure(label, 'invalid-input'): fail('non-finite', label, 'ends in '+code(o[label]))
        if o.get('one') and o.get('constant'):
            a, b = o['one'], o['constant']
            if (a['kind'], a['reason']) != (b['kind'], b['reason']):
                fail('one-constant', 'one', "'one' ends in %s, 'constant' in %s" % (code(a), code(b)))
        validity = {}; refused = set()
        min_n = keys.get('min_n', {}).get('value', 0)
        for label in VALID:
            if label not in cases: continue
            data = [float(v) for v in cases[label]['data']]
            if outside(ident, meta, held, data):
                if not is_failure(label, 'invalid-input'):
                    fail('outside-support', label, 'data beyond a fixed support bound, or on a fixed closed bound '
                         'with zero density for every coefficient, end in '+code(o[label]))
                continue
            validity[label] = valid(cases[label])
            if validity[label] and o[label]['kind'] == 'failure':
                reason = o[label]['reason']
                below = 2 <= len(data) < min_n
                if reason in ('invalid-input', 'invalid-argument') or (reason == 'unsupported' and not below):
                    fail('valid-data', label, 'valid observations end in '+code(o[label])); refused.add(label)
        if validity.get('constant'):
            c = o['constant']
            if not integer and 'location' in roles and 'scale' in roles and \
                    (c['kind'], c['reason']) != ('nonexistence', 'unbounded'):
                fail('collapse', 'constant', 'free location and scale, constant data end in '+code(c))
            if roles & {'scale', 'rate'} and c['kind'] == 'failure' and c['reason'] in NUMERICAL:
                fail('constant-scale', 'constant', 'free scale, constant data end in '+code(c))
        # The rules of the strict-rule decision memo (2026-11-14), reading the dossier's existence keys.
        eq = keys.get('equivariance')
        eq = eq if applies(eq, held) else None
        if eq is not None:
            problem = equivariant(ident, meta, eq)
            judged.add((mode, family, 'dossier', 'equivariance'))
            if problem: failures['|'.join((CHECK+':equivariance', mode, family, 'dossier', 'equivariance'))] = problem
        decided = set()
        for label, ex in keys.get('strict_exceptions', {}).items():
            if label not in cases or (label in VALID and not validity.get(label)): continue
            decided.add(label)
            if kr(label) != ex['outcome']:
                fail(RULE_OF_CLASS.get(label, 'declared'), label, 'the dossier proves %s here (strict_exceptions); ends in %s' % (
                    declared_code(*ex['outcome']), code(o[label])))
        if not integer and validity.get('near-constant') and 'near-constant' not in decided:
            n = o['near-constant']
            if n['kind'] == 'failure' and n['reason'] in NEAR_FORBIDDEN and 'near-constant' not in refused:
                fail('near-constant', 'near-constant', 'near-constant data end in '+code(n))
            t3 = cases.get('tie3')
            if eq is not None and t3 is not None and validity.get('tie3') and len(set(t3['data'])) > 1 and \
                    kr('near-constant') != kr('tie3') and kr('near-constant') != ('failure', 'conditioning'):
                fail('near-constant-tie3', 'near-constant', '%s-equivariant: near-constant ends in %s, its preimage tie3 in %s' % (
                    eq['group'], code(n), code(o['tie3'])))
        if eq is not None and 'two' in cases and validity.get('two') and 'two' not in decided:
            t = o['two']; tp = keys.get('two_point')
            if tp is not None: decided.add('two')
            if tp is not None:
                if kr('two') != (tp['kind'], tp['reason']) or (tp['kind'] == 'estimate' and t['unique'] != tp['unique']):
                    fail('two-point', 'two', 'two observations: declared %s, ends in %s' % (
                        declared_code(tp['kind'], tp['reason'], tp['unique']), code(t)))
            elif len(free) >= 3:
                if 2 < min_n:
                    if kr('two') != ('failure', 'unsupported'):
                        fail('two-point', 'two', 'two observations are below the declared min_n %d: ends in %s, not F:unsupported' % (min_n, code(t)))
                elif t['kind'] == 'budget' or (t['kind'] == 'failure' and t['reason'] in NUMERICAL):
                    fail('two-point', 'two', 'two observations, %d free coefficients, outcome not recorded: ends in %s' % (len(free), code(t)))
        if not integer and 'location' in roles and 'scale' in roles:
            collapse = keys.get('collapse')
            for label in VALID:
                if label not in cases or label in decided or not validity.get(label): continue
                data = [float(v) for v in cases[label]['data']]
                if len(set(data)) < 2: continue
                c = o[label]; unbounded = kr(label) == ('nonexistence', 'unbounded')
                if collapse is None:
                    if unbounded and not spike(free):
                        fail('tie-collapse', label, 'free location and scale, no declared collapse: nonconstant data end in N:unbounded')
                    continue
                n, k = len(data), multiplicity(data, collapse['at'])
                e = infimum(collapse, n, k)
                where = 'collapse at %s, n = %d, k = %d: e* = %s (%s)' % (
                    collapse['at'], n, k, e, 'attained' if collapse['attained'] else 'limit')
                if e < 0 and not unbounded: fail('tie-collapse', label, where+' requires N:unbounded; ends in '+code(c))
                elif e > 0 and unbounded: fail('tie-collapse', label, where+' forbids N:unbounded')
                elif e == 0 and c['kind'] == 'failure' and c['reason'] in REFUSED and label not in refused:
                    fail('tie-collapse', label, where+'; ends in '+code(c))
    return failures, judged, outcomes

def markdown(tables):
    modes = list(tables)
    families = sorted(set().union(*(set(t) for t in tables.values())))
    lines = ['| family | '+' | '.join(CLASSES)+' |', '|---|'+'---|'*len(CLASSES)]
    for family in families:
        cells = []
        for label in CLASSES:
            values = [tables[m].get(family, {}).get(label, '-') for m in modes]
            cells.append(values[0] if len(set(values)) == 1 else ' / '.join('%s %s' % (m, v) for m, v in zip(modes, values)))
        lines.append('| %s | %s |' % (family, ' | '.join(cells)))
    return '\n'.join(lines)+'\n'

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('native'); ap.add_argument('double', nargs='?')
    ap.add_argument('--table', type=Path, help='write the outcome table as Markdown')
    add_arguments(ap)
    args = ap.parse_args()
    table = fd.laws()
    failures = {}; judged = set(); tables = {}
    for paths in filter(None, (args.native, args.double)):
        mode = mode_of(paths)
        exe, index = fd.build(table, mode == 'double')
        probe = fs.Probe(exe)
        try: f, j, outcomes = judge(load(paths), mode, table, probe, index)
        finally: probe.close()
        failures.update(f); judged |= j; tables[mode] = outcomes
    if args.table: args.table.write_text(markdown(tables))
    sys.exit(ratchet(failures, judged, lambda check: check.split(':')[0] == CHECK, args, 'fit-consistency'))

if __name__ == '__main__': main()
