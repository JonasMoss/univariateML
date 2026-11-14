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
                  golden holds are valid by construction
  outside-support observations beyond a support bound that no free coefficient
                  moves (the bound evaluated with the held values alone) end
                  in failure(invalid-input): the likelihood is zero everywhere
  collapse        constant valid data in a continuous family whose location
                  and scale are both free end in nonexistence(unbounded): at
                  location m - scale * mode the density grows like 1/scale
  constant-scale  constant valid data with a free scale or rate never end in a
                  numerical failure (no-convergence, evaluation, unresolved,
                  tolerance, conditioning)

Failures are keyed 'phenomenon:<rule>|mode|family|class|<label>' and
compared with strict-known.json, like fit-judge.py.

Usage:
  fit-consistency.py NATIVE [DOUBLE] [--table FILE] [--prune | --add REF [--match RE]]
--table writes the outcome table (Markdown) for the classes below.
"""
import argparse, collections, importlib.util, json, math, re, sys
from pathlib import Path
from strict_common import HERE, add_arguments, load, mode_of, number, ratchet

CLASSES = ['empty', 'nan', 'inf', 'neginf', 'negative', 'zeros', 'one', 'two', 'constant', 'ties', 'near-constant']
VALID = ['one', 'two', 'constant', 'ties', 'near-constant', 'zeros', 'negative']
NUMERICAL = {'no-convergence', 'evaluation', 'unresolved', 'tolerance', 'conditioning'}
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

def bound(text, held, names):
    """A support bound evaluated with held values only; None if it depends on a free coefficient."""
    text = str(text).strip()
    match = re.fullmatch(r'if\s*\((.*?)\)\s*(.*?)\s+else\s+(.*)', text)
    if match:
        condition = bound(match[1], held, names)
        return None if condition is None else bound(match[2] if condition else match[3], held, names)
    for name in sorted(names, key=len, reverse=True):   # coefficient names can be keywords (Tukey lambda)
        text = re.sub(r'\b%s\b' % re.escape(name), 'p_'+name, text)
    env = dict(floor=math.floor, min=min, max=max, Inf=math.inf, inf=math.inf)
    env.update({'p_'+k: v for k, v in held.items()})
    try: value = eval(text, {'__builtins__': {}}, env)
    except NameError: return None
    return value if isinstance(value, (int, float, bool)) else None

def outside(meta, held, data):
    """True if some observation lies beyond a support bound fixed by the held values."""
    s = meta['support']; closed = s.get('closed', [True, True])
    names = {p['name'] for p in meta['params']}
    lo, hi = bound(s['lower'], held, names), bound(s['upper'], held, names)
    for x in data:
        if lo is not None and (x < lo or (x == lo and not closed[0])): return True
        if hi is not None and (x > hi or (x == hi and not closed[1])): return True
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

def judge(rows, mode, table, probe, index):
    failures = {}; outcomes = collections.defaultdict(dict); judged = set()
    routes = fd.route_ids()
    by_family = collections.defaultdict(dict)
    for (family, source, case), r in rows.items():
        if source == 'class' and case in CLASSES: by_family[family][case] = r
    for family, cases in sorted(by_family.items()):
        ident = routes.get(family, family)
        fs.ROUTE_MODE = family in routes
        meta, golden = fs.read(ident)
        fs.ROUTE_MODE = False
        held = fs.golden_held(meta, golden)
        free = [p for p in meta['params'] if p['name'] not in held]
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
        if 'empty' in o and not is_failure('empty', 'invalid-input'):
            fail('empty', 'empty', 'ends in '+code(o['empty']))
        for label in ('nan', 'inf', 'neginf'):
            if label in o and not is_failure(label, 'invalid-input'): fail('non-finite', label, 'ends in '+code(o[label]))
        if o.get('one') and o.get('constant'):
            a, b = o['one'], o['constant']
            if (a['kind'], a['reason']) != (b['kind'], b['reason']):
                fail('one-constant', 'one', "'one' ends in %s, 'constant' in %s" % (code(a), code(b)))
        validity = {}
        for label in VALID:
            if label not in cases: continue
            data = [float(v) for v in cases[label]['data']]
            if outside(meta, held, data):
                if not is_failure(label, 'invalid-input'):
                    fail('outside-support', label, 'data beyond a fixed support bound end in '+code(o[label]))
                continue
            validity[label] = valid(cases[label])
            if validity[label] and o[label]['kind'] == 'failure' and o[label]['reason'] in ('invalid-input', 'invalid-argument'):
                fail('valid-data', label, 'valid observations end in '+code(o[label]))
        if validity.get('constant'):
            roles = {p.get('role') for p in free}
            c = o['constant']
            if not integer and 'location' in roles and 'scale' in roles and \
                    (c['kind'], c['reason']) != ('nonexistence', 'unbounded'):
                fail('collapse', 'constant', 'free location and scale, constant data end in '+code(c))
            if roles & {'scale', 'rate'} and c['kind'] == 'failure' and c['reason'] in NUMERICAL:
                fail('constant-scale', 'constant', 'free scale, constant data end in '+code(c))
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
