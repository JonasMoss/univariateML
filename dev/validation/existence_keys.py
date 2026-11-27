#!/usr/bin/env python3
"""Read and validate the strict-layer keys of a dossier's `existence` block.

The keys are documented in README_SCHEMA.md ("Strict-layer keys under
`existence`") and decided in dev/research/2026-11-14-strict-rule-questions.md.
fit-consistency.py reads them through read(); equivariance.py checks the
declared maps. Run as a script to validate every dossier:

  python3 dev/validation/existence_keys.py

Keys (all optional):
  equivariance       {group: affine | power-scale, map: {coefficient: expression
                     in A, B and coefficient names}, held: {coefficient: value},
                     evidence}. affine: x -> A + B x; power-scale: x -> A x^B
                     (A, B > 0 for power-scale, B > 0 for affine). The family
                     contains the image law with every mapped coefficient
                     replaced by its expression; unlisted coefficients are
                     unchanged. Mapped coefficients must be free; `held` lists
                     coefficients that must be held at these values.
  collapse           {at: any | minimum, exponent, infimum, attained, evidence}.
                     Collapse at a value of multiplicity k in a sample of size
                     n (any value, or the sample minimum) gives likelihood of
                     order scale^exponent; `infimum` is the exponent's infimum
                     over the fitting domain as an expression in n and k, and
                     `attained` says whether a fitting-domain member attains it.
  two_point          {outcome, unique, evidence}: the outcome of every sample of
                     two distinct observations (a family constant by
                     equivariance); `unique` for an estimate.
  min_n              {value, reason}: samples of size 2 <= n < value are not
                     implemented and end in failure(unsupported).
  strict_exceptions  [{class, outcome, argument}]: the outcome the dossier
                     proves for a fit-snapshot class where the strict rules'
                     default verdict does not decide it.

Outcomes are written estimate, limit, budget, nonexistence(<reason>) or
failure(<reason>), with reasons as in core/outcome.hpp.
"""
import re, sys
from pathlib import Path
import yaml

ROOT = Path(__file__).resolve().parents[2]
DISTS = ROOT/'univariateML-cpp/include/cppunivariate/distributions'
BASE = {'uniqueness', 'statement', 'reason'}
KEYS = {'equivariance', 'collapse', 'two_point', 'min_n', 'strict_exceptions'}
REASONS = {'nonexistence': {'unbounded', 'unattained'},
           'failure': {'invalid-input', 'invalid-argument', 'unsupported', 'no-convergence', 'evaluation',
                       'tolerance', 'conditioning', 'unresolved'}}
CLASSES = {'one', 'two', 'constant', 'ties', 'half-ties', 'tie3', 'near-constant', 'zeros', 'negative'}
SYMBOLS = {'n', 'k'}

class SchemaError(ValueError):
    pass

def limit_groups(meta):
    """Yield base and modifier limits with their dossier key paths."""
    yield 'limits', meta.get('limits', [])
    modifiers = meta.get('modifiers') or {}
    if 'truncate' in modifiers:
        yield 'modifiers.truncate.limits', modifiers['truncate'].get('limits', [])
    for kind in ('zero', 'censor'):
        for scheme, config in (modifiers.get(kind) or {}).items():
            if isinstance(config, dict) and 'limits' in config:
                yield f'modifiers.{kind}.{scheme}.limits', config['limits']

def outcome(text, where):
    """'failure(evaluation)' -> ('failure', 'evaluation'); 'estimate' -> ('estimate', 'none')."""
    m = re.fullmatch(r'(estimate|limit|budget|nonexistence|failure)(?:\((\S+)\))?', str(text).strip())
    if not m: raise SchemaError('%s: outcome %r is not kind or kind(reason)' % (where, text))
    kind, reason = m[1], m[2] or 'none'
    if (kind in REASONS) != (reason != 'none') or (kind in REASONS and reason not in REASONS[kind]):
        raise SchemaError('%s: outcome %r has no valid reason for its kind' % (where, text))
    return kind, reason

def _text(value, where):
    if not isinstance(value, str) or not value.strip(): raise SchemaError('%s must be nonempty text' % where)
    return value.strip()

def _names(expression):
    return set(re.findall(r'[A-Za-z_][A-Za-z_0-9]*', str(expression))) - {'log', 'exp', 'sqrt', 'abs'}

def read(meta):
    """The strict-layer keys of one dossier, validated; {} when none is declared."""
    ident = meta.get('id', '?')
    ex = meta.get('existence') or {}
    extra = set(ex) - BASE - KEYS
    if extra: raise SchemaError('%s: unknown existence keys %s' % (ident, sorted(extra)))
    names = {p['name'] for p in meta['params']}
    out = {}
    if 'equivariance' in ex:
        e = ex['equivariance']; w = ident+': existence.equivariance'
        if not isinstance(e, dict) or set(e) - {'group', 'map', 'held', 'evidence'}: raise SchemaError(w+': keys group, map, held, evidence')
        if e.get('group') not in ('affine', 'power-scale'): raise SchemaError(w+'.group must be affine or power-scale')
        if not isinstance(e.get('map'), dict) or not e['map']: raise SchemaError(w+'.map must map coefficients to expressions')
        if names & {'A', 'B'}: raise SchemaError(w+': coefficient names clash with the map constants A and B')
        for k, v in e['map'].items():
            if k not in names: raise SchemaError('%s.map: %s is not a coefficient' % (w, k))
            unknown = _names(v) - names - {'A', 'B'}
            if unknown: raise SchemaError('%s.map.%s uses unknown names %s' % (w, k, sorted(unknown)))
        held = e.get('held') or {}
        if not isinstance(held, dict) or set(held) - names or set(held) & set(e['map']):
            raise SchemaError(w+'.held must name unmapped coefficients')
        out['equivariance'] = dict(group=e['group'], map={k: str(v) for k, v in e['map'].items()},
                                   held={k: float(v) for k, v in held.items()}, evidence=_text(e.get('evidence'), w+'.evidence'))
    if 'collapse' in ex:
        c = ex['collapse']; w = ident+': existence.collapse'
        if not isinstance(c, dict) or set(c) - {'at', 'exponent', 'infimum', 'attained', 'evidence'}:
            raise SchemaError(w+': keys at, exponent, infimum, attained, evidence')
        if c.get('at') not in ('any', 'minimum'): raise SchemaError(w+'.at must be any or minimum')
        unknown = _names(c.get('exponent', '')) - names - SYMBOLS
        if not c.get('exponent') or unknown: raise SchemaError('%s.exponent must use n, k and coefficients (unknown %s)' % (w, sorted(unknown)))
        if not c.get('infimum') or _names(c['infimum']) - SYMBOLS: raise SchemaError(w+'.infimum must be an expression in n and k')
        if not isinstance(c.get('attained'), bool): raise SchemaError(w+'.attained must be true or false')
        out['collapse'] = dict(at=c['at'], exponent=str(c['exponent']), infimum=str(c['infimum']),
                               attained=c['attained'], evidence=_text(c.get('evidence'), w+'.evidence'))
    if 'two_point' in ex:
        t = ex['two_point']; w = ident+': existence.two_point'
        if not isinstance(t, dict) or set(t) - {'outcome', 'unique', 'evidence'}: raise SchemaError(w+': keys outcome, unique, evidence')
        kind, reason = outcome(t.get('outcome'), w)
        if (kind == 'estimate') != ('unique' in t) or ('unique' in t and not isinstance(t['unique'], bool)):
            raise SchemaError(w+'.unique (true or false) is required for an estimate and only there')
        if 'equivariance' not in out: raise SchemaError(w+' requires equivariance: only then is it a family constant')
        out['two_point'] = dict(kind=kind, reason=reason, unique=t.get('unique'), evidence=_text(t.get('evidence'), w+'.evidence'))
    if 'min_n' in ex:
        m = ex['min_n']; w = ident+': existence.min_n'
        if not isinstance(m, dict) or set(m) != {'value', 'reason'} or not isinstance(m['value'], int) or m['value'] < 2:
            raise SchemaError(w+': {value: integer >= 2, reason}')
        out['min_n'] = dict(value=m['value'], reason=_text(m['reason'], w+'.reason'))
    if 'strict_exceptions' in ex:
        w = ident+': existence.strict_exceptions'
        if not isinstance(ex['strict_exceptions'], list) or not ex['strict_exceptions']: raise SchemaError(w+' must be a nonempty list')
        seen = {}
        for i, s in enumerate(ex['strict_exceptions']):
            wi = '%s[%d]' % (w, i)
            if not isinstance(s, dict) or set(s) != {'class', 'outcome', 'argument'}: raise SchemaError(wi+': keys class, outcome, argument')
            if s['class'] not in CLASSES or s['class'] in seen: raise SchemaError(wi+'.class must be a distinct snapshot class')
            seen[s['class']] = dict(outcome=outcome(s['outcome'], wi), argument=_text(s['argument'], wi+'.argument'))
        out['strict_exceptions'] = seen
    if 'two_point' in out and 'min_n' in out and out['min_n']['value'] > 2:
        raise SchemaError(ident+': existence declares a two-point outcome and a minimum size above two')
    return out

def dossier(ident):
    return yaml.safe_load((DISTS/ident/'README.md').read_text().split('---', 2)[1])

def main():
    errors = 0; declared = {}
    for folder in sorted(DISTS.iterdir()):
        if not (folder/'README.md').exists(): continue
        try: keys = read(dossier(folder.name))
        except SchemaError as e: print('ERROR', e); errors += 1; continue
        for k in keys: declared.setdefault(k, []).append(folder.name)
    for k in sorted(KEYS): print('%-18s %2d: %s' % (k, len(declared.get(k, [])), ' '.join(declared.get(k, []))))
    sys.exit(1 if errors else 0)

if __name__ == '__main__': main()
