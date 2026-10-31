"""Check declared density relations, including mapped supports.

Continuous endpoint inclusion is compared modulo null sets; integer endpoint
inclusion is exact. Numerical evidence is a regression check, not a proof.
"""
from pathlib import Path
import json
import math
import random
import re
import sys
import sympy as sp
import yaml
from .density import density_expression

ROOT = Path(__file__).resolve().parents[3]
FAMILIES = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
SEED = 20261031
TRIALS = 24
TOL = 2e-9


def read_models():
    return {p.parent.name: yaml.safe_load(p.read_text().split('---', 2)[1])
            for p in sorted(FAMILIES.glob('*/README.md'))}


def expression(value, symbols):
    source=str(value).replace('^', '**')
    local={'inf': sp.oo, 'Inf': sp.oo, 'Min': sp.Min, 'Max': sp.Max}
    # Prefix coefficient identifiers, including Python keywords such as lambda.
    for name in sorted(symbols, key=len, reverse=True):
        alias='_relation_'+name
        source=re.sub(r'\b'+re.escape(name)+r'\b',alias,source)
        local[alias]=symbols[name]
    return sp.sympify(source, locals=local)


def bound(value, values):
    text = str(value)
    match = re.fullmatch(r'if \((.*?)\) (.*?) else (.*)', text)
    if match:
        cond, yes, no = match.groups()
        return bound(yes if bool(expression(cond, values)) else no, values)
    return float(expression(re.sub(r'\bmin\(', 'Min(', re.sub(r'\bmax\(', 'Max(', text)), values))


def support(meta, values):
    s = meta['support']
    return bound(s['lower'], values), bound(s['upper'], values)


def transform_x(edge, x, symbols):
    kind = edge.get('transform')
    if kind is None: return x
    if kind == 'log': return sp.log(x)
    if kind == 'logit': return sp.log(x / (1-x))
    if kind == 'inverse': return 1/x
    if kind == 'power': return x**expression(edge['power'], symbols)
    if kind == 'negate': return -x
    if kind == 'shift': return x-expression(edge['shift'], symbols)
    # abs is many-to-one and requires summing both inverse branches.
    if kind == 'abs': return x
    raise ValueError(f'unknown transform {kind}')


def mapped_support(edge, lo, hi, symbols):
    kind = edge.get('transform')
    if kind == 'abs':
        if lo != -math.inf or hi != math.inf: raise ValueError('abs requires full real parent support')
        return 0., math.inf
    if kind == 'log': return math.exp(lo), math.exp(hi) if hi != math.inf else math.inf
    if kind == 'logit':
        logistic = lambda v: 0. if v == -math.inf else 1. if v == math.inf else 1/(1+math.exp(-v))
        return logistic(lo), logistic(hi)
    if kind == 'inverse':
        if lo < 0: raise ValueError('inverse currently requires positive parent support')
        return (0. if hi == math.inf else 1/hi), (math.inf if lo == 0 else 1/lo)
    if kind == 'power':
        power = float(expression(edge['power'], symbols))
        if power <= 0 or lo < 0: raise ValueError('power requires positive exponent and nonnegative parent support')
        return lo**(1/power), hi**(1/power)
    if kind == 'negate': return -hi, -lo
    if kind == 'shift':
        shift = float(expression(edge['shift'], symbols))
        return lo+shift, hi+shift
    return lo, hi


def close(a, b):
    return a == b or (math.isfinite(a) and math.isfinite(b) and abs(a-b) <= TOL*(1+abs(a)+abs(b)))


def check_edge(child, parent, kind, edge):
    if kind == 'transform_of' and child['support']['type']=='integer' and edge['transform'] not in ('shift','negate'):
        raise ValueError('nonlinear integer transforms require a lattice proof')
    domains = [(-math.inf, math.inf)]*len(child['params'])
    syms, x, lhs = density_expression(FAMILIES/child['id'], child, domains, prefer_spec=False)
    names = dict(zip([p['name'] for p in child['params']], syms))
    ps, px, rhs = density_expression(FAMILIES/parent['id'], parent,
                                     [(-math.inf, math.inf)]*len(parent['params']), prefer_spec=False)
    if set(edge['at']) != {p['name'] for p in parent['params']}: raise ValueError('map must cover every parent coefficient')
    at = {s: expression(edge['at'][p['name']], names) for s,p in zip(ps,parent['params'])}
    tx = transform_x(edge, x, names) if kind == 'transform_of' else x
    rhs = rhs.subs(at, simultaneous=True)
    if edge.get('transform') == 'abs':
        rhs = sp.log(sp.exp(rhs.subs(px,x)) + sp.exp(rhs.subs(px,-x)))
    else:
        rhs = rhs.subs(px, tx) + sp.log(sp.Abs(sp.diff(tx,x)))
    # Avoid forced log identities without sign assumptions. Numerical checks
    # below always run, including when the symbolic residual is zero.
    residual = sp.simplify(lhs-rhs)
    symbolic = residual == 0
    rng = random.Random(SEED)
    for trial in range(TRIALS):
        values = {}
        for p in child['params']:
            v = float(p['simulate'])
            if not math.isfinite(v): raise ValueError('nonfinite simulation seed')
            if p.get('integer') or p.get('kind') == 'design': v = max(2, round(v)+rng.randint(0,4))
            elif p['role'] in ('location','threshold','endpoint') or p['name'] in ('min','max','mu','mean','meanlog'): v += rng.uniform(-.3,.3)
            elif v > 0: v *= rng.uniform(.65,1.6)
            values[p['name']] = sp.Float(v)
        # Maintain ordered endpoints; this is a seed neighbourhood, not full-domain coverage.
        if 'min' in values and 'max' in values and values['min'] >= values['max']: values['max']=values['min']+2
        if child['support']['type']=='integer' and edge.get('transform')=='shift':
            shift=float(expression(edge['shift'],values))
            if not shift.is_integer(): raise ValueError('integer shift must preserve the lattice')
        mapped = {k: expression(v, values) for k,v in edge['at'].items()}
        for p in parent['params']:
            interval=p['domain'].strip()[1:-1].split(',')
            a,b=(bound(v.strip(),mapped) for v in interval)
            v=float(mapped[p['name']])
            if v<a or v>b or (v==a and p['domain'][0]=='(') or (v==b and p['domain'][-1]==')'):
                raise ValueError('mapped coefficient outside parent domain: '+p['name'])
        clo,chi = support(child, values)
        plo,phi = mapped_support(edge, *support(parent,mapped), values)
        if child['support']['type'] != parent['support']['type'] or not(close(clo,plo) and close(chi,phi)):
            raise ValueError(f'support mismatch: {(clo,chi)} vs {(plo,phi)}')
        if child['support']['type']=='integer' and child['support']['closed'] != parent['support']['closed']:
            raise ValueError('integer endpoint inclusion differs')
        if math.isfinite(clo) and math.isfinite(chi): point=rng.uniform(clo,chi)
        elif math.isfinite(clo): point=clo+rng.uniform(.05,5)
        elif math.isfinite(chi): point=chi-rng.uniform(.05,5)
        else: point=rng.uniform(-4,4)
        if child['support']['type']=='integer': point=math.ceil(point)
        subs={names[k]:v for k,v in values.items()};subs[x]=point
        a=complex(lhs.evalf(35,subs=subs)); b=complex(rhs.evalf(35,subs=subs))
        if abs(a.imag)>TOL or abs(b.imag)>TOL or not(math.isfinite(a.real) and math.isfinite(b.real)) or not close(a.real,b.real):
            raise ValueError(f'log-density mismatch at {point}: {a} vs {b}')
    return 'verified symbolically' if symbolic else 'verified numerically'


def main():
    models=read_models(); results=[]
    for id,child in models.items():
        for kind,edges in child.get('relations',{}).items():
            for edge in edges:
                try: status=check_edge(child,models[edge['family']],kind,edge);detail=''
                except Exception as e: status='failed';detail=str(e)
                row=dict(child=id,parent=edge['family'],kind=kind,status=status,detail=detail)
                results.append(row);print(f"{id} -> {edge['family']} ({kind}): {status} {detail}",flush=True)
    path=ROOT/'build/quality/relations.json';path.parent.mkdir(parents=True,exist_ok=True)
    path.write_text(json.dumps(dict(seed=SEED,trials=TRIALS,tolerance=TOL,results=results),indent=2)+'\n')
    print(f'{len(results)} edges; '+str(sum(r['status']=='failed' for r in results))+' failed')
    return int(any(r['status']=='failed' for r in results))

if __name__ == '__main__': sys.exit(main())
