"""Shared declarative density reader (no distribution registry)."""
import importlib.util
import re
import sympy as sp


def density_expression(folder, meta, domains, prefer_spec=True):
    params = meta['params']
    id = folder.name
    syms = []
    source = meta['log_density']
    for p, (lo, hi) in zip(params, domains):
        assumptions = {'real': True}
        if lo >= 0:
            assumptions['positive' if lo > 0 or p['domain'][0] == '(' else 'nonnegative'] = True
        if p.get('integer'):
            assumptions['integer'] = True
        syms.append(sp.Symbol('p_' + p['name'], **assumptions))
    for p in sorted(params, key=lambda p: len(p['name']), reverse=True):
        source = re.sub('\\b' + p['name'] + '\\b', 'p_' + p['name'], source)
    x = sp.Symbol('x', real=True)
    local = {s.name: s for s in syms}
    local.update(x=x, k=sp.Symbol('k', integer=True, positive=True))
    if prefer_spec and (folder / 'spec.py').exists():
        spec = importlib.util.spec_from_file_location('quality_spec_' + id, folder / 'spec.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        expr = module.log_density(dict(zip([p['name'] for p in params], syms)), x)
    else:
        expr = sp.sympify(source, locals=local)
    return syms, x, expr
