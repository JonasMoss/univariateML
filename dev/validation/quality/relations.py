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
    local={'inf': sp.oo, 'Inf': sp.oo, 'Min': sp.Min, 'Max': sp.Max,
           'min': sp.Min, 'max': sp.Max, 'floor': sp.floor, 'ceiling': sp.ceiling, 'expit': lambda x: 1/(1+sp.exp(-x))}
    # Prefix coefficient identifiers, including Python keywords such as lambda.
    for name in sorted(symbols, key=len, reverse=True):
        alias='_relation_'+name
        source=re.sub(r'\b'+re.escape(name)+r'\b',alias,source)
        local[alias]=symbols[name]
    return sp.sympify(source, locals=local)


def relation_symbols(model):
    """Use only sign assumptions explicitly guaranteed by the domains."""
    result = {}
    for p in model['params']:
        assumptions = dict(real=True)
        if p['domain'].strip().split(',')[0].strip() == '(0':
            assumptions['positive'] = True
        if p.get('integer'):
            assumptions['integer'] = True
        result[p['name']] = sp.Symbol('relation_'+p['name'], **assumptions)
    return result


def special_path(models, start, target, excluded_pair=None):
    """A nonempty special-case path, excluding all parallel direct edges."""
    queue = [(start, [start])]
    seen = {start}
    for node, path in queue:
        for edge in models[node].get('relations', {}).get('special_case_of', []):
            dest = edge['family']
            if edge.get('when') or (node, dest) == excluded_pair:
                continue
            if dest == target:
                return path+[dest]
            if dest not in seen:
                seen.add(dest)
                queue.append((dest, path+[dest]))
    return None


def transform_signature(edge, coordinates):
    transform = edge['transform']
    argument = (expression(edge['power'], coordinates) if transform == 'power' else
                expression(edge.get('shift', 0), coordinates) if transform in ('shift','abs') else None)
    return transform, argument


def same_expression(a, b):
    return a == b or (a is not None and b is not None and sp.simplify(a-b) == 0)


def transform_derivation(models, child, index, edge, relation_kind="transform_of"):
    """Find a family path with one same-kind transform and >=1 special case.

    This is the declaration graph's policy: alternative coefficient maps or
    power/shift arguments do not warrant another arrow of the same type.
    The separate density check validates the retained coefficient map.
    """
    queue = [(child, False, [child])]
    seen = {(child, False)}
    for node, transformed, path in queue:
        for kind in ('special_case_of', relation_kind):
            if kind == relation_kind and transformed:
                continue
            for i, step in enumerate(models[node].get('relations', {}).get(kind, [])):
                if step.get('when') or (node, kind, i) == (child, relation_kind, index):
                    continue
                if kind == 'transform_of' and step.get('transform') != edge.get('transform'):
                    continue
                if kind == 'convolution_of' and (step.get('operation') != edge.get('operation') or
                        [c['family'] for c in step.get('components',[])[1:]] != [c['family'] for c in edge.get('components',[])[1:]]):
                    continue
                if kind == 'mixture_of' and (step.get('mixing',{}).get('family'),step.get('mixed')) != (edge.get('mixing',{}).get('family'),edge.get('mixed')):
                    continue
                used = transformed or kind == relation_kind
                next_path = path+[step['family']]
                if step['family'] == edge['family'] and used and len(next_path) >= 3:
                    return next_path
                state = (step['family'], used)
                if state in seen:
                    continue
                seen.add(state)
                queue.append((step['family'], used, next_path))
    return None


def mixture_derivation(models, child, edge):
    """Compose one shift/special-case, one mixture, and a parent submodel.

    Unlike the coarse graph path check, this pattern compares every composed
    coefficient map, the mixing law and the additive shift symbolically.
    It catches Yule--Simon -> beta-NB -> NB(size=1) = geometric.
    """
    coordinates=relation_symbols(models[child]);coordinates['mix']=sp.Symbol('relation_mix',positive=True)
    for kind in ('special_case_of','transform_of'):
        for step in models[child].get('relations',{}).get(kind,[]):
            if step.get('when') or (kind=='transform_of' and step['transform']!='shift'): continue
            middle=step['family']; mapping={k:expression(v,coordinates) for k,v in step['at'].items()}
            mapping['mix']=coordinates['mix']
            shift=expression(step.get('shift',0),coordinates)
            for mix in models[middle].get('relations',{}).get('mixture_of',[]):
                if mix.get('when') or mix['mixing']['family']!=edge['mixing']['family']: continue
                def same_maps(left,right):
                    return set(left)==set(right) and all(sp.simplify(left[k]-right[k])==0 for k in left)
                target_mixing={k:expression(v,coordinates) for k,v in edge['mixing']['at'].items()}
                composed_mixing={k:expression(v,mapping) for k,v in mix['mixing']['at'].items()}
                if not same_maps(target_mixing,composed_mixing):continue
                if sp.simplify(shift+expression(mix.get('shift',0),mapping)-expression(edge.get('shift',0),coordinates))!=0:continue
                composed_parent={k:expression(v,mapping) for k,v in mix['at'].items()}
                target_parent={k:expression(v,coordinates) for k,v in edge['at'].items()}
                candidates=([dict(family=edge['family'],at={k:k for k in target_parent})] if edge['family']==mix['family'] else [])
                candidates+=models[edge['family']].get('relations',{}).get('special_case_of',[])
                for parent_step in candidates:
                    if parent_step.get('when') or parent_step['family']!=mix['family']:continue
                    lifted={k:expression(v,target_parent) for k,v in parent_step['at'].items()}
                    if same_maps(lifted,composed_parent):return [child,middle,mix['family'],edge['family']]
    return None


def declaration_errors(models):
    """Return per-edge violations of the immediate-declaration policy."""
    errors = {}
    for child, model in models.items():
        seen = set()
        symbols = relation_symbols(model)
        for kind, edges in model.get('relations', {}).items():
            for i, edge in enumerate(edges):
                ref = (child, kind, i)
                key = (kind, edge['family'], edge.get('transform'), edge.get('when'),
                       edge.get('operation'), tuple(c['family'] for c in edge.get('components',[])),
                       edge.get('mixing',{}).get('family'), edge.get('mixed'))
                messages = []
                if key in seen:
                    messages.append('duplicate declaration for family/kind/transform')
                seen.add(key)
                if kind == 'special_case_of':
                    path = special_path(models, child, edge['family'], (child, edge['family']))
                    if path:
                        messages.append('derivable special case: '+' -> '.join(path))
                    if special_path(models, edge['family'], child):
                        messages.append('cycle in special_case_of declarations')
                elif kind == 'transform_of':
                    transform, argument = transform_signature(edge, symbols)
                    if (transform == 'power' and same_expression(argument, sp.Integer(1)) or
                            transform == 'shift' and same_expression(argument, sp.Integer(0))):
                        messages.append('identity transform')
                    if transform in ('inverse','negate') and any(
                            other['family'] == child and other['transform'] == transform
                            for other in models[edge['family']].get('relations', {}).get('transform_of', [])):
                        messages.append('both directions of involutive '+transform+' transform')
                    path = transform_derivation(models, child, i, edge)
                    if path:
                        messages.append('derivable transform: '+' -> '.join(path))
                elif kind in ('discretisation_of', 'mixture_of', 'convolution_of'):
                    path = transform_derivation(models, child, i, edge, kind)
                    if path:
                        messages.append('derivable construction: '+' -> '.join(path))
                if kind == 'mixture_of':
                    path=mixture_derivation(models,child,edge)
                    if path: messages.append('derivable mapped mixture: '+' -> '.join(path))
                if kind == 'transform_of':
                    reverse = {'exp':'log','log':'exp','expit':'logit','logit':'expit'}
                    if edge['transform'] in reverse and any(other['family']==child and other['transform']==reverse[edge['transform']]
                            for other in models[edge['family']].get('relations',{}).get('transform_of',[])):
                        messages.append('both directions of inverse transform')
                if messages:
                    errors[ref] = '; '.join(messages)
    return errors


def bound(value, values, substitutions=None):
    text = str(value)
    match = re.fullmatch(r'if \((.*?)\) (.*?) else (.*)', text)
    if match:
        cond, yes, no = match.groups()
        condition = expression(cond, values)
        if substitutions is not None: condition = sp.simplify(condition).subs(substitutions)
        return bound(yes if bool(condition) else no, values, substitutions)
    result = expression(re.sub(r'\bmin\(', 'Min(', re.sub(r'\bmax\(', 'Max(', text)), values)
    if substitutions is not None: result = sp.simplify(result).subs(substitutions)
    return float(result)


def support(meta, values, substitutions=None):
    s = meta['support']
    return bound(s['lower'], values, substitutions), bound(s['upper'], values, substitutions)


def transform_x(edge, x, symbols):
    kind = edge.get('transform')
    if kind is None: return x
    if kind == 'exp': return sp.exp(x)
    if kind == 'expit': return 1/(1+sp.exp(-x))
    if kind == 'affine': return (x-expression(edge['shift'], symbols))/expression(edge['scale'], symbols)
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
        return float(expression(edge.get('shift',0), symbols)), math.inf
    if kind == 'exp': return (math.log(lo) if lo > 0 else -math.inf), (math.log(hi) if hi != math.inf else math.inf)
    if kind == 'expit': return (math.log(lo/(1-lo)) if lo > 0 else -math.inf), (math.log(hi/(1-hi)) if hi < 1 else math.inf)
    if kind == 'affine':
        scale=float(expression(edge['scale'],symbols)); shift=float(expression(edge['shift'],symbols))
        if scale <= 0: raise ValueError('affine scale must be positive')
        return lo*scale+shift, hi*scale+shift
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


def skellam_boundary(parent, ps, x, rhs, at):
    """Continuous Bessel extension on a fixed zero-rate face.

    I_n(2 sqrt(a b)) ~ (a b)^(n/2)/Gamma(n+1) gives the
    signed Poisson law; two zero rates give the atom at zero.
    """
    if parent['id'] != 'skellam' or not (at[ps[0]] == 0 or at[ps[1]] == 0):
        return parent, rhs
    a, b = ps
    expected = -a-b+x*sp.log(a/b)/2+sp.log(sp.besseli(sp.Abs(x), 2*sp.sqrt(a*b)))
    if sp.simplify(rhs-expected) != 0:
        raise ValueError('unrecognized Skellam density for boundary extension')
    positive = at[ps[1]] == 0
    rate, count = (ps[0], x) if positive else (ps[1], -x)
    rhs = sp.Piecewise((0, sp.And(sp.Eq(rate, 0), sp.Eq(count, 0))),
                       (-sp.oo, sp.Eq(rate, 0)),
                       (-rate+count*sp.log(rate)-sp.loggamma(count+1), True))
    bounds = dict(lower=0, upper='inf', closed=[True, False]) if positive else dict(
        lower='-inf', upper=0, closed=[False, True])
    if at[ps[0]] == 0 and at[ps[1]] == 0:
        bounds = dict(lower=0, upper=0, closed=[True, True])
    return dict(parent, support=dict(parent['support'], **bounds)), rhs


def check_edge(child, parent, kind, edge):
    if kind in ('convolution_of','mixture_of','discretisation_of'):
        return check_construction(child, parent, kind, edge)
    if kind not in ('special_case_of','transform_of','truncation_of','modification_of'):
        raise ValueError('unknown relation kind')
    if kind == 'modification_of':
        if edge.get('type') not in ('zero-truncated','zero-modified','zero-inflated'):
            raise ValueError('unknown modification type')
        if child['support']['type'] != 'integer' or parent['support']['type'] != 'integer':
            raise ValueError('zero modifications require integer laws')
    if kind in ('truncation_of','modification_of') and child['support']['type'] != parent['support']['type']:
        raise ValueError('conditional relation measure mismatch')
    if kind == 'transform_of' and child['support']['type']=='integer' and edge['transform'] not in ('shift','negate'):
        raise ValueError('nonlinear integer transforms require a lattice proof')
    domains = [(-math.inf, math.inf)]*len(child['params'])
    syms, x, lhs = density_expression(FAMILIES/child['id'], child, domains, prefer_spec=False)
    names = dict(zip([p['name'] for p in child['params']], syms))
    if child['id'] == 'nhyper':
        # The attained r=0 stopping time is identically zero; the generic
        # gamma quotient only describes positive stopping counts.
        lhs = sp.Piecewise((0, sp.And(sp.Eq(names['r'], 0), sp.Eq(x, 0))),
                           (-sp.oo, sp.Eq(names['r'], 0)), (lhs, True))
    ps, px, rhs = density_expression(FAMILIES/parent['id'], parent,
                                     [(-math.inf, math.inf)]*len(parent['params']), prefer_spec=False)
    if set(edge['at']) != {p['name'] for p in parent['params']}: raise ValueError('map must cover every parent coefficient')
    at = {s: expression(edge['at'][p['name']], names) for s,p in zip(ps,parent['params'])}
    parent, rhs = skellam_boundary(parent, ps, px, rhs, at)
    if parent['id'] in ('zmbinom','zmgeom','zmnbinom','zmpois','zmlogarithmic'):
        if at[ps[[p['name'] for p in parent['params']].index('p0')]] == 0:
            parent = dict(parent, support=dict(parent['support'], lower=1))
    tx = transform_x(edge, x, names) if kind == 'transform_of' else x
    # The attained a=0 Bhattacharjee member has a removable 0/0 in
    # its erf quotient. Derive its continuous extension from the density,
    # rather than replacing it with the candidate child's formula.
    if parent['id'] == 'bhatt' and at[ps[2]] == 0:
        rhs = sp.log(sp.limit(sp.exp(rhs), ps[2], 0, dir='+'))
    if parent['id'] in ('ztnbinom', 'zmnbinom') and at[ps[0]] == 0:
        # Gamma(r)*(1-p**r) -> -log(p) as r -> 0+. Remove
        # these two cancelling singular log terms before substitution.
        # For zero modification, keep the separate atom unchanged.
        size, prob = ps[:2]
        positive = rhs if parent['id'] == 'ztnbinom' else rhs.args[1].expr
        regular = positive + sp.loggamma(size) + sp.log(1-prob**size)
        boundary = regular.subs(size, 0) - sp.log(-sp.log(prob))
        rhs = (boundary if parent['id'] == 'ztnbinom' else
               sp.Piecewise((rhs.args[0].expr, sp.Eq(px, 0)), (boundary, True)))
    rhs = rhs.subs(at, simultaneous=True)
    if parent['id'] == 'cmp' and at[ps[1]] in (0, 1):
        # SymPy 1.14 Sum.evalf(subs=...) can omit the normalizer;
        # resolve these exact geometric/exponential series first.
        rhs = rhs.doit()
    # Tukey lambda's dossier writes its density in u=F(x). At lambda=0
    # the quantile is mu+sigma*log(u/(1-u)); invert it exactly. At
    # lambda=1 or 2 the density is already independent of u. Other
    # shapes still require an implicit inverse and are not certified here.
    if parent['id'] == 'tlambda' and at[ps[2]] == 0:
        mu, sigma = at[ps[0]], at[ps[1]]
        rhs = rhs.subs(sp.Symbol('u'), 1/(1+sp.exp(-(px-mu)/sigma)))
    if edge.get('transform') == 'abs':
        rhs = sp.log(sp.exp(rhs.subs(px,x-expression(edge.get('shift',0),names))) + sp.exp(rhs.subs(px,-x+expression(edge.get('shift',0),names))))
    else:
        rhs = rhs.subs(px, tx) + sp.log(sp.Abs(sp.diff(tx,x)))
    # Avoid forced log identities without sign assumptions. Numerical checks
    # below always run, including when the symbolic residual is zero.
    conditional = kind in ('truncation_of', 'modification_of')
    residual = sp.simplify(lhs-rhs) if not conditional else sp.nan
    symbolic = residual == 0
    rng = random.Random(SEED)
    trial = 0
    for attempt in range(20*TRIALS):
        values = {}
        for p in child['params']:
            v = float(p['simulate'])
            if not math.isfinite(v):
                if conditional and p['name'] in ('a','b'): v = -1.5 if p['name']=='a' else 2.5
                else: raise ValueError('nonfinite simulation seed')
            lattice = p.get('integer') or (p.get('kind') == 'design' and child['support']['type']=='integer')
            if attempt == 0: v = round(v) if lattice else v
            elif lattice: v = max(2, round(v)+rng.randint(0,4))
            elif p['role'] in ('location','threshold','endpoint') or p['name'] in ('min','max','mu','mean','meanlog'): v += rng.uniform(-.3,.3)
            elif v > 0: v *= rng.uniform(.65,1.6)
            values[p['name']] = sp.Integer(v) if lattice else sp.Float(v)
        if conditional and 'a' in values and 'b' in values and values['a'] >= values['b']: values['b']=values['a']+3
        # Maintain ordered endpoints; this is a seed neighbourhood, not full-domain coverage.
        if 'min' in values and 'max' in values and values['min'] >= values['max']: values['max']=values['min']+2
        # Dependent child domains (e.g. r<=m<=N) must hold before
        # testing a map. Count accepted draws, never skipped invalid ones.
        valid = True
        for p in child['params']:
            interval = p['domain'].strip()[1:-1].split(',')
            a, b = (bound(v.strip(), values) for v in interval)
            v = float(values[p['name']])
            if v<a or v>b or (v==a and p['domain'][0]=='(') or (v==b and p['domain'][-1]==')'):
                valid = False
                break
        if not valid or (edge.get('when') is not None and not condition(edge['when'],values)):
            continue
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
        # Cancel mapped endpoint identities before floating evaluation.
        # In particular mu-sigma/xi may be exactly zero on a GEV image;
        # a rounded negative residue must not invalidate an inverse map.
        symbolic_map = {p['name']: at[s] for s,p in zip(ps,parent['params'])}
        child_subs = {names[k]:v for k,v in values.items()}
        plo,phi = mapped_support(edge, *support(parent,symbolic_map,child_subs), values)
        if conditional:
            if child['support']['type']=='integer' and (not child['support']['closed'][0] or (math.isfinite(chi) and not child['support']['closed'][1])):
                raise ValueError('integer conditioning endpoints must be included')
            if kind == 'truncation_of':
                if clo < plo or chi > phi or clo > chi or (clo == chi and child['support']['type']!='integer'): raise ValueError('truncation support is not a nonempty parent subset')
            else:
                expected_lo = max(1,plo) if edge['type']=='zero-truncated' else 0
                if not close(clo,expected_lo) or not close(chi,phi): raise ValueError('modification support mismatch')
        elif child['support']['type'] != parent['support']['type'] or not(close(clo,plo) and close(chi,phi)):
            raise ValueError(f'support mismatch: {(clo,chi)} vs {(plo,phi)}')
        parent_closed = list(parent['support']['closed'])
        if edge.get('transform') == 'negate':
            parent_closed.reverse()
        if not conditional and child['support']['type']=='integer' and child['support']['closed'] != parent_closed:
            raise ValueError('integer endpoint inclusion differs')
        if math.isfinite(clo) and math.isfinite(chi): point=rng.uniform(clo,chi)
        elif math.isfinite(clo): point=clo+rng.uniform(.05,5)
        elif math.isfinite(chi): point=chi-rng.uniform(.05,5)
        else: point=rng.uniform(-4,4)
        if child['support']['type']=='integer': point=rng.randint(math.ceil(clo), int(min(chi, max(clo+5,10))))
        if kind == 'modification_of' and edge['type']!='zero-truncated' and trial % 2 == 0: point=0
        subs={names[k]:v for k,v in values.items()};subs[x]=point
        a=complex(lhs.subs(subs).doit().evalf(35)) if conditional else complex(lhs.evalf(35,subs=subs))
        if conditional:
            import mpmath as mp
            with mp.workdps(35):
                density=sp.lambdify(px,sp.exp(rhs.subs({names[k]:v for k,v in values.items()})),modules='mpmath')
                if kind == 'truncation_of':
                    mass = ((mp.fsum(density(k) for k in range(math.ceil(clo),math.floor(chi)+1)) if math.isfinite(chi) else mp.nsum(density,[math.ceil(clo),mp.inf]))
                            if child['support']['type']=='integer' else mp.quad(density,[clo,chi]))
                    if not mp.isfinite(mass) or mass <= 0: raise ValueError('invalid conditioning mass')
                    b=complex(mp.log(density(point)/mass))
                else:
                    zero = density(0) if plo <= 0 else mp.mpf(0)
                    variant=edge['type']
                    weight=mp.mpf(str(expression(edge['mass'],values))) if variant!='zero-truncated' else mp.mpf(0)
                    if not 0 <= weight <= 1: raise ValueError('modification mass outside [0,1]')
                    if variant=='zero-inflated':
                        mass=weight+(1-weight)*zero if point==0 else (1-weight)*density(point)
                    else:
                        mass=weight if point==0 else (1-weight)*density(point)/(1-zero)
                    b=complex(mp.log(mass))
        else: b=complex(rhs.evalf(35,subs=subs))
        if abs(a.imag)>TOL or abs(b.imag)>TOL or not(math.isfinite(a.real) and math.isfinite(b.real)) or not close(a.real,b.real):
            raise ValueError(f'log-density mismatch at {point}: {a} vs {b}')
        trial += 1
        if trial == TRIALS:
            break
    else:
        raise ValueError('could not draw 24 valid child parameter values')
    return 'verified symbolically' if symbolic else 'verified numerically'


def condition(source, values):
    """Evaluate scalar R-style boolean syntax without losing numeric equality."""
    import ast
    import operator
    source=str(source).replace('&&',' and ').replace('||',' or ')
    tree=ast.parse(source,mode='eval')
    comparisons={ast.Lt:operator.lt,ast.LtE:operator.le,ast.Gt:operator.gt,
                 ast.GtE:operator.ge,ast.Eq:operator.eq,ast.NotEq:operator.ne}
    def evaluate(node):
        if isinstance(node,ast.BoolOp):
            parts=[evaluate(v) for v in node.values]
            return all(parts) if isinstance(node.op,ast.And) else any(parts)
        if isinstance(node,ast.Compare):
            nodes=[node.left]+node.comparators
            numbers=[float(expression(ast.get_source_segment(source,n),values)) for n in nodes]
            return all(comparisons[type(op)](a,b) for op,a,b in zip(node.ops,numbers,numbers[1:]))
        raise ValueError('condition must be scalar logical comparisons')
    return evaluate(tree.body)


def valid_parameters(model, values):
    for p in model['params']:
        v=float(values[p['name']]); a,b=(bound(t.strip(),values) for t in p['domain'][1:-1].split(','))
        if not math.isfinite(v) or v<a or v>b or (v==a and p['domain'][0]=='(') or (v==b and p['domain'][-1]==')'):
            return False
        if p.get('integer') and not v.is_integer(): return False
    return True


def numerical_density(model, values):
    import mpmath as mp
    if set(values) != {p['name'] for p in model['params']}:
        raise ValueError('map must cover every parent coefficient')
    if not valid_parameters(model, values): raise ValueError('mapped coefficient outside parent domain')
    syms,x,logf=density_expression(FAMILIES/model['id'],model,[(-math.inf,math.inf)]*len(model['params']),prefer_spec=False)
    expr=logf.subs(dict(zip(syms,[values[p['name']] for p in model['params']])), simultaneous=True)
    f=sp.lambdify(x,sp.exp(expr),modules='mpmath')
    lo,hi=support(model,values)
    def density(t):
        if t<lo or t>hi: return mp.mpf(0)
        # Poisson's attained zero rate has a removable 0*log(0).
        if model['id']=='pois' and float(values['lambda'])==0: return mp.mpf(t==0)
        return f(t)
    return density,lo,hi


def check_construction(child, parent, kind, edge):
    """Direct independent convolution / continuous mixing / unit-cell integration.

    24 seeded accepted parameter draws; 35-digit quadrature and infinite sums.
    This is numerical identity evidence, not validated interval quadrature.
    """
    import mpmath as mp
    models=read_models(); rng=random.Random(SEED); accepted=0
    with mp.workdps(35):
        for attempt in range(20*TRIALS):
            values={}
            for p in child['params']:
                v=float(p['simulate'])
                lattice=p.get('integer') or (p.get('kind')=='design' and child['support']['type']=='integer')
                if attempt:
                    if lattice: v=round(v)+rng.randint(0,4)
                    elif v>0: v*=rng.uniform(.65,1.6)
                    else: v+=rng.uniform(-.3,.3)
                values[p['name']]=sp.Integer(round(v)) if lattice else sp.Float(v)
            if not valid_parameters(child,values) or (edge.get('when') and not condition(edge['when'],values)): continue
            lhs,lo,hi=numerical_density(child,values)
            point=rng.uniform(max(-3,lo),min(3,hi)) if math.isfinite(lo) and math.isfinite(hi) else (lo+rng.uniform(.2,4) if math.isfinite(lo) else rng.uniform(-3,3))
            if child['support']['type']=='integer': point=rng.randint(int(max(0,lo)) if math.isfinite(lo) else -3,int(min(hi,6)) if math.isfinite(hi) else 6)
            def mapped(spec, extra=None):
                names=dict(values,**(extra or {}))
                return numerical_density(models[spec['family']],{k:expression(v,names) for k,v in spec['at'].items()})
            if kind=='discretisation_of':
                if child['support']['type']!='integer' or parent['support']['type']!='real': raise ValueError('discretisation measure mismatch')
                f,a,b=mapped(edge)
                if (math.floor(a) if math.isfinite(a) else a)!=lo or (math.ceil(b)-1 if math.isfinite(b) else b)!=hi: raise ValueError('discretisation support mismatch')
                rhs=mp.quad(f,[point,point+1])
            elif kind=='mixture_of':
                mixing=edge['mixing']; mf,a,b=mapped(mixing)
                if models[mixing['family']]['support']['type']!='real': raise ValueError('mixing family must be continuous')
                mixed=edge['mixed']; names=[p['name'] for p in parent['params']]
                if mixed not in names or 'mix' not in str(edge['at'][mixed]): raise ValueError('mixed coefficient must depend on mix')
                if any('mix' in str(v) for k,v in edge['at'].items() if k!=mixed): raise ValueError('only mixed coefficient may depend on mix')
                shift=float(expression(edge.get('shift',0),values))
                # Compile the conditional density once; validate mapped domains
                # at every quadrature node, including random-scale maps.
                ps,x,lf=density_expression(FAMILIES/parent['id'],parent,[(-math.inf,math.inf)]*len(names),prefer_spec=False)
                mix=sp.Symbol('mix',positive=True); env=dict(values,mix=mix)
                at={p:expression(edge['at'][n],env) for p,n in zip(ps,names)}
                fn=sp.lambdify((x,mix),sp.exp(lf.subs(at,simultaneous=True)),modules='mpmath')
                def integrand(u):
                    vals={n:expression(edge['at'][n],dict(values,mix=sp.Float(str(u),35))) for n in names}
                    if not valid_parameters(parent,vals): raise ValueError('mapped coefficient outside parent domain')
                    plo,phi=support(parent,vals)
                    if not(close(lo,plo+shift) and close(hi,phi+shift)): raise ValueError('mixture support mismatch')
                    if child['support']['type']!=parent['support']['type'] or (child['support']['type']=='integer' and not shift.is_integer()): raise ValueError('mixture measure or lattice mismatch')
                    return fn(point-shift,u)*mf(u)
                rhs=mp.quad(integrand,[a,b])
            else:
                components=edge['components']
                if len(components)!=2 or edge['family']!=components[0]['family'] or edge['operation'] not in ('sum','difference'): raise ValueError('convolution requires two components and sum/difference')
                f,a,b=mapped(components[0]);g,c,d=mapped(components[1]); operation=edge['operation']
                offset=float(expression(edge.get('shift',0),values)); z=point-offset
                discrete=models[components[0]['family']]['support']['type']=='integer'
                if discrete and not offset.is_integer(): raise ValueError('convolution shift must preserve lattice')
                if models[components[1]['family']]['support']['type']!=('integer' if discrete else 'real') or child['support']['type']!=('integer' if discrete else 'real'): raise ValueError('convolution measure mismatch')
                expected=(a+c+offset,b+d+offset) if operation=='sum' else (a-d+offset,b-c+offset)
                if not(close(lo,expected[0]) and close(hi,expected[1])): raise ValueError('convolution support mismatch')
                lower=max(a,z-d) if operation=='sum' else max(a,z+c)
                upper=min(b,z-c) if operation=='sum' else min(b,z+d)
                integrand=lambda t:f(t)*g(z-t if operation=='sum' else t-z)
                rhs=mp.nsum(integrand,[math.ceil(lower) if math.isfinite(lower) else -mp.inf,math.floor(upper) if math.isfinite(upper) else mp.inf]) if discrete else mp.quad(integrand,[lower,upper])
            actual=lhs(point)
            if not (mp.isfinite(rhs) and rhs>0 and actual>0) or abs(mp.log(actual)-mp.log(rhs))>mp.mpf('2e-8')*(1+abs(mp.log(actual))+abs(mp.log(rhs))):
                raise ValueError(f'construction log-density mismatch at {point}: {actual} vs {rhs}')
            accepted+=1
            if accepted==TRIALS: return 'verified numerically'
    raise ValueError('could not draw 24 valid child parameter values')


def main():
    models=read_models(); results=[]
    errors=declaration_errors(models)
    for id,child in models.items():
        for kind,edges in child.get('relations',{}).items():
            for i,edge in enumerate(edges):
                try:
                    if (id,kind,i) in errors:
                        raise ValueError(errors[(id,kind,i)])
                    status=check_edge(child,models[edge['family']],kind,edge);detail=''
                except Exception as e: status='failed';detail=str(e)
                row=dict(child=id,parent=edge['family'],kind=kind,edge=edge,status=status,detail=detail)
                results.append(row);print(f"{id} -> {edge['family']} ({kind}): {status} {detail}",flush=True)
    path=ROOT/'build/quality/relations.json';path.parent.mkdir(parents=True,exist_ok=True)
    path.write_text(json.dumps(dict(seed=SEED,trials=TRIALS,tolerance=TOL,construction_tolerance=2e-8,working_precision=35,results=results),indent=2)+'\n')
    print(f'{len(results)} edges; '+str(sum(r['status']=='failed' for r in results))+' failed')
    return int(any(r['status']=='failed' for r in results))

if __name__ == '__main__': sys.exit(main())
