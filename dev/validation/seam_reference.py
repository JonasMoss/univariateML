"""Exact-input density references, including attained and excluded faces.

These formulas are independent scalar laws, not calls into the C++ library.
"""
import math
import re
import sympy as sp
import mpmath as mp
from quality.truth import declarative, domains, module_at, number
from quality.truth_reference import normal

class InvalidFit(ValueError):pass
class ExcludedModel(ValueError):pass

def reference(folder, meta, theta, data):
    route=module_at(folder/'truth.py');t={k:number(v) for k,v in theta.items()}
    for p,(lo,hi) in zip(meta['params'],domains(meta,theta)):
        v=t[p['name']]
        if not lo<=v<=hi or (v==lo and p['domain'][0]=='(') or (v==hi and p['domain'][-1]==')') or (p.get('integer') and v!=mp.floor(v)):
            raise InvalidFit(f"{p['name']}={v} outside {p['domain']}")
    ident=folder.name
    # The symbolic density is an interior formula. Check support with exact
    # binary64 rationals before it can analytically continue past an endpoint.
    exact={key:(sp.oo if value==mp.inf else -sp.oo if value==-mp.inf else sp.Rational(float(value))) for key,value in t.items()}
    local={'min':sp.Min,'max':sp.Max,'floor':sp.floor,'ceiling':sp.ceiling,'ceil':sp.ceiling,'abs':sp.Abs,'Inf':sp.oo,'inf':sp.oo,**exact}
    def support_bound(value):
        text=str(value)
        conditional=re.fullmatch(r'if\s*\(([^()]*)\)\s*(.*?)\s+else\s+(.*)',text)
        if conditional:
            condition,yes,no=conditional.groups()
            return support_bound(yes if bool(sp.sympify(condition,locals=local)) else no)
        result=sp.sympify(text,locals=local)
        if result==sp.oo:return mp.inf
        if result==-sp.oo:return -mp.inf
        return mp.mpf(str(result.evalf(mp.mp.dps)))
    lo=support_bound(meta['support']['lower']);hi=support_bound(meta['support']['upper'])
    if any(number(x)<lo or number(x)>hi or (meta['support']['type']=='integer' and number(x)!=mp.floor(number(x))) for x in data):
        return -math.inf
    def log_interval(lo,hi):
        if lo>=hi:return -mp.inf
        if lo>=0:
            a,b=normal(lo)[1],normal(hi)[1]
        elif hi<=0:
            a,b=normal(hi)[0],normal(lo)[0]
        else:return mp.log((mp.erf(hi/mp.sqrt(2))-mp.erf(lo/mp.sqrt(2)))/2)
        return a if b==-mp.inf else a+mp.log(-mp.expm1(b-a))
    if ident=='bhatt':
        m,s,a=t['mu'],t['sigma'],t['a']
        if s==0:
            if a==0:raise ExcludedModel('zero-width point mass has no Lebesgue density')
            ld=lambda x:-mp.log(2*a) if abs(x-m)<=a else -mp.inf
        elif a==0:ld=lambda x:-mp.log(s)-mp.log(2*mp.pi)/2-((x-m)/s)**2/2
        else:ld=lambda x:log_interval((x-m-a)/s,(x-m+a)/s)-mp.log(2*a)
    elif ident=='emg':
        m,s,rate=t['mu'],t['sigma'],t['lambda']
        if s==0:ld=lambda x:mp.log(rate)-rate*(x-m) if x>=m else -mp.inf
        else:ld=lambda x:mp.log(rate)+rate*(m-x)+(rate*s)**2/2+mp.log(mp.erfc((m+rate*s*s-x)/(s*mp.sqrt(2))))-mp.log(2)
    else:
        ld=declarative(folder,meta,theta,route)
    value=mp.fsum(ld(number(x)) for x in data)
    if mp.isnan(value) or isinstance(value,mp.mpc):
        raise ArithmeticError('independent density is unresolved or undefined at these inputs')
    return float(value)
