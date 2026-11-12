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
    if ident=='tbinom':
        lo=max(0,mp.floor(t['a'])+1);hi=min(t['size'],mp.floor(t['b']))
        if lo>hi:raise InvalidFit('empty conditional support')
        if (t['prob']==0 and lo>0) or (t['prob']==1 and hi<t['size']):
            raise ExcludedModel('conditioning event has probability zero; dossier excludes its weak limit')
    if ident=='tpois':
        lower=max(mp.mpf(0),mp.floor(t['a'])+1);upper=mp.floor(t['b'])
        if lower>upper:raise InvalidFit('empty conditional support')
        if t['lambda']==0 and lower>0:
            raise ExcludedModel('conditioning event has probability zero; dossier excludes its weak limit')
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
    if ident=='tpois':
        rate=t['lambda']
        if rate==0:ld=lambda x:mp.mpf(0) if x==0 else -mp.inf
        else:
            center=max(lower,mp.floor(rate)) if upper==mp.inf else min(upper,max(lower,mp.floor(rate)))
            total=mp.mpf(1)
            for direction in (-1,1):
                k=center;term=mp.mpf(1)
                for iteration in range(200000):
                    if (direction<0 and k<=lower) or (direction>0 and k>=upper):break
                    ratio=k/rate if direction<0 else rate/(k+1)
                    term*=ratio;k+=direction;total+=term
                    next_ratio=k/rate if direction<0 else rate/(k+1)
                    if next_ratio<1 and term*next_ratio/(1-next_ratio)<total*mp.mpf('1e-70'):break
                else:raise ArithmeticError('conditional Poisson positive-sum remainder after 200000 terms')
            log_mass=center*mp.log(rate)-mp.loggamma(center+1)+mp.log(total)
            ld=lambda x:x*mp.log(rate)-mp.loggamma(x+1)-log_mass
    elif ident=='bhatt':
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
    elif ident=='tnorm':
        m,s,a,b=t['mean'],t['sd'],t['a'],t['b']
        if s==0:raise ExcludedModel('zero-scale point mass has no Lebesgue density')
        z=log_interval((a-m)/s,(b-m)/s)
        ld=lambda x:-mp.log(s)-mp.log(2*mp.pi)/2-((x-m)/s)**2/2-z if a<=x<=b else -mp.inf
    else:
        ld=declarative(folder,meta,theta,route)
    value=mp.fsum(ld(number(x)) for x in data)
    if mp.isnan(value) or isinstance(value,mp.mpc):
        raise ArithmeticError('independent density is unresolved or undefined at these inputs')
    return float(value)
