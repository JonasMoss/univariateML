"""Strict returned-point support exclusions, proved with exact binary64 rationals.
Equality is deliberately not tested: endpoint density conventions differ.
"""
import functools,math,re
import sympy as sp
from quality.harness.check import r_bound_to_sympy

def rational(v):
 v=float(v)
 return sp.oo if v==math.inf else -sp.oo if v==-math.inf else sp.Rational(*v.as_integer_ratio())

@functools.lru_cache(None)
def expressions(names,lower,upper):
 symbols={n:sp.Symbol('p_'+n,real=True) for n in names};out=[]
 for raw in [lower,upper]:
  source=re.sub(r'\binf\b','oo',r_bound_to_sympy(raw))
  for n in sorted(names,key=len,reverse=True):source=re.sub(r'\b'+re.escape(n)+r'\b',symbols[n].name,source)
  out.append(sp.sympify(source,locals={**{v.name:v for v in symbols.values()},'min':sp.Min,'max':sp.Max,'oo':sp.oo}))
 return symbols,out

def exclusion(meta,case,theta):
 try:
  names=tuple(p['name'] for p in meta['params'])
  symbols,exprs=expressions(names,str(meta['support']['lower']),str(meta['support']['upper']))
  subs={symbols[n]:rational(v) for n,v in zip(names,theta)}
  for j,e in enumerate(exprs):
   bound=e.subs(subs)
   if bound.free_symbols:continue
   for i,x in enumerate(case['data']):
    value=rational(x);bad=value<bound if j==0 else value>bound
    if bad==sp.true:
     return dict(bound='lower' if j==0 else 'upper',observation_index=i,exact_bound=str(bound),exact_observation=str(value),exact_gap=str(abs(value-bound)),bound_expression=str(e),basis='exact binary64 rational support comparison')
 except (ValueError,TypeError,OverflowError,NotImplementedError):pass
 return None

def classify(record,meta,case,theta,reported):
 proof=exclusion(meta,case,theta)
 if not proof:return False
 for k in ['refined_theta','refined_logLik','scores','parameter_relative_error','coefficient_error_verified','local_information_eigenvalue_range','stationary_residual_per_observation','relative_gain']:
  record.pop(k,None)
 record.update(theta=[repr(float(v)) for v in theta],reported_logLik=repr(float(reported)),classification='silent failure',evidence='returned coefficients exclude an observation from support',reference_logLik='-inf',relative_error=float('inf'),reference_basis=proof['basis'],support_exclusion=proof)
 return True
