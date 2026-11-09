"""Sufficient incompatibility checks from fixed bounds, including Min/Max bounds."""
import math,re
import sympy as sp
from quality.harness.check import r_bound_to_sympy

def violation(meta,case):
 names=[p['name'] for p in meta['params']];symbols={n:sp.Symbol('p_'+n,real=True) for n in names};held=case['held']
 for j,key in enumerate(['lower','upper']):
  try:
   source=r_bound_to_sympy(str(meta['support'][key]));source=re.sub(r'\binf\b','oo',source)
   for n in sorted(names,key=len,reverse=True):source=re.sub(r'\b'+re.escape(n)+r'\b',symbols[n].name,source)
   e=sp.sympify(source,locals={**{v.name:v for v in symbols.values()},'min':sp.Min,'max':sp.Max,'oo':sp.oo})
   e=e.subs({symbols[n]:v for n,v in held.items()})
   if not e.free_symbols:bound=float(e)
   elif j==0 and e.func==sp.Max:
    numeric=[float(a) for a in e.args if not a.free_symbols];bound=max(numeric) if numeric else -math.inf
   elif j==1 and e.func==sp.Min:
    numeric=[float(a) for a in e.args if not a.free_symbols];bound=min(numeric) if numeric else math.inf
   else:continue
   closed=meta['support']['closed'][j]
   bad=any(v<bound for v in case['data']) if j==0 else any(v>bound for v in case['data'])
   if bad:return f'observations violate the held {key} support bound {bound!r}; every admissible parameter point has zero likelihood'
  except Exception:continue
 return ''
