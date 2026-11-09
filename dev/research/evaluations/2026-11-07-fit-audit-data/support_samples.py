#!/usr/bin/env python3
"""Supplement parameter-named min/max supports and valid constant-data locations."""
import hashlib,json,math,pathlib
import audit
import sympy as sp
from quality.harness.check import r_bound_to_sympy
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text());keys={(c['family'],c['mask'],tuple(c['data'])) for c in cases};added=[]
functions=dict(inf=math.inf,Inf=math.inf,min=min,max=max,floor=math.floor,ceil=math.ceil)
for folder,m,g in audit.models():
 names=[p['name'] for p in m['params']]
 vals={p['name']:audit.number(g['estimates'].get(p['name'],p.get('simulate',1))) for p in m['params']};env={**functions,**vals}
 def bound(key,environment,fallback):
  try:return float(eval(str(m['support'][key]),{'__builtins__':{}},environment))
  except Exception:
   try:
    source=r_bound_to_sympy(str(m['support'][key]));symbols={name:sp.Symbol('p_'+name,real=True) for name in names}
    import re
    for name in sorted(names,key=len,reverse=True):source=re.sub(r'\b'+re.escape(name)+r'\b',symbols[name].name,source)
    expr=sp.sympify(source,locals={v.name:v for v in symbols.values()})
    expr=expr.subs({symbol:environment[name] for name,symbol in symbols.items() if name in environment and isinstance(environment[name],(int,float))})
    return float(expr) if not expr.free_symbols else fallback
   except Exception:return fallback
 lo,hi=bound('lower',env,-math.inf),bound('upper',env,math.inf)
 old=[c for c in cases if c['family']==m['id']];subsets={c['mask']:c['held'] for c in old}
 discrete=m['support']['type']=='integer'
 for mask,held in sorted(subsets.items()):
  actual={**functions,**held};lower=bound('lower',actual,-math.inf);upper=bound('upper',actual,math.inf)
  def valid(x):return all(math.isfinite(v) and lower<=v<=upper for v in x)
  samples=[]
  if discrete:
   if math.isfinite(lo):samples.append(('support-lower-count',[lo,lo,lo,lo,lo+1]))
   if math.isfinite(hi):samples.append(('support-upper-count',[hi,hi,hi,hi-1,hi]))
  else:
   for scale in [1e-6,1e-12,1e-17,1e-200]:
    if math.isfinite(lo):samples.append((f'support-lower-{scale:g}',[lo+scale*t for t in [1,2,3,5,9]]))
    if math.isfinite(hi):samples.append((f'support-upper-{scale:g}',[hi-scale*t for t in [1,2,3,5,9]]))
   # Exact binary64 complements and conditional-support interior ULP probes.
   for endpoint,direction,label in [(lo,math.inf,'lower'),(hi,-math.inf,'upper')]:
    if not math.isfinite(endpoint) or endpoint==0:continue
    step=math.nextafter(endpoint,direction)-endpoint
    if abs(step)<1e-200:continue
    multipliers=[16,32,48,80,144] if str(m['support']['lower' if label=='lower' else 'upper']).strip().startswith('if') else [1,2,3,5,9]
    samples.append((f'support-{label}-ulp',[endpoint+k*step for k in multipliers]))
   samples=[(label,x) for label,x in samples if all(lo<v<hi for v in x)]
   locations={c['data'][2] for c in old if c['mask']==mask and c['sample'].startswith('constant-') and len(c['data'])==5 and valid(c['data'])}
   candidates=[lo+(hi-lo)*.25,lo+(hi-lo)*.75] if math.isfinite(lo) and math.isfinite(hi) else [lo+1,lo+2] if math.isfinite(lo) else [1,2]
   for loc in candidates:
    if len(locations)>=2:break
    if loc in locations:continue
    for spread in [1e-4,1e-8,1e-12]:samples.append((f'constant-support-{loc:g}-{spread:g}',[loc+max(abs(loc),1e-150)*spread*t for t in [-2,-1,0,1,3]]))
    locations.add(loc)
  for label,x in samples:
   if discrete:x=list(map(int,x))
   key=(m['id'],mask,tuple(x))
   if key in keys or not valid(x):continue
   c=dict(index=len(cases),family=m['id'],mask=mask,held=held,sample=label,data=x);cases.append(c);added.append(c['index']);keys.add(key)
(HERE/'cases.json').write_text(json.dumps(cases));all_support=[c['index'] for c in cases if c['sample'].startswith(('support-','constant-support-'))]
(HERE/'support-supplement.json').write_text(json.dumps(dict(added_this_run=added,indices=all_support,count=len(all_support),families=sorted({cases[i]['family'] for i in all_support})),indent=2))
(HERE/'inventory-finalized.json').write_text(json.dumps(dict(cases=len(cases),sha256=hashlib.sha256((HERE/'cases.json').read_bytes()).hexdigest(),replay='cases.json is the frozen binary64 input inventory; generation preserves it unless AUDIT_RESET_CASES=1'),indent=2))
print('support supplement',len(added),'total cases',len(cases),flush=True)
