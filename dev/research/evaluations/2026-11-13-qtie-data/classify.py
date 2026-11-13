"""Independent 400-digit quantiles and per-disagreement causes."""
from pathlib import Path
import csv,json,bisect
import mpmath as m
m.mp.dps=400
root=Path(__file__).parent
sums={}
uppers={}
for id in ['pois','binom','geom','nbinom','hyper']:
 masses=[]
 for k in range(300):
  if id=='pois': v=m.exp(-20)*m.mpf(20)**k/m.factorial(k)
  elif id=='binom': v=m.binomial(100,k)/m.mpf(2)**100 if k<=100 else m.mpf(0)
  elif id=='geom': v=m.mpf(1)/4*(m.mpf(3)/4)**k
  elif id=='nbinom': v=m.binomial(k+9,k)/m.mpf(2)**(10+k)
  else: v=m.binomial(50,k)*m.binomial(50,20-k)/m.binomial(100,20) if k<=20 else m.mpf(0)
  masses.append(v)
 total=m.mpf(0);s=[]
 for v in masses: total+=v;s.append(total)
 sums[id]=s
 # Include the infinite remainder rather than truncating survival at 299.
 remainder=(m.mpf(3)/4)**300 if id=='geom' else m.betainc(300,10,0,m.mpf(1)/2,regularized=True) if id=='nbinom' else m.gammainc(300,0,20)/m.factorial(299) if id=='pois' else m.mpf(0)
 uppers[id]=[sum(masses[k+1:],remainder) for k in range(len(masses))]
counts={}
with (root/'disagreements.csv').open() as f, (root/'classified.csv').open('w') as out:
 reader=csv.DictReader(f);w=csv.DictWriter(out,fieldnames=reader.fieldnames+['exact','exact_adjusted','adjusted_p','boundary_error_eps','cause']);w.writeheader()
 for r in reader:
  id=r['family'];p=m.mpf(float(r['p']));lower=int(r['lower']);log=int(r['log'])
  if log:p=m.exp(p)
  exact=bisect.bisect_left(sums[id],p) if lower else bisect.bisect_left([-v for v in uppers[id]],-p)
  original=float(r['p']);e=64*2.220446049250313e-16
  adjusted=(original*(1+e) if lower else original*(1-e)) if log else (original*(1-e) if lower else original*(1+e) if 1-original>4*e else original)
  ap=m.exp(m.mpf(adjusted)) if log else m.mpf(adjusted)
  exact_adjusted=bisect.bisect_left(sums[id],ap) if lower else bisect.bisect_left([-v for v in uppers[id]],-ap)
  cpp=int(float(r['cpp']));stats=int(float(r['stats']))
  if cpp==exact: cause='C++ exact; stats fuzz/closed-form rounding'
  elif stats==exact: cause='stats exact; C++ specified fuzz band'
  else: cause='both differ from exact within their fuzz bands'
  error=''
  if cpp!=exact_adjusted:
   cause+='; computed CDF rounding at adjusted boundary'
   point=cpp if cpp<exact_adjusted else cpp-1
   value=sums[id][point] if lower else uppers[id][point]
   value=m.log(value) if log else value
   error=float(abs(value-m.mpf(adjusted))/(m.mpf(2)**-52*abs(m.mpf(adjusted))))
  r.update(exact=exact,exact_adjusted=exact_adjusted,adjusted_p=repr(adjusted),boundary_error_eps=error,cause=cause);w.writerow(r)
  counts.setdefault(id,{});counts[id][cause]=counts[id].get(cause,0)+1
(root/'comparison-summary.json').write_text(json.dumps(counts,indent=2)+'\n')
print(json.dumps(counts,indent=2))
