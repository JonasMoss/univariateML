"""70-digit local roots for the recorded held census, using exact binary64 inputs.
Run from the repository root under /tmp/umvl-cpu/run; no C++ evaluator is reused.
"""
import csv,json,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
def logq(t):
 return mp.log(-mp.expm1(-t)) if t<mp.log(2) else mp.log1p(-mp.exp(-t))
root=Path(__file__).parent
source=sys.argv[1] if len(sys.argv)>1 else 'held-study.csv'
output=sys.argv[2] if len(sys.argv)>2 else 'reference-check.json'
sample_file=sys.argv[3] if len(sys.argv)>3 else 'held-samples.csv'
samples={}
for row in csv.DictReader((root/sample_file).open()):
 samples.setdefault(row['sample'],[]).append(mp.mpf(float(row['x'])))
records=[]
for row in csv.DictReader((root/source).open()):
 if row['status']!='Ok':continue
 x=samples[row['sample']]; y=list(map(mp.log,x));mask=int(row['mask'])
 initial=list(map(lambda k:mp.log(mp.mpf(float(row[k]))),['scale','shape','exponent']))
 indices=[i for i in range(3) if not mask&(1<<i)]
 def unpack(v):
  p=initial.copy()
  for i,z in zip(indices,v):p[i]=z
  return p
 def ll(p):
  a,b,c=mp.exp(p[0]),mp.exp(p[1]),mp.exp(p[2])
  return sum(mp.log(b)+mp.log(c)-z+b*(z-p[0])-mp.exp(b*(z-p[0]))+(c-1)*logq(mp.exp(b*(z-p[0]))) for z in y)
 def score(*v):
  p=unpack(v);b,c=mp.exp(p[1]),mp.exp(p[2]);g=[mp.mpf(0)]*3
  for z in y:
   w=b*(z-p[0]);t=mp.exp(w);q=1-t+(c-1)*t/mp.expm1(t)
   g[0]-=b*q;g[1]+=1+w*q;g[2]+=1+c*logq(t)
  return tuple(g[i] for i in indices)
 result=dict(sample=row['sample'],mask=mask)
 try:
  # Avoid attempting enormous exponentials for unrelated extreme all-held hazards.
  if indices:
   answer=mp.findroot(score,tuple(initial[i] for i in indices),tol=mp.mpf('1e-55'),maxsteps=50)
   if len(indices)==1 and not hasattr(answer,'rows'):answer=[answer]
   p=unpack(answer)
   errors=[abs(mp.expm1(p[i]-initial[i])) for i in indices]
   residual=max(map(abs,score(*(p[i] for i in indices))))
  else:p=initial;errors=[mp.mpf(0)];residual=mp.mpf(0)
  exact=ll(p)
  result.update(relative_coefficient_error=float(max(errors)),score_norm=float(residual),logLik_error=float(mp.mpf(float(row['logLik']))-exact),logLik_relative_error=float(abs(mp.mpf(float(row['logLik']))-exact)/max(1,abs(exact))))
 except (ValueError,ZeroDivisionError,OverflowError) as e:result['open']=str(e)[:240]
 records.append(result)
out=dict(precision=mp.mp.dps,claim='Local independent roots, not global certification',records=records)
(root/output).write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(dict(checked=len(records),open=sum('open' in r for r in records),max_coefficient_error=max(r.get('relative_coefficient_error',0) for r in records),max_logLik_relative_error=max(r.get('logLik_relative_error',0) for r in records)),indent=2))

if any(r.get("relative_coefficient_error",0)>1e-10 or r.get("logLik_relative_error",0)>1e-10 for r in records):raise SystemExit(1)
