"""Check archived outcomes against proved cases and 70-digit density values.
Numerical failures remain recorded; passing this checker does not prove global
optimality of uncertified fits or recovery of every representable maximum.
"""
import csv,collections,json,math,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
def logq(t):
 return mp.log(-mp.expm1(-t)) if t<mp.log(2) else mp.log1p(-mp.exp(-t))
root=Path(__file__).parent
source=sys.argv[1] if len(sys.argv)>1 else 'archive.csv'
output=sys.argv[2] if len(sys.argv)>2 else 'archive-check.json'
sample_file=sys.argv[3] if len(sys.argv)>3 else 'archive-samples.csv'
samples={}
for row in csv.DictReader((root/sample_file).open()):samples.setdefault(row['sample'],[]).append(float(row['x']))
rows=list(csv.DictReader((root/source).open()));counts=collections.Counter();errors=[];worst=0;failures=[]
for row in rows:
 mask=int(row['mask']);x=samples[row['sample']];constant=min(x)==max(x);free=[not mask&(1<<i) for i in range(3)]
 counts[row['status']]+=1
 if not all(v>0 and math.isfinite(v) for v in x):
  if row['status']!='Invalid':errors.append(dict(sample=row['sample'],mask=mask,error='invalid input accepted'))
  continue
 unbounded=constant and ((free[0] and (free[1] or free[2])) or (not free[0] and free[1] and (x[0]==2 or (free[2] and x[0]>2))))
 power=not free[0] and free[1] and free[2] and max(x)<=2 and min(x)<2
 expected='Unbounded' if unbounded else ('PowerBoundaryTied' if max(x)==2 else 'PowerBoundary') if power else None
 if expected and row['status']!=expected:errors.append(dict(sample=row['sample'],mask=mask,expected=expected,actual=row['status']))
 if row['status']=='Unbounded' and not unbounded:errors.append(dict(sample=row['sample'],mask=mask,error='unproved concentration stop'))
 if row['status'] in ['IterationFailure','AccuracyFailure','Invalid']:failures.append(dict(sample=row['sample'],mask=mask,status=row['status']))
 if row['status']=='Ok':
  if bool(int(row['certified'])) and not bool((mask&4) or (mask&3)==3):errors.append(dict(sample=row['sample'],mask=mask,error='incorrect certificate regime'))
  for i,(name,value) in enumerate(zip(['scale','shape','exponent'],[2.,1.5,2.5])):
   if mask&(1<<i) and float(row[name])!=value:errors.append(dict(sample=row['sample'],mask=mask,error='held coefficient changed',coefficient=name))
  a,b,c=[mp.mpf(float(row[k])) for k in ['scale','shape','exponent']]
  value=mp.mpf(0)
  for v in x:
   y=mp.log(mp.mpf(v));z=b*(y-mp.log(a));t=mp.exp(z)
   value+=mp.log(b)+mp.log(c)-y+z-t+(c-1)*logq(t)
  error=float(abs(value-mp.mpf(float(row['logLik'])))/max(1,abs(value)))
  worst=max(worst,error)
  if error>1e-10:errors.append(dict(sample=row['sample'],mask=mask,error='density mismatch',relative_error=error))
 if power:
  y=[mp.log(mp.mpf(v)) for v in x];gap=sum(mp.log(2)-v for v in y);beta=len(x)/gap
  value=len(x)*(mp.log(beta)-1)-sum(y)-sum(v==2 for v in x)*mp.log(mp.e-1)
  error=float(abs(value-mp.mpf(float(row['logLik'])))/max(1,abs(value)))
  worst=max(worst,error)
  if error>1e-10:errors.append(dict(sample=row['sample'],mask=mask,error='power envelope mismatch',relative_error=error))
summary=dict(baseline='af824a98d78f2f2325c7a675c20c84c45e6bb826',samples=len(samples),fits=len(rows),statuses=dict(counts),worst_relative_or_absolute_logLik_error=worst,violations=errors,numerical_or_accuracy_failures=failures)
(root/output).write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps({k:v for k,v in summary.items() if k!='numerical_or_accuracy_failures'},indent=2))
if errors:raise SystemExit(1)
