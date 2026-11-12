"""Independent 100-digit local roots for rejected accuracy candidates."""
import csv,json,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
root=Path(__file__).parent
samples={}
for row in csv.DictReader((root/'archive-samples.csv').open()):samples.setdefault(row['sample'],[]).append(mp.mpf(float(row['x'])))
source=sys.argv[1] if len(sys.argv)>1 else 'accuracy-rejections.csv'
output=sys.argv[2] if len(sys.argv)>2 else 'failed-accuracy.json'
records=[]
for row in csv.DictReader((root/source).open()):
 if row['status']!='AccuracyFailure' or int(row['mask'])!=0:continue
 y=list(map(mp.log,samples[row['sample']]))
 p=list(map(lambda key:mp.log(mp.mpf(float(row[key]))),['scale','shape','exponent']))
 def score(*q):
  b,c=mp.exp(q[1]),mp.exp(q[2]);g=[mp.mpf(0)]*3
  for v in y:
   z=b*(v-q[0]);t=mp.exp(z);h=mp.log1p(-mp.exp(-t)) if t>mp.log(2) else mp.log(-mp.expm1(-t))
   Q=1-t+(c-1)*t/mp.expm1(t)
   g[0]-=b*Q;g[1]+=1+z*Q;g[2]+=1+c*h
  return tuple(g)
 record=dict(sample=row['sample'],initial=list(map(float,p)))
 try:
  q=mp.findroot(score,tuple(p),tol=mp.mpf('1e-75'),maxsteps=100)
  record.update(root_logs=[str(v) for v in q],relative_coefficient_error=[float(abs(mp.expm1(q[i]-p[i]))) for i in range(3)],score_norm=float(max(map(abs,score(*q)))))
 except (ValueError,ZeroDivisionError,OverflowError) as e:record['open']=str(e)[:500]
 records.append(record)
(root/output).write_text(json.dumps(dict(precision=100,records=records),indent=2)+'\n')
print(json.dumps(records,indent=2))
