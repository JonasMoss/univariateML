import csv,json,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
root=Path(__file__).parent
worst=[0.,0.,0.];records=[]
source=sys.argv[1] if len(sys.argv)>1 else 'derivatives.csv'
output=sys.argv[2] if len(sys.argv)>2 else 'derivative-check.json'
for row in csv.reader((root/source).open()):
 profile=bool(int(row[0]));p=list(map(lambda z:mp.mpf(float(z)),row[1:4]))
 y=list(map(lambda z:mp.mpf(float(z)),[-.7,.1,.4,.9]))
 def f(*p):
  b=mp.exp(p[1]);z=[b*(v-p[0]) for v in y];t=list(map(mp.exp,z));h=[mp.log(-mp.expm1(-v)) for v in t]
  c=-len(y)/sum(h) if profile else mp.exp(p[2])
  return p[1]+mp.log(c)+sum(z[i]-t[i]+(c-1)*h[i] for i in range(len(y)))/len(y)
 exact=[f(*p)]+[mp.diff(f,p,tuple(int(k==i) for k in range(3))) for i in range(3)]+[mp.diff(f,p,tuple(int(k==i)+int(k==j) for k in range(3))) for i in range(3) for j in range(3)]
 errors=[float(abs(mp.mpf(float(v))-z)/max(1,abs(z))) for v,z in zip(row[4:],exact)]
 for i,e in enumerate([errors[0],max(errors[1:4]),max(errors[4:])]):worst[i]=max(worst[i],e)
 records.append(dict(profile=profile,p=list(map(float,p)),errors=errors))
(root/output).write_text(json.dumps(dict(precision=70,worst_relative_or_absolute=dict(zip(['value','gradient','hessian'],worst)),records=records),indent=2)+'\n')
print(worst)

if max(worst)>1e-10:raise SystemExit(1)
