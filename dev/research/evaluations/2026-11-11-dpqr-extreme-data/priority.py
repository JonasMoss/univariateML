"""Exact-input 100-digit witnesses, including the R stats routing boundary."""
import json,sys,subprocess,math
from pathlib import Path
import mpmath as mp
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
if sys.argv[1]=='before':audit.EXE=audit.WORK/'probe-baseline'
mp.mp.dps=100
jobs=[];refs=[]
for ident in ('weibull','llogis'):
 for shape in (1e8,1e12,1e16,1e100,1e308):
  scale=1000.
  xs=[scale,math.nextafter(scale,0),math.nextafter(scale,math.inf),scale*(1+.5/shape)]
  for x in sorted(set(xs)):
   k,s,v=map(mp.mpf,(shape,scale,x));z=k*mp.log(v/s)
   ld=mp.log(k)-mp.log(v)+z-mp.exp(z) if ident=='weibull' else mp.log(k)-mp.log(v)-mp.log1p(mp.exp(z))-mp.log1p(mp.exp(-z))
   upper=-mp.exp(z) if ident=='weibull' else -mp.log1p(mp.exp(z))
   for mode,b in [(0,ld),(1,upper)]:
    jobs.append((ident,mode,x,False,True,[shape,scale]));refs.append(float(b))
vals=audit.evaluate(jobs)
rows=[dict(family=i,function='d' if m==0 else 'p',parameters=p,input=x,actual=a,expected=b,error=audit.error(a,b)) for (i,m,x,lo,lg,p),a,b in zip(jobs,vals,refs)]
(HERE/('priority-'+sys.argv[1]+'.json')).write_text(json.dumps(rows,indent=2)+'\n')
print('priority',sys.argv[1],len(rows),'worst',max(r['error'] for r in rows))
