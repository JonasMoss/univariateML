"""Independent small-alpha, nonunit-scale exact-input fatigue checks."""
import json,sys,math
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
from quality.truth_reference import normal
label=sys.argv[1]
if label=='before':audit.EXE=audit.WORK/'probe-baseline'
mp.mp.dps=160
jobs=[];expected=[]
for a in (1e-8,1e-12):
 for mu in (0.,-1000.):
  for x in (1000+mu,math.nextafter(1000+mu,-math.inf),math.nextafter(1000+mu,math.inf),1000*math.expm1(.5*a) if mu else 1000*math.exp(.5*a)):
   aa,s,m,v=map(mp.mpf,(a,1000.,mu,x));y=v-m
   left=mp.sqrt(y/s);right=mp.sqrt(s/y);w=(left-right)/aa
   ld=mp.log(left+right)-mp.log(2*aa*y)-mp.log(2*mp.pi)/2-w*w/2
   jobs.append(('fatigue',0,x,True,True,[a,1000.,mu]));expected.append(float(ld))
   for lower,val in zip((True,False),normal(w)):
    jobs.append(('fatigue',1,x,lower,True,[a,1000.,mu]));expected.append(float(val))
rows=[dict(job=j,actual=a,expected=b,error=audit.error(a,b)) for j,a,b in zip(jobs,audit.evaluate(jobs),expected)]
(HERE/('fatigue-center-'+label+'.json')).write_text(json.dumps(rows,indent=2)+'\n')
print('fatigue center',label,len(rows),'worst',max(r['error'] for r in rows),flush=True)
if label!='before':assert all(r['error']<=1e-10 for r in rows)
