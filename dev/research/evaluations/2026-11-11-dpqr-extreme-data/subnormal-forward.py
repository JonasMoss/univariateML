"""Expose the natural-output range omitted by the existing truth consumer."""
import json,sys,math
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=160
jobs=[('weibull',1,x,False,False,[1.,1.]) for x in (710.,720.,737.123,744.3,800.)]
rows=[]
for j,a in zip(jobs,audit.evaluate(jobs)):
 ref=mp.exp(-mp.mpf(j[2]));nearest=float(ref)
 rows.append(dict(family='weibull',function='p',input=j[2],parameters=[1.,1.],lower=False,log=False,actual=a,reference=mp.nstr(ref,100),relative_error=float(abs(mp.mpf(a)-ref)/ref),nearest_binary64=nearest,best_binary64_relative_error=float(abs(mp.mpf(nearest)-ref)/ref),consumer_checks_natural=math.exp(float(-mp.mpf(j[2])))>1e-300))
(H/'subnormal-forward.json').write_text(json.dumps(rows,indent=2)+'\n');print(rows)
