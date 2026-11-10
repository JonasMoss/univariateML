#!/usr/bin/env python3
"""Independent closed Bessel-polynomial score for the retained tiny improvement."""
import mpmath as m,json
from pathlib import Path
m.mp.dps=90
n=m.mpf(20003)
def score(t):
 s=m.exp(t);z=m.sqrt(s*(s+2));q=m.sqrt(s/(s+2))
 degree=(10000*z/(z+1)+(3+6/z)/(1+3/z+3/z**2))/n
 return 2*q/(1+q)**2-1+(q+1/q)*degree/2
r=m.findroot(score,(m.log(10000),m.log(50000)))
result=dict(shape=str(m.exp(r)),log_shape=str(r),score=str(score(r)),precision=m.mp.dps,counts={'0':10002,'2':10000,'3':1})
Path(__file__).with_suffix('.json').write_text(json.dumps(result,indent=2)+'\n');print(result)
