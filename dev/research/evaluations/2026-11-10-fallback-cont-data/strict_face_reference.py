"""Numerical corroboration; the positive face slope is an algebraic proof."""
import mpmath as mp,json
from pathlib import Path
mp.mp.dps=80
x=[mp.mpf(v) for v in [1.,1.,1.,1.,6.0001]];mean=sum(x)/5;y=[v/mean for v in x]
def A(q):return sum(mp.log1p(q*v) for v in y)/5
def score(q):
 a=A(q);b=sum(q*v/(1+q*v) for v in y)/5
 return 1-b/a-b
q=mp.findroot(score,(mp.mpf('0.00001'),mp.mpf('0.0001')))
a=A(q);h=mp.log(q/a)-a
out=dict(precision=mp.mp.dps,sample=[str(v) for v in x],q=str(q),lambda_=str(q/mean),kappa=str(1/a),loglik_gain=str(5*h),face_slope=str(sum(v*v for v in y)/10-1),evidence='80-digit stationary-root comparison, not a global certificate')
assert h>0
Path(__file__).with_name('strict-face-reference.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
