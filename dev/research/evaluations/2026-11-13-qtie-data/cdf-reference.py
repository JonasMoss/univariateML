"""400-digit references for the two CDF defects exposed by this job."""
import csv,json,math
from fractions import Fraction
from pathlib import Path
import mpmath as m
m.mp.dps=400
root=Path(__file__).parent
p=m.exp(-20)*sum((m.mpf(20)**j/m.factorial(j) for j in range(16)),m.mpf(0))
f=Fraction(math.comb(50,20),math.comb(100,20))
refs={'pois':m.log1p(-p),'hyper':m.log1p(-m.mpf(f.numerator)/f.denominator)}
result={}
for phase in ['before','after']:
 for family,value,quantile,policy_quantile in csv.reader((root/f'cdf-probe-{phase}.csv').open()):
  ref=refs[family];got=m.mpf(float(value))
  result.setdefault(family,dict(reference=m.nstr(ref,100),expected_quantile=16 if family=='pois' else 0))[phase]=dict(log_tail=value,relative_error_eps=float(abs(got-ref)/(abs(ref)*m.mpf(2)**-52)),public_quantile=float(quantile),new_policy_on_this_cdf=float(policy_quantile))
(root/'cdf-reference.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
