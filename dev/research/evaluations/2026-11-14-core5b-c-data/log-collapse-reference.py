"""Independent high-precision witness for the nonconstant-log-collapse regression."""
import json,math
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
x=[1e300,math.nextafter(1e300,math.inf)]
assert x[0]!=x[1] and math.log(x[0])==math.log(x[1])
b=mp.mpf(x[0]);z=[(mp.mpf(v)/b)**2 for v in x];u=[v/(1+v) for v in z]
gap=1-mp.sqrt(u[0]*u[1])-mp.sqrt((1-u[0])*(1-u[1]))
assert u[0]!=u[1] and gap>mp.mpf('1e-40')
record=dict(binary64_data=[v.hex() for v in x],held=dict(shape2=2,scale=x[0].hex()),
 precision_digits=mp.mp.dps,beta_observations=[str(v) for v in u],strict_cauchy_gap=str(gap),
 proof='The distinct beta observations give a strictly interior sufficient-statistic mean; concentration does not make their joint likelihood unbounded.',
 expected_outcome=dict(kind='failure',reason='evaluation'))
Path(__file__).with_suffix('.json').write_text(json.dumps(record,indent=2)+'\n')
print('Distinct beta observations verified at 100 digits; strict Cauchy gap:',mp.nstr(gap,12))
