"""Exact-rational support regression beyond the saved audit corpus."""
import csv,json,pathlib
from fractions import Fraction
HERE=pathlib.Path(__file__).resolve().parent
out={}
for phase in ('before','after'):
 r=next(csv.reader((HERE/f'subnormal-{phase}.csv').open()))
 tiny,mu,sigma=map(float,(r[0],r[2],r[3]))
 out[phase]={'free_status':int(r[1]),'mu':mu,'sigma':sigma,'logLik':float(r[4]),
             'all_held_status':int(r[5]),'divided_residual':r[6],
             'residual_signbit':int(r[7]),
             'exact_support_contains_tiny':Fraction(tiny)<=Fraction(mu)+Fraction(sigma)}
assert not out['before']['exact_support_contains_tiny']
assert out['after']['exact_support_contains_tiny'] and out['after']['all_held_status']==1
(HERE/'subnormal-validation.json').write_text(json.dumps(out,indent=2)+'\n')
print('Exact-rational subnormal support: PASS')
