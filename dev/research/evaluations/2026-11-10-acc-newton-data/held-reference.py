"""Independent conditional-score roots on exact binary64 samples."""
import mpmath as mp,json
from pathlib import Path
mp.mp.dps=100
x=list(map(mp.mpf,[2.4349403590372,2.434940359039635,2.43494035904207,2.434940359044505,2.434940359049375])); scale=x[0]
z=[mp.log(v/scale) for v in x]
wscore=lambda eta:len(x)/mp.exp(eta)+sum(v*(1-mp.exp(mp.exp(eta)*v)) for v in z)
k=mp.exp(mp.findroot(wscore,(mp.log(mp.mpf('2e11')),mp.log(mp.mpf('4e11')))))
kappa=mp.mpf(float(1e-14));data=list(map(mp.mpf,[1,2,3]))
ls=lambda eta:sum(1/(1+mp.exp(eta)*v)-kappa*mp.exp(eta)*v/(1+mp.exp(eta)*v) for v in data)
rate=mp.exp(mp.findroot(ls,(mp.log(mp.mpf('5e13')),mp.log(mp.mpf('7e13')))))
refs={'weibull-held-scale':{'shape':str(k)},'lomax-held-kappa':{'lambda':str(rate)},'gumbel-held-scale':{'mu_in_scale_units':str(-mp.log((1+mp.exp(-1))/2))}}
# A separate quadratic oracle for a two-valued Lomax sample.
def quadratic(a,b,nl,nh,kap):
 a,b,kap=map(mp.mpf,[a,b,kap]);n=nl+nh;A=kap*n*a*b
 B=(kap*nl-nh)*a+(kap*nh-nl)*b
 d=mp.sqrt(B*B+4*A*n)
 return 2*n/(B+d) if B>=0 else (-B+d)/(2*A)
refs['lomax-wide-held-kappa']={'lambda':str(quadratic(1e-200,1e200,1,1,1.))}
refs['lomax-weighted-held-kappa']={'lambda':str(quadratic(1e-200,1e200,1,3,1/3))}
refs['lomax-subnormal-held-kappa']={'lambda':str(quadratic(1e158,2e158,1,1,1e158))}
refs['weibull-held-shape']={'scale_in_ulps':'1.5'}
h=Path(__file__).resolve().parent
for row in map(json.loads,(h/'held-probes-before.jsonl').read_text().splitlines()):
 name=row['id'];key='scale_in_ulps' if name=='weibull-held-shape' else 'shape' if name.startswith('weibull') else 'lambda' if name.startswith('lomax') else 'mu_in_scale_units'
 value=row.get(key,row.get('mu_in_ulps'))
 refs[name]['before_error']=str(abs(mp.mpf(value)/mp.mpf(refs[name][key])-1)) if value else '1'
from fractions import Fraction
rho=Fraction.from_float(1e158);lead=2/(3*rho*rho)
nearest=Fraction.from_float(float(lead));grid_error=abs(nearest/lead-1);bound=2/rho
assert (grid_error-bound)/(1+bound)>Fraction(1,10**10)
refs['lomax-subnormal-held-kappa']['nearest_leading_rate_relative_error']=str(mp.mpf(grid_error.numerator)/grid_error.denominator)
refs['lomax-subnormal-held-kappa']['root_relative_to_leading_rate_bound']='within 2/kappa of 2/(3*kappa*data[0])'
refs['lomax-subnormal-held-kappa']['exact_lattice_inequality_verified']=True
print(json.dumps(refs,indent=2))
