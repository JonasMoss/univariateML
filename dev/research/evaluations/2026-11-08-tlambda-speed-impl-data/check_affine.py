from pathlib import Path
import json,mpmath as mp
mp.mp.dps=420
p=Path(__file__).resolve().parent
tiny=mp.mpf(2)**-1074;z=2-2*tiny
ref=mp.log(4-z*z)-mp.log(4)-mp.log(8-z*z)/2
line=[s for s in (p/'affine.log').read_text().splitlines() if s.startswith('gap=')][-1]
a=dict(v.split('=') for v in line.split())
error=abs(mp.mpf(float(a['fast']))-ref)
assert mp.mpf(float(a['gap']))==tiny
assert error<mp.mpf('2e-12')
out=dict(precision=mp.mp.dps,mu='2^-1073',x=2,sigma=1,shape=.5,affine_deficit='2^-1074',reference_logdensity=mp.nstr(ref,90),absolute_error=float(error),public_density_difference=abs(float(a['dtlambda'])-float(a['fast'])))
(p/'affine-checks.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
# Public dtlambda intentionally aliases rounded endpoints for lambda>=1.
line=[s for s in (p/'affine.log').read_text().splitlines() if s.startswith('alias ')][-1]
a=dict(v.split('=') for v in line.split()[1:]);l=mp.mpf(float(a['lambda']))
lo=mp.mpf(-1500);hi=-mp.log(2)
for _ in range(300):
    s=(lo+hi)/2;prob=mp.exp(s)
    deficit=prob**l-mp.expm1(l*mp.log1p(-prob))
    if deficit>tiny:hi=s
    else:lo=s
prob=mp.exp((lo+hi)/2)
reference=-mp.log(l)-mp.log(prob**(l-1)+(1-prob)**(l-1))
error=abs(mp.mpf(float(a['fast']))-reference)
assert error<mp.mpf('2e-12') and abs(mp.mpf(float(a['public']))-reference)>.5
out=dict(precision=mp.mp.dps,x=1,mu='2^-1074',sigma=float(l),shape=float(l),reference_interior_logdensity=mp.nstr(reference,90),fitting_inverse_error=float(error),public_endpoint_alias_logdensity=float(a['public']),expected_public_alias_difference=float(abs(mp.mpf(float(a['public']))-reference)),public_convention_unchanged=True)
(p/'endpoint-alias-checks.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
