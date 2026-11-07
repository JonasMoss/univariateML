"""Independent high-precision checks for the held t certificates; no random seed."""
import json, platform
import mpmath as mp
mp.mp.dps=80
x=list(map(mp.mpf,[-3,-2,0,1,5])); df=mp.mpf(3); scale=mp.mpf(1)
score=lambda m:sum((m-v)/(df*scale**2+(m-v)**2) for v in x)
r=mp.findroot(score,(-mp.mpf('.4'),-mp.mpf('.2')))
checks={'python':platform.python_version(),'mpmath':mp.__version__,'digits':mp.mp.dps,'asymmetric_location':str(r)}
# S(m) for two points +/-b factors as
# 2m(m^2 + df*s^2-b^2)/[(df*s^2+(m-b)^2)(df*s^2+(m+b)^2)].
errors=[]
for b in (mp.mpf(1),mp.mpf(3)):
 for d in (mp.mpf('.25'),mp.mpf(1),mp.mpf(3)):
  for m in (mp.mpf('-.7'),mp.mpf('.2'),mp.mpf('2.5')):
   direct=sum((m-v)/(d+(m-v)**2) for v in [-b,b])
   factor=2*m*(m*m+d-b*b)/((d+(m-b)**2)*(d+(m+b)**2))
   errors.append(abs(direct-factor))
assert max(errors)<mp.mpf('1e-75')
checks['two_point_score_identity_max_error']=str(max(errors))
# Scale root for equally distant data is s=|residual| for every df>0.
for d in (mp.mpf('.01'),mp.mpf('.25'),mp.mpf(1),mp.mpf(3),mp.mpf('1e50')):
 assert abs(2*(d+1)*9/(d*9+9)-2)<mp.mpf('1e-75')
checks['scale_identity']='passed for 5 shapes, including below one'
# Actual-hold normal seam: fixed ordinary scale s and location mu gives
# A=sum(z^4-2*z^2-1)/4, whereas fixed sd gives
# A=sum(z^4-6*z^2+3)/4. Different constraints need different tail bounds.
for standard in (False,True):
 z=[mp.mpf(v) for v in [-2,-1,0,1,3]]
 def delta(h):
  nu=1/h
  s=mp.sqrt(1-2*h) if standard else mp.mpf(1)
  c=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-mp.log(s)
  return sum(c-(nu+1)*mp.log1p((v/s)**2/nu)/2+(mp.log(2*mp.pi)+v*v)/2 for v in z)
 A=sum(v**4-(6 if standard else 2)*v*v+(3 if standard else -1) for v in z)/4
 err=abs(delta(mp.mpf('1e-25'))/mp.mpf('1e-25')-A)
 assert err<mp.mpf('1e-20')
 checks['fixed_sd' if standard else 'fixed_ordinary_scale']={'A':str(A),'seam_error':str(err)}
for nu in map(mp.mpf, ['.00001','.01','.25','1','3','100']):
    density=mp.gamma((nu+1)/2)/(mp.gamma(nu/2)*mp.sqrt(nu*mp.pi))*(1+1/nu)**(-(nu+1)/2)
    assert density <= mp.sqrt(nu/(2*mp.pi))
checks['lower_shape_uniform_bound']='passed at 6 shapes'
for nu in map(mp.mpf,['1','3','100','10000']):
    for z in map(mp.mpf,['0','.1','1','3','20']):
        ll=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-(nu+1)*mp.log1p(z*z/nu)/2
        normal=-(mp.log(2*mp.pi)+z*z)/2
        assert ll-normal <= z**4*(1/nu+1/nu**2)/4
checks['normal_tail_value_bound']='passed at 20 shape/residual pairs'
# Uniform-in-location held-scale shape-score envelope and analytic tail.
n=7; Q=mp.mpf(10000)
b=lambda v,q:mp.log1p(q/v)-(v+1)*q/(v*(v+q))
cp=lambda v:(mp.digamma((v+1)/2)-mp.digamma(v/2)-1/v)/2
bounds=[]
for j in range(101):
    v=mp.exp(mp.log(Q)*j/100)
    B=n*cp(v)-(b(v,Q)+(n-1)*b(v,1))/2
    assert B<0
    assert cp(v)<=1/(4*v*v)
    bounds.append(B)
assert Q*Q/8-Q>(2*n-1)/2
checks['held_scale_shape_envelope']={'largest_grid_bound':str(max(bounds)),'shapes':101,'analytic_tail':'Q^2/8-Q > (2*n-1)/2 passed'}
print(json.dumps(checks,indent=2))
