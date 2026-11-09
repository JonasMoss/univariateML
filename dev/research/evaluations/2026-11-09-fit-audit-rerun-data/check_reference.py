#!/usr/bin/env python3
"""Independent integral/normalization checks for stable oracle rewrites."""
import json,pathlib
import mpmath as mp
from reference_extra import adapt
mp.mp.dps=70
results=[]
def check(name,got,want):
 err=abs(got-want)/max(1,abs(want));assert err<mp.mpf('1e-55'),(name,got,want,err)
 results.append(dict(check=name,relative_error=str(err)))
def f(ident,names,theta,x):return adapt(ident,names,lambda *a:None)(*map(mp.mpf,theta),mp.mpf(x))
phi=lambda z:mp.exp(-z*z/2)/mp.sqrt(2*mp.pi)
check('dnorm integral',mp.exp(f('dnorm',['mean','sd'],['.2','1.3'],2)),mp.quad(lambda v:phi((v-mp.mpf('.2'))/mp.mpf('1.3'))/mp.mpf('1.3'),[2,3]))
check('dgamma integral',mp.exp(f('dgamma',['shape','rate'],['2.3','.7'],2)),mp.quad(lambda v:mp.mpf('.7')**mp.mpf('2.3')*v**mp.mpf('1.3')*mp.exp(-mp.mpf('.7')*v)/mp.gamma(mp.mpf('2.3')),[2,3]))
check('bhatt convolution',mp.exp(f('bhatt',['mu','sigma','a'],['.2','1.3','.7'],2)),mp.quad(lambda v:phi((2-mp.mpf('.2')-v)/mp.mpf('1.3'))/mp.mpf('1.3')/mp.mpf('1.4'),[-mp.mpf('.7'),mp.mpf('.7')]))
check('slash integral',mp.exp(f('slash',['mu','sigma'],['.2','1.3'],2)),mp.quad(lambda u:u*phi((2-mp.mpf('.2'))*u/mp.mpf('1.3'))/mp.mpf('1.3'),[0,1]))
for a in ['-.5','0','.5','1']:
 for u in ['.2','.5','.8']:
  a,u=mp.mpf(a),mp.mpf(u);mu,s=mp.mpf('.2'),mp.mpf('1.3')
  q=lambda v:mu+s*(mp.log(v/(1-v)) if a==0 else (v**a-(1-v)**a)/a)
  check('tlambda quantile Jacobian '+str(a)+' '+str(u),mp.exp(f('tlambda',['mu','sigma','lambda'],[mu,s,a],q(u)))*mp.diff(q,u),mp.mpf(1))
for ident,names,theta,lo,hi in [('cmb',['size','prob','nu'],[7,'.3','1.7'],0,7),('tbinom',['size','prob','a','b'],[7,'.3',1,6],2,6),('tpois',['lambda','a','b'],['2.3',1,6],2,6),('zipf',['N','s'],[7,'1.7'],1,7)]:
 check(ident+' normalization',mp.fsum(mp.exp(f(ident,names,theta,k)) for k in range(lo,hi+1)),mp.mpf(1))
check('cmp Poisson member',f('cmp',['lambda','nu'],['2.3',1],3),3*mp.log(mp.mpf('2.3'))-mp.mpf('2.3')-mp.loggamma(4))
check('tnorm integral ratio',mp.exp(f('tnorm',['mean','sd','a','b'],['.2','1.3',0,3],2)),phi((2-mp.mpf('.2'))/mp.mpf('1.3'))/mp.mpf('1.3')/mp.quad(lambda v:phi((v-mp.mpf('.2'))/mp.mpf('1.3'))/mp.mpf('1.3'),[0,3]))
check('gpd uniform closed endpoint',f('gpd',['mu','sigma','xi'],[0,2,-1],2),-mp.log(2))
check('gev closed endpoint',f('gev',['mu','sigma','xi'],[0,2,-1],2),-mp.log(2))
for ident,names,theta,last in [('zmgeom',['prob','p0'],[1,'.3'],1),('zmpois',['lambda','p0'],[0,'.3'],1),('zmlogarithmic',['prob','p0'],[0,'.3'],1),('zmbinom',['size','prob','p0'],[4,1,'.3'],4)]:
 check(ident+' finite atom normalization',mp.exp(f(ident,names,theta,0))+mp.exp(f(ident,names,theta,last)),mp.mpf(1))
with mp.workdps(280):
 b=mp.mpf('1e200');x=mp.mpf('1e-200')
 generic=mp.loggamma(b+2)-mp.loggamma(b)+mp.log(x)+(b-1)*mp.log1p(-x)
 exact=mp.log(b)+mp.log(b+1)+mp.log(x)+(b-1)*mp.log1p(-x)
 check('beta large-shape digit escalation',generic,exact)
# Independent finite half-integer Bessel polynomial, beyond golden support.
mu,shape,x=mp.mpf('1.978'),mp.mpf('.04107012497414491'),mp.mpf(100)
z=mp.sqrt(shape*(2+shape/mu**2));n=int(x)-1
poly=mp.fsum(mp.factorial(n+j)/(mp.factorial(j)*mp.factorial(n-j))*(2*z)**(-j) for j in range(n+1))
polynomial=-2*mu/(1+mp.sqrt(1+2*mu**2/shape))+x*mp.log(mu/mp.sqrt(1+2*mu**2/shape))-mp.loggamma(x+1)+mp.log(poly)
bessel=mp.log(2*shape/mp.pi)/2-mp.loggamma(x+1)+shape/mu+(x-mp.mpf('.5'))*mp.log(shape/(2+shape/mu**2))/2+mp.log(mp.besselk(x-mp.mpf('.5'),z))
check('pig Bessel vs independent polynomial at count 100',bessel,polynomial)
check('emg independent convolution integral',mp.exp(f('emg',['mu','sigma','lambda'],['.2','1.3','1.2'],2)),mp.quad(lambda v:mp.mpf('1.2')*mp.exp(-mp.mpf('1.2')*v)*phi((2-mp.mpf('.2')-v)/mp.mpf('1.3'))/mp.mpf('1.3'),[0,mp.inf]))
check('emg extreme normal limit',f('emg',['mu','sigma','lambda'],[0,'1e150',1],'-2e150'),-mp.log(mp.mpf('1e150'))-mp.log(2*mp.pi)/2-2)
with mp.workdps(400):
 check('tnorm extreme exponential tilt limit',f('tnorm',['mean','sd','a','b'],['1e150',1,0,'1e-150'],'3e-151'),mp.log(mp.mpf('1e150'))+mp.mpf('.3')-mp.log(mp.expm1(1)))
check('triangular endpoint mode',f('triang',['a','b','c'],[0,1,1],1),mp.log(2))
check('zero-modified NB logarithmic member',f('zmnbinom',['size','prob','p0'],[0,'.4','.3'],3),mp.log(mp.mpf('.7'))+3*mp.log(mp.mpf('.6'))-mp.log(3)-mp.log(-mp.log(mp.mpf('.4'))))
check('CMP independent Bessel normalizer',f('cmp',['lambda','nu'],[100,2],3),3*mp.log(100)-2*mp.loggamma(4)-mp.log(mp.besseli(0,20)))
check('CMP centered series at mode one million',f('cmp',['lambda','nu'],['1e6',1],1000000),mp.mpf(1000000)*mp.log(mp.mpf(1000000))-mp.mpf(1000000)-mp.loggamma(1000001))
for exponent in ['.3','1','1.7']:
 check('Zipf Hurwitz identity '+exponent,f('zipf',['N','s'],[7,exponent],3),-mp.mpf(exponent)*mp.log(3)-mp.log(mp.fsum(mp.power(k,-mp.mpf(exponent)) for k in range(1,8))))
from tnorm_existence import criterion
assert criterion(dict(data=[0,.25,.5,.75,1],held=dict(a=0,b=1)))[0] is False
assert criterion(dict(data=[.4,.45,.5,.55,.6],held=dict(a=0,b=1)))[0] is True
assert criterion(dict(data=[.1,.2,.3,.4,.5],held=dict(a=0,b=mp.inf)))[0] is True
results.append(dict(check='truncated-normal dual-boundary attainment criteria',relative_error='theoretical sign tests passed'))
path=pathlib.Path(__file__).resolve().parent/'reference-validation.json';path.write_text(json.dumps(dict(validation='PASS',checks=results),indent=2));print('PASS',len(results),'independent oracle checks')
