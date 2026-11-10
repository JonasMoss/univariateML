"""Independent 100-digit seam root and exact binary64 lattice obstruction."""
import json, mpmath as mp
from fractions import Fraction
mp.mp.dps=100
e=mp.mpf(float(.0028867513459481287)); data=[e,e,mp.mpf(1)]
m2=sum(z*z for z in data)/3
v=lambda a:2*m2/(1+mp.sqrt(1+4*m2*a*a))
g=lambda a:(sum(z*mp.tanh(a*z) for z in data)/3-a*v(a))/a**3
a=mp.findroot(g,(mp.mpf('.005'),mp.mpf('.02')))
mu=a*v(a); sd=mp.sqrt(v(a))
mean=Fraction.from_float(1e-158); square=mean*mean
nearest=Fraction.from_float(float(square))
lattice_error=abs(nearest/square-1)
out={'precision':100,'foldnorm':{'data':[str(z) for z in data],'mu':str(mu),'sigma':str(sd),'score_over_a_cubed':str(g(a))},
 'pig':{'mean':repr(float(mean)),'nearest_shape_relative_error':str(mp.mpf(lattice_error.numerator)/lattice_error.denominator),'root_relative_to_mean_squared_bound':'between 1 and 1+4*mean (mean<=1e-6)'},
 'gumbel':{'scale_in_denorm_min_interval':['0.4','0.45'],'equation':'s+1/(1+exp(1/s))-1/2=0'},
 'weibull':{'shape_interval':['3','4'],'scale_in_denorm_min_interval':['1.65','1.71']},
 'foldnorm_subnormal':{'scale_in_denorm_min_interval':['0.5','sqrt(0.5)'],'reason':'mu<=mean=0.5 and sigma^2=mean(x^2)-mu^2'}}
e2=mp.mpf(float(.02)); data2=[e2,e2,mp.mpf(1)]; m22=sum(z*z for z in data2)/3
v2=lambda a:2*m22/(1+mp.sqrt(1+4*m22*a*a))
g2=lambda a:(sum(z*mp.tanh(a*z) for z in data2)/3-a*v2(a))/a**3
a2=mp.findroot(g2,(mp.mpf('.05'),mp.mpf('.2')))
out['foldnorm_positive_seam']={'epsilon':repr(float(e2)),'mu':str(a2*v2(a2)),'sigma':str(mp.sqrt(v2(a2)))}
import math
critical=math.sqrt(9+4*math.sqrt(6)); rows=[]
for c in [math.nextafter(critical,0),critical,math.nextafter(critical,math.inf)]:
 xx=[mp.mpf(1)]*3+[mp.mpf(c)]; mm=sum(z*z for z in xx)/4
 fourth=sum(z**4 for z in xx)/4;sixth=sum(z**6 for z in xx)/4
 coefficient=mm**2-fourth/3
 row={'last_observation':repr(c),'quartic_coefficient':str(coefficient),'H0':str(coefficient/mm**2)}
 cf=Fraction.from_float(c); mf=(3+cf*cf)/4; exact=mf*mf-(3+cf**4)/12
 row['quartic_coefficient_exact_sign']=(exact>0)-(exact<0)
 if coefficient>0:
  vv=lambda a:2*mm/(1+mp.sqrt(1+4*mm*a*a))
  gg=lambda a:(sum(z*mp.tanh(a*z) for z in xx)/4-a*vv(a))/a**3
  a0=mp.sqrt(-coefficient/(2*sixth/15-2*mm**3))
  aa=mp.findroot(gg,(a0*mp.mpf('.9'),a0*mp.mpf('1.1')))
  row.update(mu=str(aa*vv(aa)),sigma=str(mp.sqrt(vv(aa))),zero_location_error_in_scale_units=str(aa*mp.sqrt(vv(aa))),score_over_a_cubed=str(gg(aa)))
 rows.append(row)
out['foldnorm_quartic_identification']=rows
gf=lambda s:s+1/(1+mp.exp(1/s))-mp.mpf('.5')
out['gumbel']['endpoint_scores']=[str(gf(mp.mpf(s))) for s in ['.4','.45']]
assert gf(mp.mpf('.4'))<0<gf(mp.mpf('.45'))
ws=lambda k:1/mp.mpf(k)+mp.log(2)/2-2**k*mp.log(2)/(1+2**k)
out['weibull']['endpoint_shape_scores']=[str(ws(k)) for k in [3,4]]
assert ws(3)>0>ws(4)
with mp.workdps(240):
 pm=mp.mpf(float(mean))
 def ps(t):
  q=mp.sqrt(t/(t+2));z=pm*mp.sqrt(t*(t+2))
  return 2*pm*q/(1+q)**2-1+(t+1)/(2*(1+z))
 pl,ph=ps(mp.mpf(1))/pm,ps(1+4*pm)/pm
 assert pl<0<ph
 out['pig']['root_bracket_endpoint_scores_divided_by_mean']=[mp.nstr(pl,60),mp.nstr(ph,60)]
 out['pig']['endpoint_working_precision']=240
print(json.dumps(out,indent=2))
