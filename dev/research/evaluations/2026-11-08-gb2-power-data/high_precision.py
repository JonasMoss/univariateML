import json
import mpmath as m
m.mp.dps=80
x=list(map(m.mpf,['.03','.1','.3','.7','1','2','5','20']))
y=list(map(m.log,x)); n=len(y);p=m.mpf(3);q=m.mpf(2)
def score(a,l):
 t=[a*(v-l) for v in y]
 r=[p-(p+q)/(1+m.exp(-v)) for v in t]
 return n/a+sum((v-l)*w for v,w in zip(y,r)),sum(r)
rows=[]
for subset in range(3):
 if subset==0:a,l=m.findroot(score,(.5,-1.2))
 if subset==1:a=m.findroot(lambda a:score(a,0)[0],.47);l=m.mpf(0)
 if subset==2:a=m.mpf(2);l=m.findroot(lambda l:score(a,l)[1],-.7)
 ll=sum(m.log(a)-v-m.log(m.beta(p,q))-p*m.log(1+m.exp(-a*(v-l)))-q*m.log(1+m.exp(a*(v-l))) for v in y)
 rows.append(dict(subset=subset,power=str(a),scale=str(m.exp(l)),logLik=str(ll)))
for inverse in [False,True]:
 z=[-v if inverse else v for v in y];Q=m.mpf(3 if inverse else 2)
 def mixed(P,A):
  t=[A*v for v in z]
  return n*(m.digamma(P+Q)-m.digamma(P))-sum(m.log(1+m.exp(-v)) for v in t),n/A+sum(v*(P-(P+Q)/(1+m.exp(-A*v))) for v in z)
 P,A=m.findroot(mixed,(3.3,.43) if inverse else (1.8,.58))
 rows.append(dict(subset='mixed reciprocal' if inverse else 'mixed direct',beta_shape=str(P),power=str(A)))
g=json.load(open('univariateML-cpp/include/cppunivariate/distributions/fpareto/golden.json'))
x=list(map(m.mpf,g['data']));y=list(map(m.log,x));n=len(y)
p=m.mpf(g['estimates']['shape3']);q=m.mpf(g['estimates']['shape1'])
a,l=m.findroot(score,(2,0))
rows.append(dict(subset='fpareto golden',power=str(a),scale=str(m.exp(l)),relative_power_drift=str(abs(a/m.mpf(g['estimates']['shape2'])-1)),relative_scale_drift=str(abs(m.exp(l)/m.mpf(g['estimates']['scale'])-1))))
import math
p=m.mpf(math.nextafter(10,math.inf));q=m.mpf(10);a=m.mpf(1e-300)
rows.append(dict(subset='constant tiny held power',log_scale=str(m.log(2)+m.log(q/p)/a),double_log_difference=math.log(float(p))-math.log(float(q))))
p,u=m.findroot(lambda p,u:(1/p+1/(p+1)-m.log(1+m.exp(u)),1/u-p+(p+2)/(1+m.exp(u))),(.4,3.3))
a=u/m.log(2);x=m.mpf('.5')
ll=m.log(a)-m.log(x)-m.log(m.beta(p,2))-p*m.log(1+m.exp(u))-2*m.log(1+m.exp(-u))
rows.append(dict(subset='constant mixed finite stationary witness',p=str(p),power=str(a),log_density=str(ll),power_face_log_density=str(-m.log(m.log(2))-1-m.log(x))))
for index,case in enumerate(json.load(open('dev/research/evaluations/2026-11-08-gb2-power-data/curvature.json')),1):
 w=list(map(m.mpf,case['log_x_over_b']));Q=m.mpf(case['q']);A=m.mpf(case['a']);N=len(w)
 S=sum(m.log(1+m.exp(-A*v)) for v in w)/N
 start=m.mpf(case['inner_p'])
 P=m.findroot(lambda P:m.digamma(P+Q)-m.digamma(P)-S,(start*m.mpf('.99'),start*m.mpf('1.01')))
 T=sum(v/(1+m.exp(A*v)) for v in w)
 Haa=-N/A**2-(P+Q)*sum(v*v*m.exp(-abs(A*v))/(1+m.exp(-abs(A*v)))**2 for v in w)
 Hpp=N*(m.polygamma(1,P+Q)-m.polygamma(1,P))
 curvature=Haa-T*T/Hpp
 assert curvature>0
 rows.append(dict(subset='profile curvature counterexample '+str(index),inner_p=str(P),profile_curvature=str(curvature)))
print(json.dumps(rows,indent=2))
