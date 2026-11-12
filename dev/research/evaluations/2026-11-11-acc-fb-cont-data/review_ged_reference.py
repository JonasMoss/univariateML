from pathlib import Path
import mpmath as m,json,subprocess,hashlib
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';m.mp.dps=100
x=list(map(m.mpf,[.7321666961121761,2.2766042947262384,-.5491691631533211]));sd=m.mpf(float(1.0178559902092952))
def ll(mu,u):
 nu=m.exp(u);C=sd*m.sqrt(m.gamma(1/nu)/m.gamma(3/nu));return m.fsum(m.log(nu)-m.log(2*C)-m.loggamma(1/nu)-(abs(v-mu)/C)**nu for v in x)
def g(mu,u):return m.diff(lambda t:ll(t,u),mu),m.diff(lambda t:ll(mu,t),u)
mu,u=m.findroot(g,(m.mpf('.86371756578646'),m.log(m.mpf('27.4973366168316843'))),tol=m.mpf('1e-80'),maxsteps=40)
h=m.matrix([[m.diff(lambda t:g(t,u)[0],mu),m.diff(lambda t:g(mu,t)[0],u)],[m.diff(lambda t:g(t,u)[1],mu),m.diff(lambda t:g(mu,t)[1],u)]])
assert h[0,0]<0 and m.det(h)>0
r=dict(case=31101,data=[float(v) for v in x],held=dict(sd=float(sd),xi=1.),digits=100,reference=dict(mean=m.nstr(mu,85),sd=m.nstr(sd,85),nu=m.nstr(m.exp(u),85)),information_determinant=m.nstr(m.det(h),50),residual=m.nstr(max(map(abs,g(mu,u))),30))
(p/'review-ged-31101-reference.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
