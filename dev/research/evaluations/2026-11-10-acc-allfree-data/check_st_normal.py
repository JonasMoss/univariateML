"""Independent near-normal skew-t root on the R equivariance sample (50 digits)."""
import mpmath as m,json
from pathlib import Path
m.mp.dps=55
D=Path(__file__).resolve().parent
x=list(map(m.mpf,json.loads((D/'st-near-normal-data.json').read_text())))
def objective(*p):
 mu,ls,alpha,ln=p;omega=m.exp(ls);nu=1+m.exp(ln)
 constant=ls-m.loggamma((nu+1)/2)+m.loggamma(nu/2)+m.log(nu*m.pi)/2-m.log(2)
 f=len(x)*constant
 for v in x:
  z=(v-mu)/omega;h=alpha*z*m.sqrt((nu+1)/(nu+z*z));df=nu+1
  c=m.betainc(m.mpf('.5'),df/2,0,h*h/(df+h*h),regularized=True)
  prob=(1+c)/2 if h>=0 else (1-c)/2
  f+=(nu+1)*m.log1p(z*z/nu)/2-m.log(prob)
 return f
initial=[m.mpf('-.17292278716183115'),m.log(m.mpf('1.1288051218308588')),m.mpf('.21393697710454168'),m.log(m.mpf('1272.6522998630485')-1)]
def score(*p):
 result=[]
 for i in range(4):
  def one(t):
   q=list(p);q[i]=t;return objective(*q)
  result.append(m.diff(one,p[i]))
 return result
root=list(m.findroot(score,tuple(initial),tol=m.mpf('1e-43'),maxsteps=12))
coef=[root[0],m.exp(root[1]),root[2],1+m.exp(root[3])]
result=dict(precision_digits=55,method='independent mpmath incomplete-beta log density and differentiated Newton score',coefficient_names=['xi','omega','alpha','nu'],estimates=list(map(str,coef)),score=list(map(str,score(*root))),logLik=str(-objective(*root)))
(D/'st-near-normal-reference.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
