#!/usr/bin/env python3
"""Add finite-parameter sequences toward named faces, using independent scipy quantiles.
Quantiles generate data only; likelihood references remain 70-digit README densities.
"""
import json,math,pathlib
import numpy as np
from scipy import stats,special
import audit
HERE=pathlib.Path(__file__).resolve().parent
u=(np.arange(12)+.5)/12
recipes=[]
def add(f,face,k,x,parameters):
 x=np.asarray(x,dtype=float)
 if f in ['beta','gamma','invgamma','naka','weibull','invweibull','lnorm','logitnorm','exp','invgauss','fatigue','betapr','burr','invburr','gb2','fpareto','genbeta'] and np.any(x<=0):return
 if np.all(np.isfinite(x)) and len(x):recipes.append(dict(family=f,face=face,k=k,data=x.tolist(),generating_parameters=parameters))
for K in [100.,1000000.]:
 e=1/K
 add('beta','point-mass',K,stats.beta.ppf(u,.4*K,.6*K),dict(shape1=.4*K,shape2=.6*K))
 add('gamma','point-mass',K,stats.gamma.ppf(u,K,scale=1/K),dict(shape=K,scale=1/K))
 add('invgamma','point-mass',K,stats.invgamma.ppf(u,K,scale=K),dict(alpha=K,beta=K))
 add('naka','point-mass',K,np.sqrt(stats.gamma.ppf(u,K,scale=1/K)),dict(shape=K,scale=1))
 add('weibull','point-mass',K,stats.weibull_min.ppf(u,K),dict(shape=K,scale=1))
 add('invweibull','point-mass',K,stats.invweibull.ppf(u,K),dict(shape=K,scale=1))
 add('lnorm','point-mass',K,np.exp(stats.norm.ppf(u)*e),dict(meanlog=0,sdlog=e))
 add('logitnorm','point-mass',K,special.expit(stats.norm.ppf(u)*e),dict(mu=0,sigma=e))
 add('exp','point-mass',K,stats.expon.ppf(u)*e,dict(rate=K))
 add('invgauss','point-mass',K,stats.invgauss.ppf(u,1/K,scale=K),dict(mean=1,shape=K))
 add('fatigue','point-mass',K,stats.fatiguelife.ppf(u,e),dict(alpha=e,beta=1,mu=0))
 add('std','normal',K,stats.t.ppf(u,K)*np.sqrt((K-2)/K),dict(mean=0,sd=1,nu=K))
 add('lst','normal',K,stats.t.ppf(u,K),dict(mu=0,sigma=1,df=K))
 add('ht','half-normal',K,stats.t.ppf(.5+.5*u,K),dict(mu=0,sigma=1,nu=K))
 add('sn','half-normal',K,stats.skewnorm.ppf(u,K),dict(xi=0,omega=1,alpha=K))
 add('sn','reflected-half-normal',K,stats.skewnorm.ppf(u,-K),dict(xi=0,omega=1,alpha=-K))
 add('gompertz','exponential',K,np.log1p(-e*np.log1p(-u))/e,dict(a=1,b=e))
 add('betapr','gamma',K,K*stats.betaprime.ppf(u,2,K),dict(shape1=2,shape2=K,scale=K))
 add('betapr','inverse-gamma',K,stats.betaprime.ppf(u,K,2)/K,dict(shape1=K,shape2=2,scale=1/K))
 for f in ['burr','invburr']:
  x=np.sqrt(K*np.expm1(-np.log1p(-u)/K))
  add(f,'weibull' if f=='burr' else 'inverse-weibull',K,x if f=='burr' else 1/x,dict(shape1=K,shape2=2,scale=np.sqrt(K) if f=='burr' else 1/np.sqrt(K)))
 # GB2 lower/upper beta shapes p,q correspond to shape3,shape1.
 for f in ['gb2','fpareto']:
  for face,p,q,a,b in [('trgamma',2,K,2,np.sqrt(K)),('invtrgamma',K,2,2,1/np.sqrt(K)),('lnorm',K,K,np.sqrt(2*special.polygamma(1,K)),1),('pareto',2,2/K,K,1),('power',2/K,2,K,1),('double-pareto',2/K,3/K,K,1)]:
   y=stats.betaprime.ppf(u,p,q)
   add(f,face,K,b*np.exp(np.log(y)/a),dict(shape1=q,shape2=a,shape3=p,scale=b,**({'min':0} if f=='fpareto' else {})))
 add('genbeta','log-gamma',K,stats.beta.ppf(u,K,2)**K,dict(shape1=K,shape2=2,shape3=e,scale=1))
 add('genbeta','power',K,stats.beta.ppf(u,2/K,2)**e,dict(shape1=2/K,shape2=2,shape3=K,scale=1))
 for f in ['binom','zib','zmbinom','ztbinom']:
  x=stats.binom.ppf(u,int(K),2/K)
  if f=='zib':x=np.where(u<.25,0,stats.binom.ppf(np.maximum((u-.25)/.75,.001),int(K),2/K))
  if f in ['zmbinom','ztbinom']:x=np.maximum(x,1)
  add(f,'Poisson',K,x,dict(size=int(K),prob=2/K))
 for f in ['nbinom','zinb','zmnbinom','ztnbinom']:
  x=stats.nbinom.ppf(u,K,K/(K+2))
  if f=='zinb':x=np.where(u<.25,0,stats.nbinom.ppf(np.maximum((u-.25)/.75,.001),K,K/(K+2)))
  if f in ['zmnbinom','ztnbinom']:x=np.maximum(x,1)
  add(f,'Poisson',K,x,dict(size=K,prob=K/(K+2)))
 add('bbinom','binomial',K,stats.betabinom.ppf(u,10,.3*K,.7*K),dict(size=10,alpha=.3*K,beta=.7*K))
 add('bbinom','endpoint-two-point',K,stats.betabinom.ppf(u,10,.3/K,.7/K),dict(size=10,alpha=.3/K,beta=.7/K))
 # Independent convolution draws coupled by quantile rank: these are data
 # sequences converging to the stated limit; not convolution quantiles.
 add('emg','normal',K,stats.norm.ppf(u)+stats.expon.ppf(u)*e,dict(mu=0,sigma=1,lambda_=K))
 add('emg','translated-exponential',K,stats.expon.ppf(u)+stats.norm.ppf(u)*e,dict(mu=0,sigma=e,lambda_=1))
 add('bhatt','uniform',K,2*u-1+e*stats.norm.ppf(u),dict(mu=0,sigma=e,a=1))
 for f in ['ged','huber','foldnorm','hnorm','hcauchy','cauchy','norm','gumbel','logis','laplace','rayleigh']:
  if f=='ged':x=stats.gennorm.ppf(u,K,scale=np.sqrt(special.gamma(1/K)/special.gamma(3/K)))
  elif f=='huber':x=stats.norm.ppf(u)+e*(2*u-1)
  elif f in ['hnorm','hcauchy','rayleigh']:x=np.abs(stats.norm.ppf(.5+.5*u))*e
  else:x=1+e*stats.norm.ppf(u)
  add(f,'uniform' if f=='ged' else 'normal' if f=='huber' else 'collapse',K,x,dict(sequence='finite deformation',K=K))
cases=json.loads((HERE/'cases.json').read_text());cases=[c for c in cases if not c['sample'].startswith(('face-','constant-extra-'))]
metadata={m['id']:m for _,m,_ in audit.models()}
for recipe in recipes:
 f=recipe['family'];m=metadata[f];seen=set()
 for old in [c for c in cases if c['family']==f and not c['sample'].startswith('face-')]:
  if old['mask'] in seen:continue
  seen.add(old['mask']);x=recipe['data'];h=old['held']
  env={**h,'inf':math.inf,'Inf':math.inf,'max':max,'min':min,'floor':math.floor,'ceil':math.ceil}
  compatible=True
  for key in ['lower','upper']:
   try:
    v=float(eval(str(m['support'][key]),{'__builtins__':{}},env))
    if key=='lower' and min(x)<v or key=='upper' and max(x)>v:compatible=False
   except Exception:pass
  if not compatible:continue
  cases.append(dict(index=len(cases),family=f,mask=old['mask'],held=h,sample=f"face-{recipe['face']}-{recipe['k']:g}",data=[int(v) for v in x] if m['support']['type']=='integer' else x,generating_parameters=recipe['generating_parameters']))
(HERE/'cases.json').write_text(json.dumps(cases));(HERE/'limit-samples.json').write_text(json.dumps(recipes,indent=2));print('finite face sequences',len(recipes),'families',len({r['family'] for r in recipes}),'total cases',len(cases),flush=True)
