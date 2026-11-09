"""Stable evaluations of the same README densities; no fitter calls."""
import functools
import mpmath as mp

def adapt(ident,names,default):
 def normal_mass(lo,hi):
  if lo>0:return (mp.erfc(lo/mp.sqrt(2))-mp.erfc(hi/mp.sqrt(2)))/2
  return (mp.erfc(-hi/mp.sqrt(2))-mp.erfc(-lo/mp.sqrt(2)))/2
 @functools.lru_cache(maxsize=128)
 def normconst(theta,precision):
  t=dict(zip(names,theta))
  if ident=='cmp':
   l,v=t['lambda'],t['nu']
   if l==0:return mp.mpf(0)
   if v==0:return -mp.log1p(-l)
   q=mp.exp(mp.log(l)/v)
   if q>mp.mpf('1e8'):raise ValueError('CMP independent centered-series mode exceeds audit budget')
   m=int(q);logpeak=m*mp.log(l)-v*mp.loggamma(m+1);total=mp.mpf(1)
   term=mp.mpf(1);k=m
   while k>0:
    term*=mp.power(k,v)/l;k-=1;total+=term;ratio=mp.power(k,v)/l
    if ratio<1 and term*ratio/(1-ratio)<mp.eps*total:break
   term=mp.mpf(1);k=m
   while True:
    k+=1;term*=l/mp.power(k,v);total+=term;ratio=l/mp.power(k+1,v)
    if ratio<1 and term*ratio/(1-ratio)<mp.eps*total:break
   return logpeak+mp.log(total)
  if ident=='zipf':
   with mp.workdps(mp.mp.dps+30):
    value=mp.digamma(t['N']+1)+mp.euler if t['s']==1 else mp.zeta(t['s'])-mp.zeta(t['s'],t['N']+1)
   return mp.log(value)
  if ident in ['cmb','tbinom','tpois']:
   if ident=='cmb':lo,hi=0,int(t['size'])
   else:lo=max(0,int(mp.floor(t['a']))+1);hi=int(mp.floor(t['b'])) if ident=='tpois' else min(int(t['size']),int(mp.floor(t['b'])))
   def logterm(k):
    if ident=='tpois':return k*mp.log(t['lambda'])-mp.loggamma(k+1)
    p,n=t['prob'],t['size']
    a=k*mp.log(p)+(n-k)*mp.log1p(-p)
    if ident=='cmb':return a+t['nu']*(mp.loggamma(n+1)-mp.loggamma(k+1)-mp.loggamma(n-k+1))
    return a-mp.loggamma(k+1)-mp.loggamma(n-k+1)
   terms=[logterm(k) for k in range(lo,hi+1)];peak=max(terms)
   return peak+mp.log(mp.fsum(mp.exp(z-peak) for z in terms))
 def fn(*args):
  theta=tuple(args[:-1]);x=args[-1];t=dict(zip(names,theta))
  # Proper finite atomic members, evaluated by their defining mass functions.
  if ident in ['pois','cmp','zip'] and t.get('lambda')==0:return mp.mpf(0) if x==0 else -mp.inf
  if ident in ['zip','zib','zinb'] and t.get('pi')==1:return mp.mpf(0) if x==0 else -mp.inf
  if ident in ['geom','ztgeom'] and t.get('prob')==1:return mp.mpf(0) if x==(0 if ident=='geom' else 1) else -mp.inf
  if ident in ['binom','zib'] and t.get('prob') in [0,1]:
   atom=0 if t['prob']==0 else t['size']
   if ident=='zib':
    if x==0:return mp.log(t['pi']+(1-t['pi'])*(atom==0))
    return mp.log1p(-t['pi']) if x==atom else -mp.inf
   return mp.mpf(0) if x==atom else -mp.inf
  if ident in ['nbinom','zinb'] and (t.get('size')==0 or t.get('prob')==1):return mp.mpf(0) if x==0 else -mp.inf
  if ident=='ztnbinom' and t['prob']==1:return mp.mpf(0) if x==1 else -mp.inf
  if ident=='ztnbinom' and t['size']==0:return x*mp.log1p(-t['prob'])-mp.log(x)-mp.log(-mp.log(t['prob']))
  if ident=='ztpois' and t['lambda']==0:return mp.mpf(0) if x==1 else -mp.inf
  if ident=='dlaplace' and t['scale']==0:return mp.mpf(0) if x==t['location'] else -mp.inf
  if ident=='dweibull' and t['shape1']==0:return mp.mpf(0) if x==0 else -mp.inf
  if ident=='skellam' and (t['mu1']==0 or t['mu2']==0):
   rate=max(t['mu1'],t['mu2']);k=x if t['mu2']==0 else -x
   return (mp.mpf(0) if k==0 else -mp.inf) if rate==0 else k*mp.log(rate)-rate-mp.loggamma(k+1) if k>=0 else -mp.inf
  if ident=='emg':
   lam,mu,s=t['lambda'],t['mu'],t['sigma'];z=(mu+lam*s*s-x)/(mp.sqrt(2)*s)
   if z>max(20,mp.sqrt(3*mp.mp.dps)):
    term=total=mp.mpf(1);k=1
    while True:
     term*=-(2*k-1)/(2*z*z);total+=term
     if abs(term)<mp.eps*abs(total):break
     k+=1
    logerfcx=-mp.log(mp.pi)/2-mp.log(z)+mp.log(total)
   elif z>0:logerfcx=mp.log(mp.erfc(z))+z*z
   else:return mp.log(lam/2)+lam*(mu-x)+lam*lam*s*s/2+mp.log(mp.erfc(z))
   return mp.log(lam/2)-(x-mu)**2/(2*s*s)+logerfcx
  if ident=='triang':
   a,b,c=t['a'],t['b'],t['c'];width=b-a
   if x==c:return mp.log(2)-mp.log(width)
   return mp.log(2)-mp.log(width)+(mp.log((x-a)/(c-a)) if x<c else mp.log((b-x)/(b-c)))
  if ident in ['gev','gpd'] and t['xi']==-1:
   z=(x-t['mu'])/t['sigma'];w=1-z
   # Support feasibility is checked independently with exact rational arithmetic.
   if w<0:return -mp.inf
   return -mp.log(t['sigma']) if ident=='gpd' else -mp.log(t['sigma'])-w
  if ident=='tnorm':
   mu,s=t['mean'],t['sd'];return -mp.log(s)-mp.log(2*mp.pi)/2-(x-mu)**2/(2*s*s)-mp.log(normal_mass((t['a']-mu)/s,(t['b']-mu)/s))
  if ident in ['zmgeom','zmlogarithmic','zmpois','zmnbinom','zmbinom']:
   p0=t['p0']
   if x==0:return mp.log(p0)
   if p0==1:return -mp.inf
   atomic=(ident=='zmgeom' and t['prob']==1) or (ident=='zmlogarithmic' and t['prob']==0) or (ident=='zmpois' and t['lambda']==0)
   if atomic:return mp.log1p(-p0) if x==1 else -mp.inf
   if ident=='zmbinom' and t['prob']==1:return mp.log1p(-p0) if x==t['size'] else -mp.inf
   if ident=='zmbinom' and t['prob']==0:return mp.log1p(-p0) if x==1 else -mp.inf
   if ident=='zmnbinom' and t['prob']==1:return mp.log1p(-p0) if x==1 else -mp.inf
   if ident=='zmnbinom' and t['size']==0:return mp.log1p(-p0)+x*mp.log1p(-t['prob'])-mp.log(x)-mp.log(-mp.log(t['prob']))
  if ident=='tlambda':
   a,m,s=t['lambda'],t['mu'],t['sigma'];z=-abs((x-m)/s)
   if a==1:return -mp.log(2*s)
   if a>0 and z<=-1/a:return -mp.log(s) if a>1 and abs(z+1/a)<=mp.mpf(64)*mp.mpf(2)**-52/a else -mp.inf
   if z==0:lp=lc=-mp.log(2)
   else:
    def q(lp):
     lc=mp.log(-mp.expm1(lp))
     return lp-lc if a==0 else (mp.expm1(a*lp)-mp.expm1(a*lc))/a
    lo,hi=mp.mpf(-2),-mp.log(2)
    while q(lo)>z:lo*=2
    for _ in range(260):
     mid=(lo+hi)/2
     if q(mid)<z:lo=mid
     else:hi=mid
    lp=(lo+hi)/2;lc=mp.log(-mp.expm1(lp))
   v,w=(a-1)*lp,(a-1)*lc;peak=max(v,w)
   return -mp.log(s)-peak-mp.log(mp.exp(v-peak)+mp.exp(w-peak))
  if ident=='slash':
   z=(x-t['mu'])/t['sigma']
   return -mp.log(t['sigma'])-mp.log(2*mp.pi)/2-mp.log(2) if z==0 else -mp.log(t['sigma'])-mp.log(2*mp.pi)/2+mp.log(-mp.expm1(-z*z/2))-2*mp.log(abs(z))
  if ident=='dnorm':return mp.log(normal_mass((x-t['mean'])/t['sd'],(x+1-t['mean'])/t['sd']))
  if ident=='dgamma':return mp.log(mp.gammainc(t['shape'],t['rate']*x,t['rate']*(x+1)))-mp.loggamma(t['shape'])
  if ident=='bhatt':
   z=(x-t['mu'])/t['sigma'];a=t['a']
   return -mp.log(t['sigma'])-mp.log(2*mp.pi)/2-z*z/2 if a==0 else mp.log(normal_mass(z-a/t['sigma'],z+a/t['sigma']))-mp.log(2*a)
  if ident=='sn':
   z=(x-t['xi'])/t['omega'];return mp.log(2)-mp.log(t['omega'])-mp.log(2*mp.pi)/2-z*z/2+mp.log(mp.erfc(-t['alpha']*z/mp.sqrt(2))/2)
  if ident=='cmp':return x*mp.log(t['lambda'])-t['nu']*mp.loggamma(x+1)-normconst(theta,mp.mp.dps)
  if ident=='zipf':return -t['s']*mp.log(x)-normconst(theta,mp.mp.dps)
  if ident=='cmb':
   n,p,v=t['size'],t['prob'],t['nu'];return v*(mp.loggamma(n+1)-mp.loggamma(x+1)-mp.loggamma(n-x+1))+x*mp.log(p)+(n-x)*mp.log1p(-p)-normconst(theta,mp.mp.dps)
  if ident=='tpois':return x*mp.log(t['lambda'])-mp.loggamma(x+1)-normconst(theta,mp.mp.dps)
  if ident=='tbinom':
   n,p=t['size'],t['prob'];return x*mp.log(p)+(n-x)*mp.log1p(-p)-mp.loggamma(x+1)-mp.loggamma(n-x+1)-normconst(theta,mp.mp.dps)
  return default(*args)
 return fn
