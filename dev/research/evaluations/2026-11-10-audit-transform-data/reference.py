#!/usr/bin/env python3
"""Independent raw-data score roots at two precisions, exact binary64 inputs."""
import gzip,json,pathlib
import mpmath as mp
HERE=pathlib.Path(__file__).resolve().parent
CASES=json.loads(gzip.decompress((HERE.parent/'2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
OLD={r['index']:r for r in map(json.loads,gzip.decompress((HERE.parent/'2026-11-09-fit-audit-rerun-data/reference.jsonl.gz').read_bytes()).splitlines())}
NAMES={'invgamma':['alpha','beta'],'naka':['shape','scale'],'lgamma':['shapelog','ratelog'],'lnorm':['meanlog','sdlog'],'logitnorm':['mu','sigma'],'llogis':['shape','scale'],'invweibull':['shape','scale'],'invburr':['shape1','shape2','scale']}
def raw_ll(f,x,p):
 n=len(x)
 if f=='invgamma':
  a,b=p;return n*(a*mp.log(b)-mp.loggamma(a))-(a+1)*mp.fsum(map(mp.log,x))-b*mp.fsum(1/v for v in x)
 if f=='naka':
  a,b=p;return n*(mp.log(2)+a*mp.log(a/b)-mp.loggamma(a))+(2*a-1)*mp.fsum(map(mp.log,x))-a*mp.fsum(v*v for v in x)/b
 if f=='lgamma':
  a,b=p;y=list(map(mp.log,x));return n*(a*mp.log(b)-mp.loggamma(a))+(a-1)*mp.fsum(map(mp.log,y))-b*mp.fsum(y)-mp.fsum(y)
 if f in ('lnorm','logitnorm'):
  a,b=p;y=[mp.log(v) if f=='lnorm' else mp.log(v/(1-v)) for v in x]
  jac=-mp.fsum(mp.log(v) if f=='lnorm' else mp.log(v)+mp.log1p(-v) for v in x)
  return -n*(mp.log(b)+mp.log(2*mp.pi)/2)-mp.fsum(((v-a)/b)**2 for v in y)/2+jac
 if f=='invweibull':
  a,b=p;return mp.fsum(mp.log(a)-mp.log(v)-a*mp.log(v/b)-mp.exp(-a*mp.log(v/b)) for v in x)
 a,b,s=(1,p[0],p[1]) if f=='llogis' else p
 sign=1 if f=='llogis' else -1
 return mp.fsum(mp.log(a)+mp.log(b)-mp.log(v)-mp.log1p(mp.exp(-sign*b*mp.log(v/s)))-a*mp.log1p(mp.exp(sign*b*mp.log(v/s))) for v in x)
def solve(c,dps):
 mp.mp.dps=dps;f=c['family'];x=list(map(mp.mpf,c['data']));n=len(x)
 start=list(map(mp.mpf,OLD[c['index']].get('refined_theta',OLD[c['index']]['theta']))); held=c['held']
 if f in ('lnorm','logitnorm'):
  y=[mp.log(v) if f=='lnorm' else mp.log(v/(1-v)) for v in x];m=mp.fsum(y)/n;s=mp.sqrt(mp.fsum((v-m)**2 for v in y)/n);p=[m,s]
 elif f in ('invgamma','lgamma','naka'):
  y=[1/v if f=='invgamma' else v*v if f=='naka' else mp.log(v) for v in x];m=mp.fsum(y)/n;lm=mp.fsum(map(mp.log,y))/n
  # log-shape score, independent of transformed binary64 observations.
  root=mp.findroot(lambda z:mp.log(mp.exp(z))-mp.digamma(mp.exp(z))-mp.log(m)+lm,mp.log(start[0]),tol=mp.mpf(10)**(-dps+40))
  a=mp.exp(root);p=[a,m if f=='naka' else a/m]
 elif f in ('llogis','invweibull') or (f=='invburr' and 'shape1' in held):
  a=mp.mpf(held['shape1']) if f=='invburr' else mp.mpf(1);sign=-1 if f=='invburr' else 1
  y=[sign*mp.log(v/x[0]) for v in x];unit=max(map(abs,y));y=[v/unit for v in y]
  b0=start[-2] if f=='invburr' else start[0];s0=start[-1]
  t0=mp.log(b0*unit);c0=b0*sign*mp.log(s0/x[0])
  def scores(t,c):
   b=mp.exp(t);z=[b*v-c for v in y]
   if f=='invweibull':
    w=[mp.exp(-v) for v in z];return (n-mp.fsum(w),n+mp.fsum(b*v*(w0-1) for v,w0 in zip(y,w)))
   w=[1/(1+mp.exp(-v)) for v in z]
   return (-n+(a+1)*mp.fsum(w),n+mp.fsum(b*v*(1-(a+1)*w0) for v,w0 in zip(y,w)))
  if len(held)==2:p=start
  else:
   t,c0=mp.findroot(scores,(t0,c0),tol=mp.mpf(10)**(-dps+40));b=mp.exp(t)/unit;s=x[0]*mp.exp(sign*c0/b);p=[a,b,s] if f=='invburr' else [b,s]
 else:
  s=mp.mpf(held['scale']);y=[-mp.log(v/s) for v in x]
  def score(t):
   b=mp.exp(t);g=mp.fsum(mp.log1p(mp.exp(b*v)) for v in y);gp=mp.fsum(v/(1+mp.exp(-b*v)) for v in y)
   return n-(n/g+1)*b*gp+b*mp.fsum(y)
  t=mp.findroot(score,mp.log(start[1]),tol=mp.mpf(10)**(-dps+40));b=mp.exp(t);a=n/mp.fsum(mp.log1p(mp.exp(b*v)) for v in y);p=[a,b,s]
 return dict(index=c['index'],family=f,theta=[mp.nstr(v,100) for v in p],logLik=mp.nstr(raw_ll(f,x,p),100))
def main():
 out=[];path=HERE/'reference.json';cache={r['index']:r for r in json.loads(path.read_text())} if path.exists() else {}
 for target in json.loads((HERE/'targets.json').read_text())+[dict(index=9806)]:
  i=target['index']
  if i in cache:out.append(cache[i]);continue
  r=solve(CASES[i],180);rr=solve(CASES[i],360)
  error=max(abs(mp.mpf(v)-mp.mpf(w))/max(1,abs(mp.mpf(w))) for v,w in zip(r['theta']+[r['logLik']],rr['theta']+[rr['logLik']]))
  assert error<mp.mpf('1e-90'),(i,error)
  rr['precision_check']='180/360 digits agree within 1e-90';out.append(rr);cache[i]=rr;path.write_text(json.dumps(list(cache.values()),indent=2)+'\n');print(i,rr['family'],flush=True)
if __name__=='__main__':main()
