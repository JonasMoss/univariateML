#!/usr/bin/env python3
"""Independent mpmath roots on exact binary64 inputs, at 350 and 700 digits.
Uses raw model score equations, not the C++ transformations/series.
"""
import gzip,json,pathlib
import mpmath as mp
HERE=pathlib.Path(__file__).resolve().parent
CASES=json.loads(gzip.decompress((HERE.parent/'2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
TARGETS=[248,249,251,252,254,255,256,257,260,5348,10791,10792,10799,11011,5666,5669,272]
def solve(c, digits):
 mp.mp.dps=digits
 x=list(map(mp.mpf,c['data'])); n=len(x); held={k:mp.mpf(v) for k,v in c['held'].items()}
 if c['family']=='betapr':
  s=held['scale']; y=[v/(s+v) for v in x]; m=mp.fsum(y)/n; var=mp.fsum((v-m)**2 for v in y)/n
  t=m*(1-m)/var-1
  lx=mp.fsum(mp.log(v) for v in y)/n; lc=mp.fsum(mp.log1p(-v) for v in y)/n
  def scores(u,v):
   a,b=mp.exp(u),mp.exp(v); p=mp.digamma(a+b)
   return a*(lx+p-mp.digamma(a)), b*(lc+p-mp.digamma(b))
  def jacobian(u,v):
   a,b=mp.exp(u),mp.exp(v); f,g=scores(u,v); tri=mp.polygamma(1,a+b)
   return mp.matrix([[f+a*a*(tri-mp.polygamma(1,a)),a*b*tri],
                     [a*b*tri,g+b*b*(tri-mp.polygamma(1,b))]])
  if 'shape1' in held:
   a=held['shape1'];target=mp.fsum(mp.log1p(v/s) for v in x)/n
   left=mp.mpf(0);right=max(mp.mpf(1),2*a/target)
   for _ in range(450):
    b=(left+right)/2;g=mp.digamma(a+b)-mp.digamma(b)-target
    if g>0:left=b
    else:right=b
   b=(left+right)/2
  else:
   u,v=mp.findroot(scores,(mp.log(m*t),mp.log((1-m)*t)),J=jacobian,tol=mp.mpf('1e-100'),maxsteps=100)
   a,b=mp.exp(u),mp.exp(v)
  ll=mp.fsum((a-1)*mp.log(v/s)-(a+b)*mp.log1p(v/s)-mp.log(s)-mp.log(mp.beta(a,b)) for v in x)
  theta=[a,b,s]
 elif c['family']=='nsbeta':
  lo,hi=held['min'],held['max']; y=[(v-lo)/(hi-lo) for v in x]
  fa='shape1' in held; h=held['shape1' if fa else 'shape2']
  target=-mp.fsum(mp.log1p(-v) if fa else mp.log(v) for v in y)/n
  left=mp.mpf(0);right=max(mp.mpf(1),2*h/target)
  for _ in range(450):
   z=(left+right)/2;g=mp.digamma(z+h)-mp.digamma(z)-target
   if g>0:left=z
   else:right=z
  z=(left+right)/2; a,b=(h,z) if fa else (z,h)
  ll=mp.fsum((a-1)*mp.log(v)+(b-1)*mp.log1p(-v)-mp.log(mp.beta(a,b))-mp.log(hi-lo) for v in y)
  theta=[a,b,lo,hi]
 else:
  a=held['shape1']; shift=held['min']; x=[v-shift for v in x]; anchor=x[0]
  logs=[mp.log(v/anchor) for v in x];unit=max(map(abs,logs));logs=[v/unit for v in logs]
  def scores(z,c):
   k=mp.exp(z); p=[1/(1+mp.exp(-(k*l-c))) for l in logs]
   return n-(a+1)*mp.fsum(p),n+mp.fsum(k*l*(1-(a+1)*v) for l,v in zip(logs,p))
  z,c0=mp.findroot(scores,(mp.log(5),mp.mpf(0)),tol=mp.mpf('1e-100'),maxsteps=100)
  b=mp.exp(z)/unit;s=anchor*mp.exp(c0/b)
  ll=mp.fsum(mp.log(a)+mp.log(b)-mp.log(v)-mp.log1p(mp.exp(-b*mp.log(v/s)))-a*mp.log1p(mp.exp(b*mp.log(v/s))) for v in x)
  theta=[a,b,s,shift]
 return dict(theta=[mp.nstr(t,100) for t in theta],logLik=mp.nstr(ll,100))
def main():
 result=[]
 checkpoint=HERE/'reference-checkpoint.json'
 cached={r['index']:r for r in json.loads(checkpoint.read_text())} if checkpoint.exists() else {}
 for index in TARGETS:
  if index in cached:
   result.append(cached[index]);continue
  print('reference',index,'350/700 digits',flush=True)
  c=CASES[index]; r=solve(c,350); s=solve(c,700)
  err=max(abs(mp.mpf(a)-mp.mpf(b))/abs(mp.mpf(b)) if mp.mpf(b) else abs(mp.mpf(a)) for a,b in zip(r['theta']+[r['logLik']],s['theta']+[s['logLik']]))
  assert err<mp.mpf('1e-90'),(index,err)
  result.append(dict(index=index,family=c['family'],data=c['data'],held=c['held'],**s,precision_digits=[350,700],precision_relative_error=str(err)))
  checkpoint.write_text(json.dumps(result,indent=2)+'\n')
  print(index,s,flush=True)
 (HERE/'reference.json').write_text(json.dumps(result,indent=2)+'\n')
if __name__=='__main__':main()
