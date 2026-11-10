#!/usr/bin/env python3
"""Independent 80-digit likelihood checks for the large-count count fits."""
import json,pathlib,mpmath as mp
mp.mp.dps=80
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text());out=[]
for line in (HERE/'after-final.tsv').read_text().splitlines():
 a=line.split('\t');index=int(a[0]);c=cases[index];family=c['family']
 if family not in ['tbinom','ztbinom','zib','zmbinom'] or max(c['data'])<1000000 or a[2] not in ['certified','uncertified']:continue
 theta={}
 for field in a[4:]:
  if '=' in field and field.split('=',1)[0] in ['size','prob','pi','p0','a','b']:
   k,v=field.split('=',1);theta[k]=mp.mpf(float(v))
 N,p=theta['size'],theta['prob']
 def logpmf(v):
  if p==0:return mp.mpf(0) if v==0 else mp.ninf
  if p==1:return mp.mpf(0) if v==N else mp.ninf
  return mp.loggamma(N+1)-mp.loggamma(v+1)-mp.loggamma(N-v+1)+v*mp.log(p)+(N-v)*mp.log1p(-p)
 ll=mp.mpf(0)
 if family=='tbinom':
  L=max(0,int(mp.floor(theta['a']))+1);U=int(min(mp.floor(theta['b']),N))
  if U-L<=128:z=mp.fsum(mp.exp(logpmf(v)) for v in range(L,U+1))
  elif N-U<=128 and L<=128:
   z=1-mp.fsum(mp.exp(logpmf(v)) for v in range(L))-mp.fsum(mp.exp(logpmf(v)) for v in range(U+1,int(N)+1))
  else:
   out.append(dict(index=index,family=family,checked=False,reason='wide window requires an independent incomplete-beta oracle'));continue
  ll=mp.fsum(logpmf(v)-mp.log(z) for v in c['data'])
 elif family=='ztbinom':
  logz=mp.log(-mp.expm1(N*mp.log1p(-p))) if p>0 else mp.ninf
  ll=mp.fsum(logpmf(v)-logz for v in c['data'])
 elif family=='zib':
  pi=theta['pi'];ll=mp.fsum(mp.log(pi+(1-pi)*(1-p)**N) if v==0 else mp.log1p(-pi)+logpmf(v) for v in c['data'])
 else:
  p0=theta['p0'];logz=mp.log(-mp.expm1(N*mp.log1p(-p)))
  ll=mp.fsum(mp.log(p0) if v==0 else mp.log1p(-p0)+logpmf(v)-logz for v in c['data'])
 reported=mp.mpf(float(a[3]));error=abs(ll-reported)/(1+abs(ll))
 out.append(dict(index=index,family=family,checked=True,reference_logLik=str(ll),reported_logLik=a[3],relative_error=float(error),outcome=a[2],within_1e10=bool(error<mp.mpf('1e-10'))))
 if family=='tbinom':assert error<mp.mpf('1e-10'),out[-1]
(HERE/'point-checks.json').write_text(json.dumps(out,indent=2))
checked=[v for v in out if v['checked']]
print('80-digit points',len(checked),'skipped',len(out)-len(checked),'max relative error',max(v['relative_error'] for v in checked))
print('new tbinom maximum',max(v['relative_error'] for v in checked if v['family']=='tbinom'),'inherited helper deviations',[(v['index'],v['family'],v['outcome'],v['relative_error']) for v in checked if not v['within_1e10']])
