#!/usr/bin/env python3
"""Independent raw-model verification of retained uncertified restorations."""
import csv,json,pathlib,subprocess
import mpmath as mp
from reference import raw_ll
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
p=subprocess.run([str(ROOT/'build/scratch/restoration-check')],text=True,capture_output=True,check=True)
(HERE/'restoration-native.csv').write_text('family,sample,status,certified,power,k,scale,logLik\n'+p.stdout)
rows=list(csv.DictReader((HERE/'restoration-native.csv').read_text().splitlines()))
e=2**-52;data={'symmetric':[1+j*e for j in range(5)],'asymmetric':[1+j*e for j in [0,1,2,4,7]],'tiny-power':[1,4],'wide-tiny-power':[1e-300,1e300,1e300,1e300], 'normal-small':[1e-150*(1+j*1e-12) for j in [-2,-1,0,1,3]], 'normal-large':[1e150*(1+j*1e-12) for j in [-2,-1,0,1,3]], 'log-asymmetric':[2*(1+j*e) for j in [0,1,2,4,7]], 'normal-held':[2,2,2]}
data.update({'square-reference':[1.5e154,1,1,1,1], 'reciprocal-reference':[1e-309,1,1,1,1], 'broad-normalization':[1e150]+[1]*100000})
def reference(row,dps):
 mp.mp.dps=dps;x=list(map(mp.mpf,data[row['sample']]));n=len(x);family=row['family']
 kind=family if family in ('lnorm','invgamma','naka','lgamma') else 'invweibull' if family in ('invweibull','frechet') else 'llogis' if family in ('llogis','pareto3') else 'invburr'
 k=mp.mpf(float(row['k']));a=mp.mpf(float(row['power']));scale=mp.mpf(float(row['scale']));sign=-1 if kind=='invburr' else 1
 if kind=='lnorm':
  logs=list(map(mp.log,x));ahat=a if row["sample"]=="normal-held" else mp.fsum(logs)/n;scalehat=mp.sqrt(mp.fsum((v-ahat)**2 for v in logs)/n)
 elif kind in ('invgamma','naka','lgamma'):
  y=[1/v if kind=='invgamma' else v*v if kind=='naka' else mp.log(v) for v in x];mean=mp.fsum(y)/n;lm=mp.fsum(map(mp.log,y))/n
  root=mp.findroot(lambda t:t-mp.digamma(mp.exp(t))-mp.log(mean)+lm,mp.log(a),tol=mp.mpf(10)**(-dps+40))
  ahat=mp.exp(root);scalehat=mean if kind=='naka' else ahat/mean
 elif 'tiny-power' in row['sample']:
  scalehat=mp.exp(-mp.log(mp.fsum(mp.exp(-a*mp.log(v)) for v in x)/n)/a);ahat=a
 else:
  y=[sign*mp.log(v/x[0]) for v in x];unit=max(map(abs,y));y=[v/unit for v in y]
  def scores(t,c):
   power=mp.exp(t);z=[power*v-c for v in y]
   if kind=='invweibull':
    w=[mp.exp(-v) for v in z];return n-mp.fsum(w),n+mp.fsum(power*v*(w0-1) for v,w0 in zip(y,w))
   w=[1/(1+mp.exp(-v)) for v in z]
   return -n+(k+1)*mp.fsum(w),n+mp.fsum(power*v*(1-(k+1)*w0) for v,w0 in zip(y,w))
  t,c=mp.findroot(scores,(mp.log(a*unit),a*sign*mp.log(scale/x[0])),tol=mp.mpf(10)**(-dps+40));ahat=mp.exp(t)/unit;scalehat=x[0]*mp.exp(sign*c/ahat)
 parms=[k,a,scale] if kind=='invburr' else [a,scale];ideal=[k,ahat,scalehat] if kind=='invburr' else [ahat,scalehat]
 return dict(power=mp.nstr(ahat,100),scale=mp.nstr(scalehat,100),raw_logLik=mp.nstr(raw_ll(kind,x,parms),100),maximum_logLik=mp.nstr(raw_ll(kind,x,ideal),100))
out=[]
for row in rows:
 assert row['status']=='0',row
 lo,hi=reference(row,180),reference(row,360)
 assert all(abs(mp.mpf(lo[k])-mp.mpf(hi[k]))/max(1,abs(mp.mpf(hi[k])))<mp.mpf('1e-90') for k in hi)
 errors={k:float(abs(mp.mpf(float(row[k]))/mp.mpf(hi[k])-1)) for k in ['power','scale']}
 error=float(abs(mp.mpf(float(row['logLik']))-mp.mpf(hi['raw_logLik']))/max(1,abs(mp.mpf(hi['raw_logLik']))))
 assert max(list(errors.values())+[error])<1e-10,(row,errors,error)
 out.append(dict(**row,reference=hi,coefficient_relative_errors=errors,raw_likelihood_error=error,precision_check='180/360 digits agree within 1e-90'))
(HERE/'restoration-reference.json').write_text(json.dumps(out,indent=2)+'\n')
print(len(out),'restoration cases match their raw likelihoods and continuous-root coefficients;',sum(r['certified']=='0' for r in out),'explicitly uncertified fits retained.')
