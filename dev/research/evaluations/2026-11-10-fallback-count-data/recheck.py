#!/usr/bin/env python3
"""Numerical 80-digit investigation; not a global certificate."""
import json,pathlib
import mpmath as mp
mp.mp.dps=80
out=[]
# The critical held-ZIP sample has a mean-matched face at lambda=1.6.
# Recheck both adjacent binary64 inflation constants exactly as supplied.
x=[0]*5+[1,1,2,2,4];n=len(x);S=sum(x);k=sum(v>0 for v in x)
for value in [.3772146069463776,.37721460694637765]:
 pi=mp.mpf(value)
 def ll(rate):return 5*mp.log(pi+(1-pi)*mp.exp(-rate))+5*mp.log1p(-pi)+S*mp.log(rate)-k*rate-sum(mp.loggamma(v+1) for v in x if v)
 rate=mp.findroot(lambda u:mp.diff(ll,u),(mp.mpf('1.5'),mp.mpf('1.7')))
 w=(1-pi)*mp.exp(-rate)/(pi+(1-pi)*mp.exp(-rate))
 A=-sum(v*(v-1) for v in x)/2+rate*S-(k+5*w)*rate**2/2
 out.append(dict(family='zib',pi_binary64=value,rate=str(rate),first_order=str(A),conclusion='local coefficient only; no uniform tail/prefix certificate'))
# Zero first-order gap on a finite window, with mixed higher moments.
for count in [[50,41,33,3,3],[104,0,0,608,112],[1,2,2]]:
 U=len(count)-1;n=sum(count);mu=mp.fsum(i*c for i,c in enumerate(count))/n
 def weights(eta,N=None):
  logw=[]
  for i in range(U+1):
   v=i*eta-mp.loggamma(i+1)
   if N is not None:
    for j in range(i):v+=mp.log1p(-mp.mpf(j)/N)
   logw.append(v)
  top=max(logw);w=[mp.exp(v-top) for v in logw];z=mp.fsum(w);return [v/z for v in w]
 def profile(N=None):
  eta=mp.findroot(lambda e:mp.fsum(i*q for i,q in enumerate(weights(e,N)))-mu,mp.log(mu))
  q=weights(eta,N);return mp.fsum(c*mp.log(q[i]) for i,c in enumerate(count))
 face=profile();emp2=mp.fsum(i*(i-1)*c for i,c in enumerate(count))/n
 rate=mp.findroot(lambda e:mp.fsum(i*q for i,q in enumerate(weights(e)))-mu,mp.log(mu))
 gap=emp2-mp.fsum(i*(i-1)*q for i,q in enumerate(weights(rate)))
 diffs={str(N):str(profile(N)-face) for N in [max(U,5),10,100,1000]}
 out.append(dict(family='tbinom',counts=count,first_gap=str(gap),finite_minus_face=diffs,conclusion='tested integer comparisons only; mixed moments require the existing higher-order/global certificate'))
for counts in [[1,1,1,1,1,12],[1,2,2,3,3,3,4,4,5,5]]:
 mean=mp.mpf(sum(counts))/len(counts)
 rate=mp.findroot(lambda u:u/(-mp.expm1(-u))-mean,mean)
 gap=mp.fsum(v*(v-1) for v in counts)/len(counts)-rate*mean
 for family in ['ztbinom','zmbinom']:
  out.append(dict(family=family,positive_counts=counts,mean=str(mean),rate=str(rate),gap=str(gap),conclusion='strict local moment sign; tail and finite-prefix coverage still required'))
p=pathlib.Path(__file__).with_name('rechecks.json');p.write_text(json.dumps(out,indent=2));print(p)
