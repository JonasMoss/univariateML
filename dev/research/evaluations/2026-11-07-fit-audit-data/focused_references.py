#!/usr/bin/env python3
"""Unique global held-shape beta reference for the NSBeta cancellation witness."""
import json,pathlib
import mpmath as mp
HERE=pathlib.Path(__file__).resolve().parent
case=json.loads((HERE/'cases.json').read_text())[5348]
row=next(l.split(',') for l in (HERE/'fits.csv').read_text().splitlines() if l.split(',')[2]=='5348')
mp.mp.dps=350
x=list(map(mp.mpf,case['data']));a=mp.mpf(case['held']['shape1']);n=len(x)
assert case['held']['min']==0 and case['held']['max']==1
s=mp.fsum(mp.log1p(-v) for v in x)/n
score=lambda z:mp.digamma(a+mp.exp(z))-mp.digamma(mp.exp(z))+s
center=mp.log(a/(-s));lo,hi=center-4,center+4
assert score(lo)>0 and score(hi)<0
for _ in range(320):
 mid=(lo+hi)/2
 if score(mid)>0:lo=mid
 else:hi=mid
b=mp.exp((lo+hi)/2)
def ll(beta):return (a-1)*mp.fsum(map(mp.log,x))+(beta-1)*n*s+n*(mp.loggamma(a+beta)-mp.loggamma(a)-mp.loggamma(beta))
returned=mp.mpf(float(row[10].split('=')[1]))
result=dict(case=5348,parameters=['shape1','shape2','min','max'],returned_shape2=str(returned),reference_shape2=str(b),returned_reported_logLik=row[8],reference_logLik_at_returned=str(ll(returned)),reference_logLik_at_MLE=str(ll(b)),log_coordinate_score=str(n*b*score(mp.log(b))),relative_root_bracket_width=str(mp.expm1(hi-lo)),precision_digits=350,global_justification='At fixed positive shape1, the beta likelihood is strictly concave in shape2; its score decreases from positive infinity to the negative data statistic. The computed bracket contains its unique zero. Bracket signs are numerical, not outward-rounded interval bounds.')
with mp.workdps(700):
 assert score(lo)>0 and score(hi)<0
 result['precision_check']='bracket signs retained at 700 digits'
(HERE/'focused-reference.json').write_text(json.dumps(result,indent=2));print(json.dumps(result,indent=2))
