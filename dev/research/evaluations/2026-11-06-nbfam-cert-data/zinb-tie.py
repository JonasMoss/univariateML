"""Held size=10, 99 zeros and one count=10: probability-peak crossover.
No fitter changes. 60-digit numerical comparisons, with analytic root count.
"""
import json,sys
from pathlib import Path
import mpmath as m
m.mp.dps=60
r=m.mpf(10);n=100;k=1;s=10
p0=k*r/(k*r+s);p1=n*r/(n*r+s)
B=r*(p0+p1)-(p1-p0)
disc=B*B-4*r*r*p0*p1
turns=[(B-m.sqrt(disc))/(2*r),(B+m.sqrt(disc))/(2*r)]
R=lambda p:p**r*(n*r+s)*(p1-p)/((k*r+s)*(p-p0))
E=lambda p,pi:pi/(1-pi)*(k*r-(k*r+s)*p)+p**r*(n*r-(n*r+s)*p)
def ll(p,pi):
 return 99*m.log(pi+(1-pi)*p**r)+m.log1p(-pi)+m.loggamma(r+10)-m.loggamma(r)-m.loggamma(11)+r*m.log(p)+10*m.log1p(-p)
a,b,pi=m.findroot(lambda a,b,pi:(E(a,pi),E(b,pi),ll(a,pi)-ll(b,pi)),(m.mpf('.51'),m.mpf('.92'),m.mpf('.78')),tol=m.mpf('1e-50'))
assert p0<a<turns[0]<turns[1]<b<p1
assert max(abs(E(a,pi)),abs(E(b,pi)),abs(ll(a,pi)-ll(b,pi)))<m.mpf('1e-45')
ends=[]
for pp in [pi-m.mpf('1e-8'),pi+m.mpf('1e-8')]:
 aa=pp/(1-pp)
 assert R(turns[0])<aa<R(turns[1])
 roots=[]
 for left,right in zip([p0]+turns,turns+[p1]):
  lo,hi=left,right
  for _ in range(190):
   mid=(lo+hi)/2
   if E(lo,pp)*E(mid,pp)>0:lo=mid
   else:hi=mid
  roots.append((lo+hi)/2)
 assert m.diff(lambda p:E(p,pp),roots[0])<0 and m.diff(lambda p:E(p,pp),roots[2])<0
 ends.append(dict(pi=str(pp),roots=list(map(str,roots)),loglik=list(map(lambda p:str(ll(p,pp)),roots)),low_minus_high=str(ll(roots[0],pp)-ll(roots[2],pp))))
assert m.mpf(ends[0]['low_minus_high'])<0<m.mpf(ends[1]['low_minus_high'])
result=dict(precision=m.mp.dps,size='10',zeros=99,positive=[10],turns=list(map(str,turns)),
            R_at_turns=list(map(lambda p:str(R(p)),turns)),pi=str(pi),low_prob=str(a),high_prob=str(b),
            loglik=str(ll(a,pi)),bracket=ends)
Path(sys.argv[1]).write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
