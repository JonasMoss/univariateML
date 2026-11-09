"""Resolve the NB normal coefficient for the remaining 0:5 cover example."""
import json
from pathlib import Path
import mpmath as m
m.mp.dps=90
x=list(range(6));n=len(x);mean=m.fsum(x)/n
v=m.fsum((z-mean)**2 for z in x)/n
score=lambda r:m.fsum(m.digamma(r+z)-m.digamma(r) for z in x)+n*m.log(r/(r+mean))
r=m.findroot(score,(8,12));p=r/(r+mean)
c=(n*p*p*(v-mean)/(1-p)-n*r*(1-p))/2
assert c<0
out=dict(sample=x,nb_size=str(r),nb_prob=str(p),c1=str(c),dispersion=str(v-mean),
         obligation='Default/6000 allowances are insufficient; the maximum allowance closes the cover (large-budget.json).')
Path(__file__).with_suffix('.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
