"""Independent 180-digit Bhatt roots from raw Gaussian CDF endpoint scores.
No production Hermite coefficients or production derivatives are imported.
Inputs are the exact retained binary64 observations. The weak width score is
scaled by eta**6 solely to condition the reference root finder.
"""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;m.mp.dps=180
inputs=json.loads((P/'bhatt-critical-width-inputs.json').read_text());results=[]
cache={r['id']:r for r in json.loads((P/'bhatt-critical-width-references.json').read_text())}
for row in inputs:
 x=list(map(m.mpf,row['data']));n=len(x);held=row['held'];symmetric=all(x[i]==-x[-i-1] for i in range(n));free_mu='mu' not in held and not symmetric
 mu=m.mpf(held['mu']) if 'mu' in held else m.fsum(x)/n;v=m.fsum((y-mu)**2 for y in x)/n
 h4=m.fsum(((y-mu)/m.sqrt(v))**4 for y in x)-3*n;h6=m.fsum(((y-mu)/m.sqrt(v))**6 for y in x)-15*(h4+3*n)+30*n
 eta=m.sqrt(m.mpf('10.5')*h4/h6)
 def scores(mu,S,E):
  S=m.exp(S);E=m.exp(E);s=S*m.sqrt(1-E*E/3);a=S*E;gm=m.mpf(0);gs=m.mpf(0);ga=m.mpf(0)
  for y in x:
   u=(y-mu+a)/s;l=(y-mu-a)/s;probability=(m.erf(u/m.sqrt(2))-m.erf(l/m.sqrt(2)))/2
   A=m.exp(-u*u/2)/m.sqrt(2*m.pi)/probability;B=m.exp(-l*l/2)/m.sqrt(2*m.pi)/probability
   gm+=(B-A)/s;gs+=l*B-u*A;ga+=a/s*(A+B)-1
  return gm,gs,ga
 def unpack(z):return z if free_mu else (mu,*z)
 def score(*z):
  u,S,E=unpack(z);gm,gs,ga=scores(u,S,E);e=m.exp(E);answer=(gs+ga,(ga-e*e/(3-e*e)*gs)/e**6)
  return (gm,*answer) if free_mu else answer
 guess=(mu,m.log(v)/2,m.log(eta)) if free_mu else (m.log(v)/2,m.log(eta))
 z=m.findroot(score,guess,tol=m.mpf('1e-80'),maxsteps=30);u,S,E=unpack(tuple(z))
 def original_score(*w):
  location,scale,width=unpack(w);gm,gs,ga=scores(location,scale,width);e=m.exp(width);answer=(gs+ga,ga-e*e/(3-e*e)*gs)
  return (gm,*answer) if free_mu else answer
 h=m.matrix([[m.diff(lambda t:original_score(*[t if k==j else z[k] for k in range(len(z))])[i],z[j]) for j in range(len(z))] for i in range(len(z))])
 for k in range(1,len(z)+1):assert m.det((-h)[:k,:k])>0
 r=dict(cache.get(row['id'],{}),**row,digits=180,reference=dict(mu=m.nstr(u,140),sigma=m.nstr(m.exp(S)*m.sqrt(1-m.exp(2*E)/3),140),a=m.nstr(m.exp(S+E),140)),information_determinant=m.nstr(m.det(-h),50),scaled_score_residual=m.nstr(max(map(abs,score(*z))),30))
 results.append(r);print(row['id'],r['reference'],r['scaled_score_residual'],flush=True)
(P/'bhatt-critical-width-references.json').write_text(json.dumps(results,indent=2)+'\n')
