"""Independent 80-digit scores in the public log coefficients at probe points."""
from pathlib import Path
import json,mpmath as m
m.mp.dps=80
D=Path(__file__).resolve().parent
cases={r['case']['index']:r['case'] for r in json.loads((D/'audit-cases.json').read_text())}
rows=[]
for r in map(json.loads,(D/'conditioning-native.jsonl').read_text().splitlines()):
 c=cases[r['index']];names=['shape1','shape2','shape3','scale'];free=[j for j,k in enumerate(names) if k not in c['held']]
 point=list(map(m.mpf,r['point']));data=list(map(m.mpf,c['data']))
 def objective(*z):
  v=[m.mpf(c['held'][k]) if k in c['held'] else m.exp(z[free.index(j)]) for j,k in enumerate(names)]
  q,a,p,b=v
  f=len(data)*(m.loggamma(p)+m.loggamma(q)-m.loggamma(p+q)-m.log(a))
  for x in data:
   t=a*(m.log(x)-m.log(b));f+=p*m.log1p(m.exp(-t))+q*m.log1p(m.exp(t))
  return f
 def component(i,t):
  z=point[:];z[i]=t;return objective(*z)
 gradient=m.matrix([m.diff(lambda t:component(i,t),point[i]) for i in range(3)])
 H=m.matrix(3)
 for i in range(3):
  for j in range(3):
   if i==j:H[i,j]=m.diff(lambda t:component(i,t),point[i],2)
   else:
    def pair(a,b):
     z=point[:];z[i]=a;z[j]=b;return objective(*z)
    H[i,j]=m.diff(pair,(point[i],point[j]),(1,1))
 noise=m.matrix(list(map(m.mpf,r['gradient'])))-gradient
 displacement=m.lu_solve(H,noise)
 eigen=m.eigsy(H,eigvals_only=True)
 rows.append(dict(index=c['index'],free=[names[j] for j in free],gradient=list(map(str,gradient)),score_roundoff=list(map(str,noise)),equivalent_log_coefficient_error=list(map(str,displacement)),maximum_equivalent_error=float(max(map(abs,displacement))),information_eigenvalues=list(map(str,eigen)),information_condition=float(eigen[2]/eigen[0])))
(D/'conditioning-analysis.json').write_text(json.dumps(rows,indent=2)+'\n')
for r in rows:print(r['index'],r['maximum_equivalent_error'],r['information_condition'])
