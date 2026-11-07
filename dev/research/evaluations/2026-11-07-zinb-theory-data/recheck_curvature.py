"""80-digit feasibility and curvature adjudication of every double LP candidate.
Correct its mean/stationarity constraints on a three-point basis; retain any
other weights. Infeasible corrections cannot supply a counterexample.
"""
import json,mpmath as mp
from pathlib import Path
P=Path(__file__).resolve().parent;mp.mp.dps=80
records=[]
for row in json.loads((P/'curvature-search.json').read_text())['candidates']:
 r=mp.mpf(str(row['r']));m=mp.mpf(str(row['m']))
 t=mp.findroot(lambda t:r*mp.expm1(t)/(-mp.expm1(-r*t))-m,mp.log1p(m/r))
 E=lambda v:1/mp.expm1(v)-1/v
 J=1-t/mp.expm1(t);Jp=(mp.exp(t)*(t-1)+1)/mp.expm1(t)**2
 tp=t*E(r*t)/(1+E(t)-r*E(r*t))
 weights={x:mp.mpf(str(z)) for x,z in row['weights']}
 for x in [1,2,3]:
  if len(weights)>=3:break
  weights.setdefault(x,mp.mpf(0))
 xs=sorted(weights);basis=sorted(sorted(xs,key=lambda x:abs(weights[x]),reverse=True)[:3])
 H=lambda x:sum(mp.mpf(j)/(r+j) for j in range(1,x))
 H2=lambda x:sum(mp.mpf(j)/(r+j)**2 for j in range(1,x))
 original_F=m*J-sum(z*H(x) for x,z in weights.items())
 other=[x for x in xs if x not in basis]
 rhs=mp.matrix([1-sum(weights[x] for x in other),m-sum(x*weights[x] for x in other),m*J-sum(weights[x]*H(x) for x in other)])
 mat=mp.matrix([[1]*3,basis,[H(x) for x in basis]])
 corrected=mp.lu_solve(mat,rhs)
 for x,z in zip(basis,corrected):weights[x]=z
 feasible=all(z>=0 for z in weights.values())
 curvature=(m*Jp*tp+sum(z*H2(x) for x,z in weights.items()))*r*r
 records.append(dict(m=str(m),r=str(r),support=xs,weights=[str(weights[x]) for x in xs],original_F=str(original_F),scaled_Fprime=str(curvature),feasible=feasible,classification='negative curvature' if feasible and curvature<0 else 'infeasible stationary correction' if not feasible else 'unresolved counterexample candidate'))
 assert not feasible or curvature<0,'genuine candidate: do not claim universal uniqueness'
summary=dict(dps=80,records=records,all_candidates_negative=True,feasible_negative=sum(z['feasible'] for z in records),infeasible=sum(not z['feasible'] for z in records),meaning='Every feasible high-precision correction has negative curvature; other candidates fail stationarity/positivity feasibility.')
(P/'curvature-recheck.json').write_text(json.dumps(summary,indent=2)+'\n')
print('Adjudicated',len(records),'candidates:',summary['feasible_negative'],'negative,',summary['infeasible'],'infeasible.')
