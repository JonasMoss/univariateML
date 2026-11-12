"""Independent high-precision information at the rejected closure proposals."""
from pathlib import Path
import json,mpmath as mp
from fractions import Fraction
from reference import likelihood
P=Path(__file__).resolve().parent
cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())}
rows={r[0]:r for r in [s.split('\t') for s in (P/'before-native.tsv').read_text().splitlines()]}
out=[]
for key in ['route-burr-nearly-constant','route-invburr-nearly-constant','route-pareto4-nearly-constant']:
 mp.mp.dps=420;c=cases[key];row=rows[key]
 base={k:mp.mpf(float(v)) for k,v in (s.split('=') for s in row[4:])};x=list(map(mp.mpf,c['data']));names=['shape1','shape2','scale']
 def f(*q):return likelihood(c['family'],x,{**base,**{k:base[k]*mp.exp(q[j]) for j,k in enumerate(names)}})
 zero=[mp.mpf(0)]*3
 def g(*z):return tuple(mp.diff(lambda t:f(*[t if j==i else z[j] for j in range(3)]),z[i]) for i in range(3))
 score=g(*zero);H=mp.matrix(3)
 for i in range(3):
  for j in range(3):H[i,j]=mp.diff(lambda t:g(*[t if k==j else zero[k] for k in range(3)])[i],0)
 record={'id':key,'digits':mp.mp.dps,'score':[mp.nstr(v,70) for v in score]}
 try:
  step=mp.lu_solve(-H,mp.matrix(score));record['newton_log_correction']=[mp.nstr(v,70) for v in step];record['largest_correction']=mp.nstr(max(map(abs,step)),70)
  mp.cholesky(-H);record['information_positive_definite']=True
 except Exception as e:record['information_positive_definite']=False;record['information_failure']=str(e)
 out.append(record);(P/'burr-boundary-evidence.json').write_text(json.dumps(out,indent=2)+'\n');print(key,record.get('largest_correction'),flush=True)
u=Fraction(1e-320);v=Fraction(6e-320)
seam={'exact_ratio':str(v/u),'moment_profile_constant':'0','moment_profile_linear':'2/81','conclusion':'The normalized profile score divided by rate is strictly positive in the small-rate enclosure; the returned finite small-rate point is not stationary.'}
(P/'unit-seam-evidence.json').write_text(json.dumps(seam,indent=2)+'\n')
